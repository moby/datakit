open Lwt.Infix

let src = Logs.Src.create "irmin-io" ~doc:"Datakit sync support"
module Log = (val Logs.src_log src : Logs.LOG)

module IO = struct
  type ctx = unit
  let ctx () = Lwt.return (Some ())
  type ic = Lwt_io.input_channel
  type oc = Lwt_io.output_channel
  let write oc s = Lwt_io.write oc s
  let flush oc = Lwt_io.flush oc

  let with_ssh_process ?init uri fn =
    let host = match Uri.host uri with
      | None   -> "localhost"
      | Some x -> x
    in
    let user = match Uri.userinfo uri with
      | None   -> ""
      | Some u -> u ^ "@"
    in
    let cmd = match init with
      | None   -> [| "ssh"; user ^ host; |]
      | Some x -> [| "ssh"; user ^ host; x |]
    in
    Log.info (fun f -> f "Executing '%s'" (String.concat " " (Array.to_list cmd)));
    let env = Unix.environment () in
    let p = Lwt_process.open_process_full ~env ("ssh", cmd) in
    Lwt.finalize
      (fun () -> fn (p#stdout, p#stdin))
      (fun () -> let _ = p#close in Lwt.return_unit)

  let with_conduit ?init uri fn =
    Log.debug (fun f -> f "Connecting to %s" (Uri.to_string uri));
    let resolver = Resolver_lwt_unix.system in
    Resolver_lwt.resolve_uri ~uri resolver >>= fun endp ->
    let ctx = Conduit_lwt_unix.default_ctx in
    Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
    Conduit_lwt_unix.connect ~ctx client >>= fun (_flow, ic, oc) ->
    Lwt.finalize
      (fun () ->
         begin match init with
           | None   -> Lwt.return_unit
           | Some s -> write oc s
         end >>= fun () ->
         fn (ic, oc))
      (fun ()  ->
         Lwt.catch
           (fun () -> Lwt_io.close ic)
           (function
             | Unix.Unix_error _ -> Lwt.return_unit
             | e -> Lwt.fail e))

  let with_connection ?ctx:_ uri ?init fn =
    match Git.Sync.protocol uri with
    | `Ok `SSH -> with_ssh_process ?init uri fn
    | `Ok `Git -> with_conduit ?init uri fn
    | `Ok `Smart_HTTP ->
      (* HTTP.with_http ?init (with_conduit ?init:None) uri fn *)
      Lwt.fail (Failure ("smart HTTP is not supported. Use git:// or SSH"))
    | `Not_supported x ->
      Lwt.fail (Failure ("Scheme " ^ x ^ " not supported yet"))
    | `Unknown ->
      Lwt.fail (Failure ("Unknown protocol. Must supply a scheme like git://"))

  let read_all ic =
    let len = 4 * 4096 in
    let return l = Lwt.return (List.rev l) in
    let rec aux acc =
      let buf = Bytes.create len in
      Lwt_io.read_into ic buf 0 len >>= function
      | 0 -> return acc
      | i ->
        let buf = Bytes.sub buf 0 i |> Bytes.unsafe_to_string in
        if len = i then return (buf :: acc)
        else aux (buf :: acc)
    in
    aux []

  let read_exactly ic n =
    let res = Bytes.create n in
    Lwt_io.read_into_exactly ic res 0 n >>= fun () ->
    Lwt.return (Bytes.unsafe_to_string res)

end

module FS = struct

  (* From mirage/ocaml-git/lib/unix/git_unix.ml (v1.7.3) *)

  let mmap_threshold = 4096
  (* Files smaller than this are loaded using [read].

     Use of mmap is necessary to handle packfiles efficiently. Since these
     are stored in a weak map, we won't run out of open files if we keep
     accessing the same one.

     Using read is necessary to handle references, since these are mutable
     and can't be cached. Using mmap here leads to hitting the OS limit on
     the number of open files.

     This threshold must be larger than the size of a reference.
  *)

  let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)
  let openfile_pool = Lwt_pool.create 200 (fun () -> Lwt.return_unit)

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> Lwt.fail (Failure (Printexc.to_string e))
    | e -> Lwt.fail e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
    | e -> Lwt.fail e

  let protect f x = Lwt.catch (fun () -> f x) protect_unix_exn
  let safe f x = Lwt.catch (fun () -> f x) ignore_enoent

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir && Sys.is_directory dir then Lwt.return_unit
      else (
        let clear =
          if Sys.file_exists dir then (
            Log.debug (fun l ->
                l "%s already exists but is a file, removing." dir);
            safe Lwt_unix.unlink dir
          ) else
            Lwt.return_unit
        in
        clear >>= fun () ->
        aux (Filename.dirname dir) >>= fun () ->
        Log.debug (fun l -> l "mkdir %s" dir);
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

  let file_exists f = Lwt_unix.file_exists f

  module Lock = struct

    let is_stale max_age file =
      file_exists file >>= fun exists ->
      if exists then (
        Lwt.catch (fun () ->
            Lwt_unix.stat file >>= fun s ->
            let stale = Unix.gettimeofday () -. s.Unix.st_mtime > max_age in
            Lwt.return stale)
          (function
            | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return false
            | e -> Lwt.fail e)
      ) else
        Lwt.return false

    let unlock file =
      Lwt_unix.unlink file

    let lock ?(max_age = 10. *. 60. (* 10 minutes *)) ?(sleep = 0.001) file =
      let rec aux i =
        Log.debug (fun f -> f "lock %s %d" file i);
        is_stale max_age file >>= fun is_stale ->
        if is_stale then (
          Log.err (fun f -> f "%s is stale, removing it." file);
          unlock file >>= fun () ->
          aux 1
        ) else
          let create () =
            let pid = Unix.getpid () in
            mkdir (Filename.dirname file) >>= fun () ->
            Lwt_unix.openfile file [Unix.O_CREAT; Unix.O_RDWR; Unix.O_EXCL] 0o600
            >>= fun fd ->
            let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
            Lwt_io.write_int oc pid >>= fun () ->
            Lwt_unix.close fd
          in
          Lwt.catch create (function
              | Unix.Unix_error(Unix.EEXIST, _, _) ->
                let backoff = 1. +. Random.float (let i = float i in i *. i) in
                Lwt_unix.sleep (sleep *. backoff) >>= fun () ->
                aux (i+1)
              | e -> Lwt.fail e)
      in
      aux 1

    let with_lock file fn =
      match file with
      | None   -> fn ()
      | Some f -> lock f >>= fun () -> Lwt.finalize fn (fun () -> unlock f)

  end

  let mkdir = mkdir

  type path = string

  (* we use file locking *)
  type lock = path
  let lock_file x = x

  let file_exists = file_exists

  let list_files kind dir =
    if Sys.file_exists dir && Sys.is_directory dir then
      let d = Sys.readdir dir in
      let d = Array.to_list d in
      let d = List.map (Filename.concat dir) d in
      let d = List.filter kind d in
      let d = List.sort String.compare d in
      Lwt.return d
    else
      Lwt.return_nil

  let directories dir =
    list_files (fun f ->
        try Sys.is_directory f with Sys_error _ -> false
      ) dir

  let files dir =
    list_files (fun f ->
        try not (Sys.is_directory f) with Sys_error _ -> false
      ) dir

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then Lwt.fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return_unit in
    match Cstruct.len b with
    | 0   -> Lwt.return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let delays = Array.init 20 (fun i -> 0.1 *. (float i) ** 2.)

  let command fmt =
    Printf.ksprintf (fun str ->
        Log.debug (fun l -> l "[exec] %s" str);
        let i = Sys.command str in
        if i <> 0 then Log.debug (fun l -> l "[exec] error %d" i);
        Lwt.return_unit
      ) fmt

  let remove_dir dir =
    if Sys.os_type = "Win32" then
      command "cmd /d /v:off /c rd /s /q %S" dir
    else
      command "rm -rf %S" dir

  let remove_file ?lock file =
    Lock.with_lock lock (fun () ->
        Lwt.catch
          (fun () -> Lwt_unix.unlink file)
          (function
            (* On Windows, [EACCES] can also occur in an attempt to
               rename a file or directory or to remove an existing
               directory. *)
            | Unix.Unix_error (Unix.EACCES, _, _)
            | Unix.Unix_error (Unix.EISDIR, _, _) -> remove_dir file
            | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
            | e -> Lwt.fail e)
      )

  let rename =
    if Sys.os_type <> "Win32" then Lwt_unix.rename
    else
      fun tmp file ->
        let rec aux i =
          Lwt.catch
            (fun () -> Lwt_unix.rename tmp file)
            (function
              (* On Windows, [EACCES] can also occur in an attempt to
                 rename a file or directory or to remove an existing
                 directory. *)
              | Unix.Unix_error (Unix.EACCES, _, _) as e ->
                if i >= Array.length delays then Lwt.fail e
                else (
                  file_exists file >>= fun exists ->
                  if exists && Sys.is_directory file then (
                    remove_dir file >>= fun () -> aux (i+1)
                  ) else (
                    Log.debug (fun l ->
                        l "Got EACCES, retrying in %.1fs" delays.(i));
                    Lwt_unix.sleep delays.(i) >>= fun () -> aux (i+1)
                  ))
              | e -> Lwt.fail e)
        in
        aux 0

  let with_write_file ?temp_dir file fn =
    begin match temp_dir with
      | None   -> Lwt.return_unit
      | Some d -> mkdir d
    end >>= fun () ->
    let dir = Filename.dirname file in
    mkdir dir >>= fun () ->
    let tmp = Filename.temp_file ?temp_dir (Filename.basename file) "write" in
    Lwt_pool.use openfile_pool (fun () ->
        Log.debug (fun l -> l "Writing %s (%s)" file tmp);
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644)
        >>= fun fd ->
        Lwt.finalize (fun () -> protect fn fd) (fun () -> Lwt_unix.close fd)
        >>= fun () ->
        rename tmp file
      )

  let read_file_with_read file size =
    let chunk_size = max 4096 (min size 0x100000) in
    let buf = Cstruct.create size in
    let flags = [Unix.O_RDONLY] in
    let perm = 0o0 in
    Lwt_unix.openfile file flags perm >>= fun fd ->
    let rec aux off =
      let read_size = min chunk_size (size - off) in
      Lwt_bytes.read fd buf.Cstruct.buffer off read_size >>= fun read ->
      (* It should test for read = 0 in case size is larger than the
         real size of the file. This may happen for instance if the
         file was truncated while reading. *)
      let off = off + read in
      if off >= size then
        Lwt.return buf
      else
        aux off
    in
    Lwt.finalize (fun () -> aux 0)
      (fun () -> Lwt_unix.close fd)

  let read_file_with_mmap file =
    let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
    let ba = Lwt_bytes.map_file ~fd ~shared:false () in
    Unix.close fd;
    Lwt.return (Cstruct.of_bigarray ba)

  let read_file file =
    Lwt.catch (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            Log.debug (fun l -> l "Reading %s" file);
            Lwt_unix.stat file >>= fun stats ->
            let size = stats.Lwt_unix.st_size in
            (if size >= mmap_threshold then read_file_with_mmap file
             else read_file_with_read file size
            ) >|= fun buf ->
            Some buf
          )
      ) (function
        | Unix.Unix_error _ | Sys_error _ -> Lwt.return_none
        | e -> Lwt.fail e)

  let stat_info_unsafe path =
    let open Git.Index in
    let stats = Unix.stat path in
    let ctime = { lsb32 = Int32.of_float stats.Unix.st_ctime; nsec = 0l } in
    let mtime = { lsb32 = Int32.of_float stats.Unix.st_mtime; nsec = 0l } in
    let dev = Int32.of_int stats.Unix.st_dev in
    let inode = Int32.of_int stats.Unix.st_ino in
    let mode = match stats.Unix.st_kind, stats.Unix.st_perm with
      | Unix.S_REG, p -> if p land 0o100 = 0o100 then `Exec else `Normal
      | Unix.S_LNK, _ -> `Link
      | k, p ->
        let kind = match k with
          | Unix.S_REG -> "REG"
          | Unix.S_DIR -> "DIR"
          | Unix.S_CHR -> "CHR"
          | Unix.S_BLK -> "BLK"
          | Unix.S_LNK -> "LNK"
          | Unix.S_FIFO -> "FIFO"
          | Unix.S_SOCK -> "SOCK"
        in
        let perm = Printf.sprintf "%o" p in
        let error =
          Printf.sprintf "%s: not supported kind of file [%s, %s]."
            path kind perm
        in
        failwith error
    in
    let uid = Int32.of_int stats.Unix.st_uid in
    let gid = Int32.of_int stats.Unix.st_gid in
    let size = Int32.of_int stats.Unix.st_size in
    { ctime; mtime; dev; inode; uid; gid; mode; size }

  let stat_info path =
    Lwt.catch (fun () -> Lwt.return (Some (stat_info_unsafe path))) (function
        | Sys_error _ | Unix.Unix_error _ -> Lwt.return_none
        | e -> Lwt.fail e)

  let chmod ?lock f `Exec =
    Lock.with_lock lock (fun () -> Lwt_unix.chmod f 0o755)

  let write_file ?temp_dir ?lock file b =
    let write () =
      with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)
    in
    Lock.with_lock lock (fun () ->
        Lwt.catch write (function
            | Unix.Unix_error (Unix.EISDIR, _, _) -> remove_dir file >>= write
            | e -> Lwt.fail e
          )
      )

  let test_and_set_file ?temp_dir ~lock file ~test ~set =
    Lock.with_lock (Some lock) (fun () ->
        read_file file >>= fun v ->
        let equal = match test, v with
          | None  , None   -> true
          | Some x, Some y -> Cstruct.equal x y
          | _ -> false
        in
        if not equal then Lwt.return false
        else
          (match set with
           | None   -> remove_file file
           | Some v -> write_file ?temp_dir file v)
          >|= fun () ->
          true
      )

end

module Zlib = Git.Inflate.Make(Zlib)
