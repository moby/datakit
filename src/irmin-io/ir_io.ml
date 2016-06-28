open Lwt.Infix

let src = Logs.Src.create "irmin-io" ~doc:"Datakit sync support"
module Log = (val Logs.src_log src : Logs.LOG)

module Sync = struct
  type ctx = unit
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

  let remove_file f = safe Lwt_unix.unlink f

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir && Sys.is_directory dir then Lwt.return_unit
      else (
        let clear =
          if Sys.file_exists dir then (
            Log.debug (fun f -> f "%s already exists but is a file, removing." dir);
            remove_file dir;
          ) else
            Lwt.return_unit
        in
        clear >>= fun () ->
        aux (Filename.dirname dir) >>= fun () ->
        Log.debug (fun f -> f "mkdir %s" dir);
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

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

  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then Lwt.fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return_unit in
    match Cstruct.len b with
    | 0   -> Lwt.return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let rename =
    if Sys.os_type <> "Win32" then Lwt_unix.rename
    else
      fun tmp file ->
        let delays = [| 0.; 1.; 10.; 20.; 40. |] in
        let rec aux i =
          Lwt.catch
            (fun () -> Lwt_unix.rename tmp file)
            (function
              | Unix.Unix_error (Unix.EACCES, _, _) as e ->
                if i >= Array.length delays then Lwt.fail e
                else Lwt_unix.sleep delays.(i) >>= fun () -> aux (i+1)
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
        Log.info (fun f -> f "Writing %s (%s)" file tmp);
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644)
        >>= fun fd ->
        Lwt.finalize (fun () -> protect fn fd) (fun () -> Lwt_unix.close fd)
        >>= fun () ->
        rename tmp file
      )

  let write_file file ?temp_dir b =
    with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)

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
    Unix.handle_unix_error (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            Log.info (fun l -> l "Reading %s" file);
            Lwt_unix.stat file >>= fun stats ->
            let size = stats.Lwt_unix.st_size in
            if size >= mmap_threshold then read_file_with_mmap file
            else read_file_with_read file size
          )
    ) ()

  let realdir dir =
    if Sys.file_exists dir && Sys.is_directory dir then (
      let d = Sys.getcwd () in
      Unix.chdir dir;
      let e = Sys.getcwd () in
      Sys.chdir d;
      e
    ) else dir

  (* FIXME: this is crazy *)
  let realpath file =
    let rec aux file =
      if Sys.file_exists file && Sys.is_directory file then
        realdir file
      else
        let dirname = Filename.dirname file in
        let basename = Filename.basename file in
        Filename.concat (aux dirname) basename
    in
    Lwt.return (aux file)

  let stat_info path =
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

  let file_exists f =
    Lwt.return (Sys.file_exists f)

  let rm_command =
    if Sys.os_type = "Win32" then
      "cmd /d /v:off /c rd /s /q"
    else
      "rm -rf"

  let remove f =
    if Sys.file_exists f && not (Sys.is_directory f) then remove_file f
    else if not (Sys.file_exists f) then Lwt.return_unit
    else
      (* FIXME: eeek *)
      let i = Sys.command (Printf.sprintf "%s %s" rm_command f) in
      if i = 0 then Lwt.return_unit else Lwt.fail (Failure ("Cannot remove " ^ f))

  let chmod f i =
    Lwt.return (Unix.chmod f i)

  let getcwd () =
    Lwt.return (Sys.getcwd ())

end

module Lock = struct

  (* From mirage/irmin/lib/unix/irmin_unix.ml (v0.10.1) *)

  let is_stale max_age file =
    FS.file_exists file >>= fun exists ->
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
    FS.remove file

  let lock ?(max_age = 2.) ?(sleep = 0.001) file =
    let rec aux i =
      Log.debug (fun f -> f "lock %d" i);
      is_stale max_age file >>= fun is_stale ->
      if is_stale then (
        Log.err (fun f -> f "%s is stale, removing it." file);
        unlock file >>= fun () ->
        aux 1
      ) else
        let create () =
          let pid = Unix.getpid () in
          FS.mkdir (Filename.dirname file) >>= fun () ->
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
    lock file >>= fun () ->
    Lwt.finalize fn (fun () -> unlock file)

end

module Zlib = Git.Inflate.Make(Zlib)

module Poll = struct

  (* from mirage/irmin/lib/unix/irmin_unix.ml (v0.10.1) *)
  module S = struct
    module X = struct
      include Set.Make(struct
          type t = string * string
          let compare = Tc.Compare.pair String.compare String.compare
        end)
      let of_list l = List.fold_left (fun set elt -> add elt set) empty l
      let to_list = elements
      module K = Tc.Pair(Tc.String)(Tc.String)
      let sdiff x y = union (diff x y) (diff y x)
    end
    include X
    include Tc.As_L0 (X)
  end

  module StringSet = struct
    include Set.Make(Tc.String)
    let of_list l = List.fold_left (fun acc e -> add e acc) empty l
  end

  let to_string set = Tc.show (module S) set

  let string_chop_prefix t ~prefix =
    let lt = String.length t in
    let lp = String.length prefix in
    if lt < lp then None else
      let p = String.sub t 0 lp in
      if String.compare p prefix <> 0 then None
      else Some (String.sub t lp (lt - lp))

  let string_chop_prefix_exn t ~prefix = match string_chop_prefix t ~prefix with
    | None   -> failwith "string_chop_prefix"
    | Some s -> s

  let (/) = Filename.concat

  let read_files dir =
    FS.rec_files dir >>= fun new_files ->
    let prefix = dir / "" in
    let new_files =
      List.map (fun f -> string_chop_prefix_exn f ~prefix, Digest.file f) new_files
    in
    Lwt.return (S.of_list new_files)

  (* run [t] and returns an handler to stop the task. *)
  let stoppable t =
    let s, u = Lwt.task () in
    Lwt.async (fun () -> Lwt.pick ([s; t ()]));
    function () -> Lwt.wakeup u ()

  (* active polling *)
  let rec poll ~callback ~delay dir files =
    read_files dir >>= fun new_files ->
    let diff = S.sdiff files new_files in
    begin if S.is_empty diff then (
        Log.debug (fun f -> f "polling %s: no changes!" dir);
        Lwt.return_unit
      ) else (
        Log.debug (fun f -> f "polling %s: diff:%s" dir (to_string diff));
        let files =
          S.to_list diff |> List.map fst |> StringSet.of_list |> StringSet.elements
        in
        Lwt_list.iter_p (callback dir) files)
    end >>= fun () ->
    Lwt_unix.sleep delay >>= fun () ->
    poll ~callback ~delay dir new_files

  let listen ~callback ~delay dir =
    read_files dir >|= fun files ->
    stoppable (fun () -> poll ~callback ~delay dir files)

  (* map directory names to list of callbacks *)
  let listeners = Hashtbl.create 10
  let watchdogs = Hashtbl.create 10

  let nb_listeners dir =
    try List.length (Hashtbl.find listeners dir) with Not_found -> 0

  let watchdog dir =
    try Some (Hashtbl.find watchdogs dir) with Not_found -> None

  (* call all the callbacks on the file *)
  let callback dir file =
    let fns = try Hashtbl.find listeners dir with Not_found -> [] in
    Lwt_list.iter_p (fun (id, f) -> Log.debug (fun f -> f "callback %d" id); f file) fns

  let realdir dir = if Filename.is_relative dir then Sys.getcwd () / dir else dir

  let start_watchdog ~delay dir =
    match watchdog dir with
    | Some _ -> assert (nb_listeners dir <> 0); Lwt.return_unit
    | None   ->
      (* Note: multiple threads can wait here *)
      listen dir ~delay ~callback >|= fun u ->
      match watchdog dir with
      | Some _ -> u ()
      | None   ->
        Log.debug (fun f -> f "Start watchdog for %s" dir);
        Hashtbl.add watchdogs dir u

  let stop_watchdog dir =
    match watchdog dir with
    | None      -> assert (nb_listeners dir = 0)
    | Some stop ->
      if nb_listeners dir = 0 then (
        Log.debug (fun f -> f "Stop watchdog for %s" dir);
        Hashtbl.remove watchdogs dir;
        stop ()
      )

  let add_listener id dir fn =
    let fns = try Hashtbl.find listeners dir with Not_found -> [] in
    let fns = (id, fn) :: fns in
    Hashtbl.replace listeners dir fns

  let remove_listener id dir =
    let fns = try Hashtbl.find listeners dir with Not_found -> [] in
    let fns = List.filter (fun (x,_) -> x <> id) fns in
    if fns = [] then Hashtbl.remove listeners dir
    else Hashtbl.replace listeners dir fns

  let uninstall_dir_polling_listener () =
    Hashtbl.iter (fun _dir stop -> stop ()) watchdogs;
    Hashtbl.clear watchdogs;
    Hashtbl.clear listeners

  let install_dir_polling_listener delay =
    uninstall_dir_polling_listener ();
    let listen_dir id dir fn =
      let dir = realdir dir in
      start_watchdog ~delay dir >|= fun () ->
      add_listener id dir fn;
      function () ->
        remove_listener id dir;
        stop_watchdog dir
    in
    Irmin.Private.Watch.set_listen_dir_hook listen_dir

end
