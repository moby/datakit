(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
include Common_worker

let debug fmt =
  section := "job-worker";
  debug fmt

let (/) = Filename.concat

module OSet = Set.Make(struct
    type t = Object.id
    let compare = Id.compare
  end)

module System = struct

  let debug fmt = Printf.printf fmt
  open Lwt.Infix

  (* FIXME: use Bos? *)

  (* from ocaml-git/lib/unix/git_unix.ml *)

  let openfile_pool = Lwt_pool.create 200 (fun () -> Lwt.return_unit)

  let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)

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
            debug "%s already exists but is a file, removing." dir;
            remove_file dir;
          ) else
            Lwt.return_unit
        in
        clear >>= fun () ->
        aux (Filename.dirname dir) >>= fun () ->
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

(*  let list_files kind dir =
    Lwt_pool.use openfile_pool (fun () ->
        if Sys.file_exists dir then (
          let s = Lwt_unix.files_of_directory dir in
          let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
          let s = Lwt_stream.map (Filename.concat dir) s in
          let s = Lwt_stream.filter kind s in
          Lwt_stream.to_list s >>= fun l ->
          Lwt.return l
        ) else
          Lwt.return_nil
      )

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
      Lwt_list.fold_left_s aux (fs @ accu) ds
    in
    aux [] dir
*)

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then Lwt.fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return_unit in
    match Cstruct.len b with
    | 0   -> Lwt.return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let with_write_file ?temp_dir file fn =
    begin match temp_dir with
      | None   -> Lwt.return_unit
      | Some d -> mkdir d
    end >>= fun () ->
    let dir = Filename.dirname file in
    mkdir dir >>= fun () ->
    let tmp = Filename.temp_file ?temp_dir (Filename.basename file) "write" in
    Lwt_pool.use openfile_pool (fun () ->
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644)
        >>= fun fd ->
        Lwt.finalize
          (fun () -> protect fn fd >>= fun () -> Lwt_unix.rename tmp file)
          (fun _  -> Lwt_unix.close fd)
      )

  let write_file file ?temp_dir b =
    with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)

  let read_file file =
    Unix.handle_unix_error (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            debug "Reading %s" file;
            let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
            let ba = Lwt_bytes.map_file ~fd ~shared:false () in
            Unix.close fd;
            Lwt.return (Cstruct.of_bigarray ba)
          ))
      ()

  (* end of ocaml-git *)


(*
  let install_archive (name, content) =
    let tmp = "/tmp/ciso-" / name in
    if Sys.file_exists tmp then Sys.remove tmp;
    write_file tmp content >>= fun () ->
    Lwt.return tmp

  let extract_archive tar =
    if not (Filename.check_suffix tar ".tar.gz") then
      raise (Invalid_argument tar);
    let cmd = Printf.sprintf "tar -xzf %s -C /" tar in
    let on_failure () = err "extract: cannot untar %s" tar in
    let on_success () = debug "extract: OK"; Lwt.return_unit in
    exec ~on_success ~on_failure cmd

  let install_files ~src ~dst files =
    let cp src dst =
      let comm = Printf.sprintf "cp %s %s" src dst in
      let on_failure () = err "cp: cannot copy %s to %s" src dst in
      let on_success () = debug "%s installed" src; Lwt.return_unit in
      exec ~on_success ~on_failure comm
    in
    Lwt_list.iter_s (fun (f, _) ->
        (* FIXME: verify integrity of the digest? *)
        let src_path = src / f in
        let dst_path = dst / f in
        mkdir (Filename.dirname (dst / f)) >>= fun () ->
        cp src_path dst_path
      ) files

  let name_of_archive name =
    assert (Filename.check_suffix name ".tar.gz");
    Filename.chop_extension (Filename.chop_extension name)

  let clean_tmp action name =
    let file = "/tmp" / name in
    let dir = "/tmp" / name_of_archive name in
    let comm = Printf.sprintf "rm -rf %s %s" file dir in
    let on_success () =
      debug "clean_tmp: %s done (%s %s)!" action file dir;
      Lwt.return_unit
    in
    let on_failure () = err "cannot remove %s and %s" file dir in
    exec ~on_failure ~on_success comm

*)
end

(*
let archive_of_id id =
  Filename.get_temp_dir_name () / Id.to_string id  ^ ".tar.gz"

let prefix_of_job t job = opam_root t / Switch.to_string (Job.switch job)

let snapshots t ?white_list job =
  let prefix = prefix_of_job t job in
  let rec loop checksums = function
    | [] -> checksums
    | path :: tl ->
      (* soft link to absent file *)
      if not (Sys.file_exists path) then loop checksums tl
      else if not (Sys.is_directory path) then
        loop ((path, Digest.file path) :: checksums) tl
      else
        let files =
          Sys.readdir path
          |> Array.to_list
          |> List.rev_map (fun f -> path / f)
        in
        loop checksums (List.rev_append files tl)
  in
  if not (Sys.file_exists prefix) then []
  else (
    let sub_dirs =
      Sys.readdir prefix
      |> Array.to_list
      |> (fun lst -> match white_list with
          | Some wl -> List.filter (fun n -> List.mem n wl) lst
          | None    -> lst)
      |> List.rev_map (fun n -> prefix / n)
    in
    loop [] sub_dirs
  )

let opam_snapshot t job =
  let s = Job.switch job in
  Opam.read_installed (opam t s)

let collect_installed t job ~before ~after =
  let module CsMap = Map.Make(String) in
  let cmap =
    List.fold_left
      (fun acc (f, checksum) -> CsMap.add f checksum acc)
      CsMap.empty before
  in
  (* TODO: collect deleted files *)
  let installed =
    List.fold_left (fun acc (f, checksum) ->
        if not (CsMap.mem f cmap) then (f, checksum) :: acc else
          let cs = CsMap.find f cmap in
          if cs <> checksum then (f, checksum) :: acc else acc
      ) [] after
  in
  (* 1 is for the delimiter *)
  let prefix = prefix_of_job t job in
  let files = List.rev_map (fun (f, d) -> chop_prefix ~prefix f, d) installed in
  Lwt.return files

(* FIXME: console outputs should not be in the archive *)
let create_archive t job files ~old_pkgs ~new_pkgs =
  let path = archive_of_id (Job.id job) in
  let dst  = Filename.dirname path in
  let src  = prefix_of_job t job in
  System.install_files ~src ~dst files >>= fun () ->
  let installed = List.filter (fun p -> not (List.mem p old_pkgs)) new_pkgs in
  Opam.write_installed (opam t @@ Job.switch job) installed;
  let cmd = Printf.sprintf "tar -zcf %s %s" path dst in
  System.exec cmd >>= fun () ->
  System.read_file path >|= fun content ->
  Object.archive files content

let extract_object t job obj =
  match Object.contents obj with
  | Object.File (name, raw) ->
    let path = prefix_of_job t job / name in
    System.write_file path raw
  | Object.Archive { Object.files; raw } ->
    let path = archive_of_id (Object.id obj) in
    System.install_archive (path, raw) >>= fun arch_path ->
    System.extract_archive arch_path >>= fun () ->
    let src = System.name_of_archive arch_path in
    let dst = prefix_of_job t job in
    System.install_files ~src ~dst files >>= fun () ->
    System.clean_tmp "extract_object" (Filename.basename arch_path)

(* FIXME: add caching *)
let find_job_deps t j =
  let rec aux todo deps =
    if JSet.is_empty todo then Lwt.return (JSet.elements deps)
    else
      let id = JSet.choose todo in
      let todo = JSet.remove id todo in
      Store.Job.get (store t) id >>= fun job ->
      let inputs = JSet.of_list (Job.inputs job) in
      let todo = JSet.(union todo (diff inputs deps)) in
      let deps = JSet.union inputs deps in
      aux todo deps
  in
  aux JSet.(singleton j) JSet.empty

let find_obj_deps t j =
  find_job_deps t j >>= fun jobs ->
  Lwt_list.fold_left_s (fun deps job ->
      if j = job then Lwt.return deps
      else
        Store.Job.outputs (store t) job >|= fun objs ->
        List.fold_left (fun s e -> OSet.add e s) deps objs
    ) OSet.empty jobs
  >|= fun objs ->
  OSet.elements objs

let prepare t job  =
  find_obj_deps t (Job.id job) >>= fun objs ->
  Opam.switch_to (opam t Switch.system) (Job.switch job);
  (* URGENT FIXME: installation order IS important *)
  Lwt_list.iter_p (fun oid ->
      Store.Object.get (store t) oid >>=
      extract_object t job
    ) objs

let default_white_list = ["lib"; "bin"; "sbin"; "doc"; "share"; "etc"; "man"]

let process_job ?(white_list=default_white_list) t job =
  let cache = cache t in
  let id = Job.id job in
  prepare t job >|= fun () ->
  debug "build: %s, pre-snapshot" (Id.to_string id);
  let before = snapshots t ~white_list job in
  let old_pkgs = opam_snapshot t job in
  debug "build: %s, install." (Id.to_string id);
  (* FIXME: handle the Package.info *)
  let pkgs = List.map fst (Job.packages job) in
  begin
    Lwt.catch
      (fun () ->
         Opam.install (opam t @@ Job.switch job) pkgs;
         Lwt.return `Success)
      (fun exn ->
         debug "Job %s exited with: %s" (Id.to_string id) (Printexc.to_string exn);
         Lwt.return `Failure)
  end >>= fun result ->
  let () = match result with
    | `Success -> debug "build: %s Success!" (Id.to_string id)
    | `Failure -> debug "build: %s Failure!" (Id.to_string id)
  in
  debug "build: %s, post-snapshot" (Id.to_string id);
  let after = snapshots t ~white_list job in
  let new_pkgs = opam_snapshot t job in
  collect_outputs t job >>= fun outputs ->
  collect_installed t job ~before ~after >>= fun installed ->
  create_archive t job installed ~old_pkgs ~new_pkgs >>= fun archive ->
  System.clean_tmp "pkg_build" (archive_of_id id) >>= fun () ->
  Opam.remove (opam t @@ Job.switch job) pkgs;
  Store.with_transaction (store t) "Job complete" (fun t ->
      let add_one obj =
        Store.Object.add t obj >>= fun () ->
        Store.Job.add_output t id (Object.id obj)
      in
      let objs = if cache then archive :: outputs else outputs in
      Lwt_list.iter_p add_one objs >>= fun () ->
      match result with
      | `Success -> Store.Job.success t id
      | `Failure -> Store.Job.success t id
    )
*)

(* FIXME: Use Bos *)
let opam t ?output fmt =
  let open Rresult in
  Fmt.kstrf (fun str ->
      let out = match output with
        | None   -> ""
        | Some o -> Printf.sprintf " -vv >>%s 2>&1" o
      in
      (* FIXME: the OPAMROOT is not necessary but it's good to be
         extra-carefull in that case. *)
      let cmd = Printf.sprintf "OPAMROOT=%s opam %s%s" (opam_root t) str out in
      let err = Sys.command cmd in
      if err = 0 then Ok () else Error Fmt.(strf "%s: exit %d" cmd err)
    ) fmt

let add_output t job output =
  let id = Job.id job in
  System.read_file output >>= fun output ->
  let obj = Object.file "output" output in
  Store.Job.add_output (store t) id obj

let default_callback t job =
  let id = Job.id job in
  let pkgs = Job.packages job in
  let switch = Job.switch job in
  let pkgs_s =
    List.map (fun m -> Package.to_string @@ Package.pkg m) pkgs
    |> String.concat " "
  in
  let o = Opam.create ~root:(opam_root t) (Some switch) in
  Opam.switch_install o;
  Opam.repo_clean o;
  Opam.pin_clean o;
  let repo_root = opam_root t / "ciso" / "jobs" / Id.to_string id in
  let output = repo_root / "output" in
  Lwt_list.iter_s (fun m ->
      let p = Package.pkg m in
      let dir = repo_root / "packages" / Package.to_string p in
      let write (k, v) = match v with
        | None   -> Lwt.return_unit
        | Some v -> System.write_file (dir / k) v
      in
      let files =
        List.map (fun (f, c) -> "files" / f, Some c) (Package.files m)
      in
      Lwt_list.iter_s write ([
          "opam" , Some (Package.opam m);
          "descr", Package.descr m;
          "url"  , Package.url m;
        ] @ files)
    ) pkgs
  >>= fun () ->
  let result =
    let open Rresult in
    opam t "repo add ciso %s" repo_root >>= fun () ->
    opam t "update" >>= fun () ->
    opam t "install %s" ~output pkgs_s >>= fun () ->
    opam t "remove %s" ~output pkgs_s
  in
  add_output t job output >|= fun () ->
  let _x = opam t "remove --force %s" pkgs_s in
  if Rresult.R.is_ok result then `Success else `Failure


type result = [`Success | `Failure]
type callback = t -> Job.t -> result Lwt.t

let worker = worker

let start ?(callback=default_callback) =
  let callback t = function
    | `Idle
    | `Task _ -> Lwt.return_unit
    | `Job id ->
      debug "Got a new job: %s" (Id.to_string id);
      let wid = Worker.id (worker t) in
      let store = store t in
      Store.Job.ack store id wid >>= fun () ->
      Store.Job.get store id >>= fun job ->
      callback t job >>= fun result ->
      begin match result with
        | `Success -> Store.Job.success store id
        | `Failure -> Store.Job.failure store id
      end >>= fun () ->
      Store.Worker.idle store wid
  in
  start ~kind:`Job callback
