open! Astring
open Lwt.Infix
open CI_utils.Infix

module DK = CI_utils.DK

let branch_safe_char = function
  | ':' -> '-'
  | x -> x

module Image = struct
  type t = {
    id : string;
  }

  let v id = { id }

  let of_published = v

  let id t = t.id

  let pp f t = Fmt.string f t.id

  let pp_short f t = Fmt.string f (String.with_range ~len:10 t.id)
end

(* Check that [path] is a valid path in [base] and contains no symlink components,
   or "..". *)
let validate_path ~log ~base path =
  let rec aux base = function
    | [] -> Ok ()
    | "" :: _ -> Error "Empty path component"   (* Could be an absolute path *)
    | ".." :: _ -> Error "'..' in Dockerfile path!"
    | x :: xs ->
      let subpath = Filename.concat base x in
      match Unix.lstat subpath with
      | {Unix.st_kind = Unix.S_REG | Unix.S_DIR; _} -> aux subpath xs
      | _ -> Error (Fmt.strf "Not a regular file or directory: %S" x)
      | exception Unix.Unix_error(Unix.ENOENT, _, _) -> Error "Dockerfile does not exist"
      | exception ex -> Error (Fmt.strf "Bad Dockerfile: %a" CI_utils.pp_exn ex)
  in
  match aux base (String.cuts ~sep:"/" path) with
  | Ok () -> ()
  | Error msg ->
    CI_live_log.log log "%s [%S]" msg path;
    CI_utils.failf "%s" msg

module Builder = struct

  module Key = struct
    type t = {
      src : CI_git.commit;
      from : Image.t option;
    }
  end

  type t = {
    label : string;
    dockerfile : string;
    timeout : float;
    pool : CI_monitored_pool.t;
  }

  type context = CI_s.job_id

  type value = Image.t

  let name t =
    Fmt.strf "docker build -f %S" t.dockerfile

  let title _t _key = "Docker build"

  let label t {Key.from; src} =
    match from with
    | None -> Fmt.strf "Build %s in %a" t.label CI_git.Commit.pp_short src
    | Some base -> Fmt.strf "Build %s in %a from %a" t.label CI_git.Commit.pp_short src Image.pp_short base

  let load _t tree _key =
    DK.Tree.read_file tree CI_cache.Path.value >>*= fun data ->
    Lwt.return (Image.v (String.trim (Cstruct.to_string data)))

  let branch t {Key.src; from} =
    let from =
      match from with
      | None -> ""
      | Some from -> "-from-" ^ String.map branch_safe_char (Image.id from)
    in
    Printf.sprintf "docker-build-%s-of-%s%s" t.label (CI_git.hash src) from

  let rewrite_from ~log ?from path =
    match from with
    | None -> Lwt.return ()
    | Some base ->
      Lwt_io.with_file ~mode:Lwt_io.input path (fun ch -> Lwt_io.read ch) >>= fun contents ->
      match String.cut ~sep:"\n" contents with
      | None -> CI_utils.failf "Missing newline in %S" path
      | Some (first, rest) ->
        if not (String.is_prefix ~affix:"FROM " (String.Ascii.uppercase first)) then
          CI_utils.failf "Dockerfile %S starts %S, not 'FROM '" path first;
        let first = Printf.sprintf "FROM %s" (Image.id base) in
        CI_live_log.log log "Rewrite Dockerfile's first line to:@\n%s" first;
        let contents = Printf.sprintf "%s\n%s" first rest in
        Lwt_io.with_file ~mode:Lwt_io.output path (fun ch -> Lwt_io.write ch contents)

  let build ~pull ~q dockerfile =
    let pull = if pull then ["--pull"] else [] in
    let q = if q then ["-q"] else [] in
    let cmd = ["docker"; "build"] @ pull @ q @ ["-f"; dockerfile; "."] in
    Array.of_list cmd

  let generate t ~switch ~log trans job_id key =
    let {Key.src; from} = key in
    let output = CI_live_log.write log in
    CI_git.with_clone ~log ~job_id src (fun srcdir ->
        CI_monitored_pool.use t.pool ~log ~label:(label t key) job_id @@ fun () ->
        CI_utils.with_timeout ~switch t.timeout @@ fun switch ->
        validate_path ~log ~base:srcdir t.dockerfile;
        let dockerpath = Filename.concat srcdir t.dockerfile in
        rewrite_from ~log ?from dockerpath >>= fun () ->
        let cwd = Filename.dirname dockerpath in
        let dockerfile = Filename.basename t.dockerfile in
        let cmd = build ~q:false ~pull:(from = None) dockerfile in
        CI_process.run ~cwd ~switch ~output ("", cmd) >>= fun () ->
        let cmd = build ~q:true ~pull:false dockerfile in
        let buffer = Buffer.create 64 in
        let output = Buffer.add_string buffer in
        CI_process.run ~cwd ~switch ~output ("", cmd) >>= fun () ->
        let image = Image.v (String.trim (Buffer.contents buffer)) in
        let data = Cstruct.of_string (Image.id image) in
        DK.Transaction.create_file trans CI_cache.Path.value data >>*= fun () ->
        Lwt.return (Ok image)
      )
end

module Build_cache = CI_cache.Make(Builder)

type t = Build_cache.t

let create ~logs ~pool ~timeout ~label dockerfile =
  Build_cache.create ~logs { Builder.label; dockerfile; timeout; pool }

let build t ?from src =
  let open! CI_term.Infix in
  CI_term.job_id >>= fun job_id ->
  Build_cache.find t job_id {Builder.Key.src; from}

module Runner = struct

  module Key = struct
    type t = {
      image : Image.t;
    }
  end

  type t = {
    label : string;
    command : string list;
    timeout : float;
    pool : CI_monitored_pool.t;
  }

  type context = CI_s.job_id

  type value = unit

  let pp_args =
    let sep = Fmt.(const string) " " in
    Fmt.list ~sep String.dump

  let name t =
    Fmt.strf "docker run %a" pp_args t.command

  let title _t _key = "Docker run"

  let label t {Key.image} =
    Fmt.strf "Run %s in %a" t.label Image.pp_short image

  let load _t _tree _key =
    Lwt.return ()

  let branch t {Key.image} =
    Printf.sprintf "docker-run-%s-in-%s" t.label (String.map branch_safe_char (Image.id image))

  let generate t ~switch ~log _trans job_id key =
    let {Key.image} = key in
    let output = CI_live_log.write log in
    CI_monitored_pool.use t.pool ~log ~label:(label t key) job_id @@ fun () ->
    CI_utils.with_timeout ~switch t.timeout @@ fun switch ->
    let cmd = Array.of_list (["docker"; "run"; "--rm"; Image.id image] @ t.command) in
    CI_process.run ~switch ~log ~output ("", cmd) >>= fun () ->
    Lwt.return (Ok ())
end

module Run_cache = CI_cache.Make(Runner)

type command = Run_cache.t

let command ~logs ~pool ~timeout ~label command =
  Run_cache.create ~logs { Runner.label; command; timeout; pool }

let run t image =
  let open! CI_term.Infix in
  CI_term.job_id >>= fun job_id ->
  Run_cache.find t job_id {Runner.Key.image}


