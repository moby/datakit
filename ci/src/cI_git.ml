open CI_s
open! Astring
open Lwt.Infix

let src = Logs.Src.create "datakit-ci.git" ~doc:"Git plugin for DataKitCI"
module Log = (val Logs.src_log src : Logs.LOG)

type repo = {
  dir : string;
  dir_lock : CI_monitored_pool.t;
  is_ancestor : (string * string, bool Lwt.t) Hashtbl.t;    (* (ancestor, desc) *)
}

(* Git sometimes says:
   Unable to create '.../.git/index.lock': File exists.
   So do its locking for it. *)
let git_lock = Lwt_mutex.create ()

let with_gitdir dir env =
  let not_gitdir s =
    not (String.is_prefix ~affix:"GIT_DIR=" s)
  in
  let env = (Printf.sprintf "GIT_DIR=%s" dir) :: (env |> Array.to_list |> List.filter not_gitdir) in
  Array.of_list env

module Commit = struct
  type t = { repo : repo; hash : string; }
  let hash t = t.hash
  let pp f t = Fmt.string f t.hash

  let is_after ~old t =
    try Hashtbl.find t.repo.is_ancestor (old, t.hash)
    with Not_found ->
      let result =
        let cmd = "", [| "git"; "merge-base"; "--is-ancestor"; old; t.hash |] in
        CI_process.run_with_exit_status ~cwd:t.repo.dir ~output:ignore cmd >|= function
        | Unix.WEXITED 0 -> true
        | Unix.WEXITED _ -> false
        | x -> CI_process.check_status cmd x; true
      in
      Hashtbl.add t.repo.is_ancestor (old, t.hash) result;
      result
end

type commit = Commit.t
let hash = Commit.hash
let is_after = Commit.is_after

module Builder = struct
  module Key = struct
    open !Datakit_github

    type t = [`PR of PR.t | `Ref of Ref.t]

    let pp f = function
      | `PR pr -> PR.pp f pr
      | `Ref r -> Ref.pp f r

    let head = function
      | `PR pr -> PR.commit pr
      | `Ref r -> Ref.commit r

    let hash t = Commit.hash (head t)

    let branch = function
      | `PR pr -> Printf.sprintf "pull/%d/head" (PR.number pr)
      | `Ref r -> Fmt.to_to_string Ref.pp_name (Ref.name r)

  end

  type t = repo

  type context = NoContext

  type value = Commit.t

  let name t =
    "git:" ^ t.dir

  let title _t key =
    Fmt.strf "Git fetch %a" Key.pp key

  let load t _ key =
    let hash = Key.hash key in
    Lwt.return { Commit.repo = t; hash }

  let branch _t key =
    let hash = Key.hash key in
    Printf.sprintf "git-pull-of-%s" hash

  let generate t ~switch ~log _trans NoContext target =
    let output = CI_live_log.write log in
    let hash = Key.hash target in
    let tmp_branch = branch t target in
    let tmp_tag = Printf.sprintf "%s.new" tmp_branch in
    let env = Unix.environment () |> with_gitdir (Filename.concat t.dir ".git") in
    (* First, see if we've already got this commit. *)
    Lwt.try_bind
      (fun () ->
         Lwt_mutex.with_lock git_lock @@ fun () ->
         CI_process.run ~env ~output ("", [| "git"; "branch"; "-f"; tmp_branch; hash |])
      )
      (fun () ->
         CI_live_log.log log "Commit %s is available locally, so not fetching" hash;
         Lwt.return @@ Ok { Commit.repo = t; hash }
      )
      (fun ex ->
         Log.info (fun f -> f "Commit %S did not resolve (%s); fetching..." hash (Printexc.to_string ex));
         CI_live_log.log log "Fetching PR branch";
         (* We can't be sure the PR's head still exists, but we can fetch the current
            head and then try to switch to the one we want. *)
         (* We fetch to a tag rather than a branch in case the target is a tag object. *)
         (* Three step process to ensure we don't end up pointing at the wrong commit *)
         let cmd = [| "git"; "fetch"; "origin";
                      Printf.sprintf "%s:tags/%s" (Key.branch target) tmp_tag |] in
         CI_process.run ~switch ~env ~output ("", cmd) >>= fun () ->
         Lwt_mutex.with_lock git_lock @@ fun () -> (* (hopefully fetch can handle parallel uses) *)
         CI_process.run ~env ~output ("", [| "git"; "branch"; "-f"; tmp_branch; hash |]) >>= fun () ->
         CI_process.run ~env ~output ("", [| "git"; "tag"; "-d"; tmp_tag |]) >>= fun () ->
         Lwt.return @@ Ok { Commit.repo = t; hash }
      )
end

module PR_Cache = CI_cache.Make(Builder)

type ready_t = {
  repo : repo;
  fetches : PR_Cache.t;
}

type t = ready_t Lwt.t

let is_directory d =
  match Sys.is_directory d with
  | r -> r
  | exception Sys_error _ -> false

let clone_if_missing ?remote ~dir =
  if is_directory dir then Lwt.return ()
  else match remote with
    | None ->
      CI_utils.failf "Directory %S does not exist (and no ~remote provided, so can't clone it automatically)." dir
    | Some remote ->
      Lwt.catch (fun () ->
          CI_process.run ~output:print_string ("", [| "git"; "clone"; remote; dir |])
        )
        (fun ex ->
           Log.err (fun f -> f "Failed to clone Git repository: %a" CI_utils.pp_exn ex);
           Lwt.fail ex
        )

let v ?remote ~logs dir =
  let dir = CI_utils.abs_path dir in
  clone_if_missing ?remote ~dir >|= fun () ->
  let repo = {
    dir;
    dir_lock = CI_monitored_pool.create dir 1;
    is_ancestor = Hashtbl.create 11;
  } in
  {
    repo;
    fetches = PR_Cache.create ~logs repo;
  }

let fetch_head t target =
  let open !CI_term.Infix in
  CI_term.of_lwt_quick t >>= fun t ->
  CI_term.target target >>=
  PR_Cache.find t.fetches Builder.NoContext

let with_checkout ~log ~job_id {Commit.repo = {dir; dir_lock; _}; hash} fn =
  CI_live_log.enter_with_pending_reason log ("Waiting for lock on " ^ dir) (CI_monitored_pool.use ~log dir_lock job_id) @@ fun () ->
  let output = CI_live_log.write log in
  Lwt_mutex.with_lock git_lock (fun () ->
      CI_process.run ~cwd:dir ~output ("", [| "git"; "reset"; "--hard"; hash |]) >>= fun () ->
      CI_process.run ~cwd:dir ~output ("", [| "git"; "clean"; "-xdf" |])
    )
  >>= fun () ->
  fn dir

let with_clone ~log ~job_id {Commit.repo = {dir; dir_lock; _}; hash} fn =
  let output = CI_live_log.write log in
  CI_utils.with_tmpdir ~prefix:"with_clone" ~mode:0o700 @@ fun tmpdir ->
  CI_live_log.enter_with_pending_reason log
    ("Waiting for lock on " ^ dir)
    (CI_monitored_pool.use ~log dir_lock job_id) (fun () ->
        CI_process.run ~output ("", [| "git"; "clone"; dir; tmpdir |])
      )
  >>= fun () ->
  CI_process.run ~cwd:tmpdir ~output ("", [| "git"; "reset"; "--hard"; hash |]) >>= fun () ->
  fn tmpdir

module Shell_builder = struct
  type t = {
    label : string;
    cmds : string array list;
    timeout : float;
    clone : bool;               (* Whether to make a clone before running the commands *)
  }

  module Key = Commit

  type context = job_id

  type value = unit

  let name t =
    "shell:" ^ t.label

  let title t commit =
    Fmt.strf "Run %s on commit %a" t.label Commit.pp commit

  let generate t ~switch ~log _trans job_id git_dir =
    let build working_dir =
      CI_utils.with_timeout ~switch t.timeout (fun switch ->
          let output = CI_live_log.write log in
          let sep = Fmt.(const string) " " in
          t.cmds |> Lwt_list.iter_s (fun cmd ->
              CI_live_log.log log "Running @[<h>%a@]..." (Fmt.array ~sep String.dump) cmd;
              CI_process.run ~cwd:working_dir ~switch ~output ("", cmd)
            )
          >|= fun () ->
          Ok ()
        )
    in
    if t.clone then
      with_clone ~job_id ~log git_dir build
    else
      with_checkout ~job_id ~log git_dir build

  let branch t commit =
    Printf.sprintf "shell-of-%s-on-%s" t.label (Commit.hash commit)

  let load _t _ _key = Lwt.return ()

end

module Shell_cache = CI_cache.Make(Shell_builder)

type command = Shell_cache.t

let command ~logs ~timeout ~label ~clone cmds =
  Shell_cache.create ~logs { Shell_builder.label; cmds; clone; timeout }

let run command git_dir =
  let open! CI_term.Infix in
  CI_term.job_id >>= fun job_id ->
  Shell_cache.find command job_id git_dir
