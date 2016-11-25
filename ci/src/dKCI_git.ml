open DataKitCI

open! Astring
open Lwt.Infix

let src = Logs.Src.create "datakit-ci.git" ~doc:"Git plugin for DataKitCI"
module Log = (val Logs.src_log src : Logs.LOG)

type repo = {
  dir : string;
  dir_lock : Monitored_pool.t;
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
  type t = {
    repo : repo;
    hash : string;
  }

  let compare a b =
    String.compare a.hash b.hash

  let hash t = t.hash

  let pp f t =
    Fmt.string f t.hash

  let includes_lwt t ~commit =
    try Hashtbl.find t.repo.is_ancestor (commit, t.hash)
    with Not_found ->
      let result =
        let cmd = "", [| "git"; "merge-base"; "--is-ancestor"; commit; t.hash |] in
        Process.run_with_exit_status ~cwd:t.repo.dir ~output:ignore cmd >|= function
        | Unix.WEXITED 0 -> true
        | Unix.WEXITED _ -> false
        | x -> Process.check_status cmd x; true
      in
      Hashtbl.add t.repo.is_ancestor (commit, t.hash) result;
      result

  let includes t ~commit =
    Term.of_lwt_quick (includes_lwt t ~commit)
end

module Builder = struct
  module Key = struct
    type t = [`PR of Github_hooks.PR.t | `Ref of Github_hooks.Ref.t]

    let pp f = function
      | `PR pr -> Github_hooks.PR.dump f pr
      | `Ref r -> Github_hooks.Ref.dump f r

    let compare a b =
      match a, b with
      | `PR a, `PR b -> Github_hooks.PR.compare a b
      | `Ref a, `Ref b -> Github_hooks.Ref.compare a b
      | `Ref _, `PR _ -> -1
      | `PR _, `Ref _ -> 1

    let head = function
      | `PR pr -> Github_hooks.PR.head pr
      | `Ref r -> Github_hooks.Ref.head r

    let hash t = Github_hooks.Commit.hash (head t)

    let branch = function
      | `PR pr -> Printf.sprintf "pull/%d/head" (Github_hooks.PR.id pr)
      | `Ref r -> Github_hooks.Ref.name r |> Datakit_path.to_hum

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

  let generate t ~switch ~log _trans NoContext pr =
    let output = Live_log.write log in
    let hash = Key.hash pr in
    let tmp_branch = branch t pr in
    let env = Unix.environment () |> with_gitdir (Filename.concat t.dir ".git") in
    (* First, see if we've already got this commit. *)
    Lwt.try_bind
      (fun () ->
         Lwt_mutex.with_lock git_lock @@ fun () ->
         Process.run ~env ~output ("", [| "git"; "branch"; "-f"; tmp_branch; hash |])
      )
      (fun () ->
         Live_log.log log "Commit %s is available locally, so not fetching" hash;
         Lwt.return @@ Ok { Commit.repo = t; hash }
      )
      (fun ex ->
         Log.info (fun f -> f "Commit %S did not resolve (%s); fetching..." hash (Printexc.to_string ex));
         Live_log.log log "Fetching PR branch";
         (* We can't be sure the PR's head still exists, but we can fetch the current
            head and then try to switch to the one we want. *)
         (* Three step process to ensure we don't end up pointing at the wrong commit *)
         Process.run ~switch ~env ~output ("", [| "git"; "fetch"; "origin"; Printf.sprintf "%s:%s.new" (Key.branch pr) tmp_branch |]) >>= fun () ->
         Lwt_mutex.with_lock git_lock @@ fun () -> (* (hopefully fetch can handle parallel uses) *)
         Process.run ~env ~output ("", [| "git"; "branch"; "-f"; tmp_branch; hash |]) >>= fun () ->
         Process.run ~env ~output ("", [| "git"; "branch"; "-D"; tmp_branch ^ ".new" |]) >>= fun () ->
         Lwt.return @@ Ok { Commit.repo = t; hash }
      )
end

module PR_Cache = Cache.Make(Builder)

type t = {
  repo : repo;
  fetches : PR_Cache.t;
}

let connect ~logs ~dir =
  let repo = {
    dir;
    dir_lock = Monitored_pool.create dir 1;
    is_ancestor = Hashtbl.create 11;
  } in
  {
    repo;
    fetches = PR_Cache.create ~logs repo;
  }

let fetch_head t target =
  let ( >>~= ) = Term.Infix.( >>= ) in
  Term.github_target target >>~= PR_Cache.term t.fetches Builder.NoContext

let with_checkout ~log ~job_id {Commit.repo = {dir; dir_lock; _}; hash} fn =
  Live_log.enter_with_pending_reason log ("Waiting for lock on " ^ dir) (Monitored_pool.use ~log dir_lock job_id) @@ fun () ->
  let output = Live_log.write log in
  Lwt_mutex.with_lock git_lock (fun () ->
      Process.run ~cwd:dir ~output ("", [| "git"; "reset"; "--hard"; hash |]) >>= fun () ->
      Process.run ~cwd:dir ~output ("", [| "git"; "clean"; "-xdf" |])
    )
  >>= fun () ->
  fn dir

let with_clone ~log ~job_id {Commit.repo = {dir; dir_lock; _}; hash} fn =
  let output = Live_log.write log in
  Utils.with_tmpdir ~prefix:"with_clone" ~mode:0o700 @@ fun tmpdir ->
  Live_log.enter_with_pending_reason log ("Waiting for lock on " ^ dir) (Monitored_pool.use ~log dir_lock job_id) (fun () ->
      Process.run ~output ("", [| "git"; "clone"; dir; tmpdir |])
    )
  >>= fun () ->
  Process.run ~cwd:tmpdir ~output ("", [| "git"; "reset"; "--hard"; hash |]) >>= fun () ->
  fn tmpdir

module Shell_builder = struct
  type t = {
    label : string;
    cmds : string array list;
    timeout : float;
    clone : bool;               (* Whether to make a clone before running the commands *)
  }

  module Key = Commit

  type context = DataKitCI.job_id

  type value = unit

  let name t =
    "shell:" ^ t.label

  let title t commit =
    Fmt.strf "Run %s on commit %a" t.label Commit.pp commit

  let generate t ~switch ~log _trans job_id git_dir =
    let build working_dir =
      Utils.with_timeout ~switch t.timeout (fun switch ->
          let output = Live_log.write log in
          let sep = Fmt.(const string) " " in
          t.cmds |> Lwt_list.iter_s (fun cmd ->
              Live_log.log log "Running @[<h>%a@]..." (Fmt.array ~sep String.dump) cmd;
              Process.run ~cwd:working_dir ~switch ~output ("", cmd)
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

module Shell_cache = Cache.Make(Shell_builder)

type command = Shell_cache.t

let command ~logs ~timeout ~label ~clone cmds =
  Shell_cache.create ~logs { Shell_builder.label; cmds; clone; timeout }

let run command git_dir =
  let open! Term.Infix in
  Term.job_id >>= fun job ->
  Shell_cache.term command job git_dir
