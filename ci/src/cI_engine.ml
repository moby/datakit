open CI_utils
open Result
open! Astring
open Lwt.Infix

module IntSet = Set.Make(CI_utils.Int)

type database = {
  dk : DK.t;
  gh_hooks : CI_github_hooks.t;
}

type job = {
  name : string;
  parent : target;
  mutable term : string CI_term.t;
  mutable cancel : unit -> unit;        (* Cancel the previous evaluation, if any *)
  mutable state : string * CI_state.t;                 (* The last result of evaluating [term] (commit, state) *)
}
and target = {
  project_id : CI_projectID.t;
  mutable head : [`PR of CI_github_hooks.PR.t | `Ref of CI_github_hooks.Ref.t];
  mutable jobs : job list;      (* (only mutable for init) *)
}

let pp_target f target =
  match target.head with
  | `PR pr -> CI_github_hooks.PR.dump f pr
  | `Ref r -> CI_github_hooks.Ref.dump f r

let commit = function
  | `PR pr -> CI_github_hooks.PR.head pr
  | `Ref r -> CI_github_hooks.Ref.head r

let pp_job f j =
  Fmt.pf f "%a/%s" pp_target j.parent j.name

let state job = snd job.state
let git_target job = job.head
let title pr =
  match pr.head with
  | `PR pr -> CI_github_hooks.PR.title pr
  | `Ref r -> Fmt.strf "Branch %a" Datakit_path.pp (CI_github_hooks.Ref.name r)
let jobs pr = pr.jobs
let job_name j = j.name
let project target = target.project_id

type project = {
  terms : string CI_term.t String.Map.t;
  canaries : CI_target.ID_Set.t option;
  mutable open_prs : target IntMap.t;
  mutable refs : target Datakit_path.Map.t;
}

type t = {
  web_ui : Uri.t;
  connect_dk : unit -> DK.t Lwt.t;
  projects : project CI_projectID.Map.t;
  term_lock : Lwt_mutex.t;              (* Held while evaluating terms *)
  mutable db : database Lwt.t;
}

let rec connect connect_dk =
  Lwt.catch
    (fun () ->
      connect_dk () >|= fun dk ->
      let gh_hooks = CI_github_hooks.connect dk in
      {
        dk;
        gh_hooks;
      }
    )
    (fun ex ->
       Log.warn (fun f -> f "Failed to connect to DataKit: %s (will retry in 10s)" (Printexc.to_string ex));
       Lwt_unix.sleep 10.0 >>= fun () ->
       connect connect_dk
    )

let create ~web_ui ?canaries connect_dk projects =
  begin match canaries with
    | None -> ()
    | Some canaries -> canaries |> CI_projectID.Map.iter (fun id _ ->
        if not (CI_projectID.Map.mem id projects) then
          Log.warn (fun f -> f "Canary project %a not in list of monitored projects" CI_projectID.pp id)
      )
  end;
  let projects =
    projects |> CI_projectID.Map.mapi (fun id terms ->
        let canaries =
          match canaries with
          | None -> None
          | Some canaries -> Some (CI_projectID.Map.find id canaries |> default CI_target.ID_Set.empty)
        in
        {terms; open_prs = IntMap.empty; refs = Datakit_path.Map.empty; canaries}
      )
  in
  {
    web_ui;
    connect_dk;
    db = connect connect_dk;
    projects;
    term_lock = Lwt_mutex.create ();
  }

let dk t       = t.db >|= fun db -> db.dk
let gh_hooks t = t.db >|= fun db -> db.gh_hooks

let targets t =
  t.projects |> CI_projectID.Map.map (fun project -> project.open_prs, project.refs)

let escape_ref path =
  Uri.pct_encode ~scheme:"http" (String.concat ~sep:"/" (Datakit_path.unwrap path))

let set_status t { CI_projectID.user; project } target name ~status ~descr =
  let commit, target_url =
    match target with
    | `PR pr ->
      Log.info (fun f -> f "Job %a:%s -> %s" CI_github_hooks.PR.dump pr name descr);
      let url = Uri.with_path t.web_ui (Printf.sprintf "pr/%s/%s/%d" user project (CI_github_hooks.PR.id pr)) in
      let commit = CI_github_hooks.PR.head pr in
      (commit, url)
    | `Ref r ->
      Log.info (fun f -> f "Job ref %a:%s -> %s" CI_github_hooks.Ref.dump r name descr);
      let url = Uri.with_path t.web_ui (Printf.sprintf "ref/%s/%s/%s"
                                          user project (escape_ref (CI_github_hooks.Ref.name r))) in
      let commit = CI_github_hooks.Ref.head r in
      (commit, url)
  in
  gh_hooks t >>= fun gh_hooks ->
  CI_github_hooks.set_state gh_hooks (CI_github_hooks.CI.datakit_ci name) ~status ~descr ~target_url commit

let reconnect t =
  match Lwt.state t.db with
  | Lwt.Sleep -> ()     (* Already reconnecting *)
  | Lwt.Fail _ | Lwt.Return _ ->
    Log.info (fun f -> f "Reconnecting to DataKit...");
    t.db <- connect t.connect_dk

let rec auto_restart t ?switch label fn =
  dk t >>= fun dk ->
  Lwt.catch fn
    (fun ex ->
       match switch with
       | Some switch when not (Lwt_switch.is_on switch) ->
         Log.info (fun f -> f "Switch is off, so not auto-restarting after error: %s" (Printexc.to_string ex));
         Lwt.return `Abort
       | None | Some _ ->
         DK.branch dk "master" >>= function
         | Ok _ -> Lwt.fail ex              (* Database is OK; must be something else *)
         | Error (`Msg err) ->
           Log.warn (fun f -> f "%s: database connection failed: %s\n(probable cause of %s)" label err (Printexc.to_string ex));
           reconnect t;
           auto_restart t label fn
    )

(* Note: must hold [t.term_lock] while calling this. *)
let rec recalculate t ~snapshot job =
  Log.debug (fun f -> f "Recalculate %a" pp_job job);
  let recalc () =
    Lwt.async (fun () ->
        Lwt_mutex.with_lock t.term_lock (fun () ->
            gh_hooks t >>= fun gh_hooks ->
            CI_github_hooks.snapshot gh_hooks >>= fun snapshot ->
            recalculate t ~snapshot job
          )
      )
  in
  job.cancel ();        (* Stop any previous evaluation *)
  let head = job.parent.head in
  Lwt.catch
    (fun () ->
       let r, cancel = CI_term.run ~snapshot ~target:head ~recalc ~dk:(fun () -> dk t) job.term in
       job.cancel <- cancel;
       r
    )
    (function
      | Failure msg ->
        Lwt.return (Error (`Failure msg), CI_result.Step_log.Empty)
      | ex ->
        Lwt.return (Error (`Failure (Printexc.to_string ex)), CI_result.Step_log.Empty)
    )
  >>= fun (result, logs) ->
  let status, descr =
    match result with
    | Ok descr -> `Success, descr
    | Error (`Pending descr) -> `Pending, descr
    | Error (`Failure descr) -> `Failure, descr
  in
  let (old_head, old_state) = job.state in
  let new_hash = CI_github_hooks.Commit.hash (commit head) in
  begin if (old_head, old_state.CI_state.status, old_state.CI_state.descr) <> (new_hash, status, descr) then (
    set_status t job.parent.project_id head job.name ~status ~descr
  ) else (
    Lwt.return ()
  )
  end >|= fun () ->
  let state = (new_hash, { CI_state.status; descr; logs }) in
  job.state <- state

let make_job ~parent name term =
  let head_commit = commit parent.head in
  let dk_state = CI_github_hooks.Commit.state (CI_github_hooks.CI.datakit_ci name) head_commit in
  CI_github_hooks.Commit_state.status dk_state >>= fun status ->
  CI_github_hooks.Commit_state.descr dk_state >>= fun descr ->
  let state =
    match status, descr with
    | Some status, Some descr -> { CI_state.status; descr; logs = CI_result.Step_log.Empty }
    | _ -> { CI_state.status = `Pending; descr = "(new)"; logs = CI_result.Step_log.Empty }
  in
  let hash = CI_github_hooks.Commit.hash head_commit in
  Lwt.return {
    name;
    parent;
    term;
    cancel = ignore;
    state = (hash, state);
  }

let apply_canaries canaries prs refs =
  match canaries with
  | None -> (prs, refs)
  | Some canaries ->
    let prs = prs |> List.filter (fun pr -> CI_target.ID_Set.mem (`PR (CI_github_hooks.PR.id pr)) canaries) in
    let refs = refs |> List.filter (fun r -> CI_target.ID_Set.mem (`Ref (CI_github_hooks.Ref.name r)) canaries) in
    (prs, refs)

let listen ?switch t =
  auto_restart t ?switch "monitor" @@ fun () ->
  Log.info (fun f -> f "Starting monitor loop");
  gh_hooks t >>= fun gh_hooks ->
  let check_pr ~snapshot ~project_id project pr =
    let id = CI_github_hooks.PR.id pr in
    Log.debug (fun f -> f "Checking for work on %a" CI_github_hooks.PR.dump pr);
    begin match IntMap.find id project.open_prs with
      | None ->
        let open_pr = {
          project_id;
          head = `PR pr;
          jobs = [];
        } in
        String.Map.bindings project.terms
        |> Lwt_list.map_s (fun (name, term) -> make_job ~parent:open_pr name term)
        >>= fun jobs ->
        open_pr.jobs <- jobs;
        project.open_prs <- IntMap.add id open_pr project.open_prs;
        Lwt.return open_pr
      | Some open_pr ->
        open_pr.head <- `PR pr; (* Update in all cases, because we read other things from the same snapshot.
                                   XXX: so compare is very misleading here! *)
        Lwt.return open_pr
    end >>= fun open_pr ->
    Lwt_list.iter_s (recalculate t ~snapshot) open_pr.jobs
  in
  let check_ref ~snapshot ~project_id project r =
    let id = CI_github_hooks.Ref.name r in
    Log.debug (fun f -> f "Checking for work on %a" CI_github_hooks.Ref.dump r);
    begin match Datakit_path.Map.find id project.refs with
      | exception Not_found ->
        let target = {
          project_id;
          head = `Ref r;
          jobs = [];
        } in
        String.Map.bindings project.terms
        |> Lwt_list.map_s (fun (name, term) -> make_job ~parent:target name term)
        >>= fun jobs ->
        target.jobs <- jobs;
        project.refs <- Datakit_path.Map.add id target project.refs;
        Lwt.return target
      | target ->
        target.head <- `Ref r;
        Lwt.return target
    end >>= fun target ->
    Lwt_list.iter_s (recalculate t ~snapshot) target.jobs
  in
  CI_github_hooks.enable_monitoring gh_hooks (List.map fst (CI_projectID.Map.bindings t.projects)) >>= fun () ->
  CI_github_hooks.monitor ?switch gh_hooks (fun snapshot ->
      t.projects |> CI_projectID.Map.bindings |> Lwt_list.iter_s (fun (project_id, project) ->
          CI_github_hooks.project snapshot project_id >>= fun (prs, refs) ->
          Log.debug (fun f -> f "Monitor iter");
          let prs, refs = apply_canaries project.canaries prs refs in
          (* PRs *)
          let current_prs = List.fold_left (fun acc pr -> IntSet.add (CI_github_hooks.PR.id pr) acc) IntSet.empty prs in
          let is_current id open_pr =
            let current = IntSet.mem id current_prs in
            if not current then (
              Log.info (fun f -> f "Removing closed PR#%d" id);
              List.iter (fun j -> j.cancel ()) open_pr.jobs
            );
            current
          in
          project.open_prs <- IntMap.filter is_current project.open_prs;
          Lwt_mutex.with_lock t.term_lock (fun () ->
              Lwt_list.iter_s (check_pr ~snapshot ~project_id project) prs
            )
          >>= fun () ->
          (* Refs *)
          let current_refs = List.fold_left (fun acc r ->
              Datakit_path.Set.add (CI_github_hooks.Ref.name r) acc
            ) Datakit_path.Set.empty refs in
          let is_current id target =
            let current = Datakit_path.Set.mem id current_refs in
            if not current then (
              Log.info (fun f -> f "Removing closed branch %a" Datakit_path.pp id);
              List.iter (fun j -> j.cancel ()) target.jobs
            );
            current
          in
          project.refs <- Datakit_path.Map.filter is_current project.refs;
          Lwt_mutex.with_lock t.term_lock (fun () ->
              Lwt_list.iter_s (check_ref ~snapshot ~project_id project) refs
            )
          (* TODO: diff with previous set and kill jobs we no longer require *)
        )
    )

let rebuild t ~branch_name =
  let jobs_needing_recalc = ref [] in
  let triggers = ref [] in
  let rec check_logs =
    let open CI_result.Step_log in
    function
    | Saved {branch; rebuild; _} when branch = branch_name ->
      if not (Lazy.is_val rebuild) then triggers := Lazy.force rebuild :: !triggers;
      true
    | Saved _ -> false
    | Empty -> false
    | Pair (a, b) ->
      let a = check_logs a in
      let b = check_logs b in
      a || b
    | Live _ -> false
  in
  let check_job job =
    let _, state = job.state in
    if check_logs state.CI_state.logs then
      jobs_needing_recalc := job :: !jobs_needing_recalc
  in
  let check_target target = List.iter check_job target.jobs in
  t.projects |> CI_projectID.Map.iter (fun _ project ->
      project.open_prs |> IntMap.iter (fun _ x -> check_target x);
      project.refs |> Datakit_path.Map.iter (fun _ x -> check_target x);
    );
  match !triggers, !jobs_needing_recalc with
  | [], [] -> CI_utils.failf "No job depends on %S, so can't rebuild anything" branch_name
  | triggers, jobs_needing_recalc ->
    Lwt.join triggers >>= fun () ->
    Lwt_mutex.with_lock t.term_lock (fun () ->
        gh_hooks t >>= fun gh_hooks ->
        CI_github_hooks.snapshot gh_hooks >>= fun snapshot ->
        Lwt_list.iter_s (recalculate t ~snapshot) jobs_needing_recalc
      )
