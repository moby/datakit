open Datakit_github
open CI_utils
open Result
open! Astring
open Lwt.Infix
module Conv = Datakit_github_conv.Make(DK)

module Metrics = struct
  let namespace = "DataKitCI"
  let subsystem = "engine"

  let connection_attempts =
    let help = "Number of attempted connections to DataKit" in
    CI_prometheus.Counter.v ~help ~namespace ~subsystem "connection_attempts_total"

  let connection_failures =
    let help = "Number of failed attempts to connect to DataKit" in
    CI_prometheus.Counter.v ~help ~namespace ~subsystem "connection_failures_total"

  let status_updates =
    let help = "Number of status updates" in
    CI_prometheus.Counter.v ~help ~namespace ~subsystem "status_updates_total"

  let update_notifications =
    let help = "Number of notifications from DataKit" in
    CI_prometheus.Counter.v ~help ~namespace ~subsystem "update_notifications_total"
end

type job = {
  name : string;
  parent : target;
  mutable term : string CI_term.t;
  mutable cancel : unit -> unit;    (* Cancel the previous evaluation, if any *)
  mutable state : string * CI_state.t;  (* The last result of evaluating [term]
                                           (commit, state) *)
}
and target = {
  mutable head : [`PR of PR.t | `Ref of Ref.t];
  mutable jobs : job list;      (* (only mutable for init) *)
}

let job_id job =
  let target = match job.parent.head with
    | `PR pr -> `PR (PR.id pr)
    | `Ref r -> `Ref (Ref.id r)
  in
  target, job.name

let pp_target f target =
  match target.head with
  | `PR pr -> Fmt.pf f "%a/pr/%d" Repo.pp (PR.repo pr) (PR.number pr)
  | `Ref r -> Fmt.pf f "%a/ref/%a" Repo.pp (Ref.repo r) Ref.pp_name (Ref.name r)

let commit = function
  | `PR pr -> PR.commit pr
  | `Ref r -> Ref.commit r

let repo t = match t.head with
  | `PR r  -> PR.repo r
  | `Ref r -> Ref.repo r

let pp_job f j =
  Fmt.pf f "%a:%s" pp_target j.parent j.name

let state job = snd job.state
let git_target job = job.head

let title pr =
  match pr.head with
  | `PR pr -> PR.title pr
  | `Ref r -> Fmt.strf "Ref %a" Ref.pp_name (Ref.name r)

let jobs pr = pr.jobs
let job_name j = j.name

type project = {
  make_terms : CI_target.t -> string CI_term.t String.Map.t;
  canaries : CI_target.Set.t option;
  mutable open_prs : target PR.Index.t;
  mutable refs : target Ref.Index.t;
}

type t = {
  web_ui : Uri.t;
  connect_dk : unit -> DK.t Lwt.t;
  projects : project Repo.Map.t;
  term_lock : Lwt_mutex.t;              (* Held while evaluating terms *)
  mutable dk : DK.t Lwt.t;
}

let dk t = t.dk

let rec connect connect_dk =
  Lwt.catch
    (fun () ->
       CI_prometheus.Counter.inc_one Metrics.connection_attempts;
       connect_dk ()
    )
    (fun ex ->
       CI_prometheus.Counter.inc_one Metrics.connection_failures;
       Log.warn (fun f -> f "Failed to connect to DataKit: %s (will retry in 10s)" (Printexc.to_string ex));
       Lwt_unix.sleep 10.0 >>= fun () ->
       connect connect_dk
    )

let create ~web_ui ?canaries connect_dk projects =
  begin match canaries with
    | None -> ()
    | Some canaries ->
      Repo.Map.iter (fun id _ ->
          if not (Repo.Map.mem id projects) then
            Log.warn (fun f -> f "Canary project %a not in list of monitored \
                                  projects" Repo.pp id)
        ) canaries
  end;
  let projects =
    Repo.Map.mapi (fun id make_terms ->
        let canaries =
          match canaries with
          | None -> None
          | Some canaries ->
            Some (Repo.Map.find id canaries
                  |> CI_utils.default CI_target.Set.empty)
        in
        { make_terms;
          open_prs = PR.Index.empty;
          refs = Ref.Index.empty;
          canaries }
      ) projects
  in
  {
    web_ui;
    connect_dk;
    dk = connect connect_dk;
    projects;
    term_lock = Lwt_mutex.create ();
  }

let prs t = t.projects |> Repo.Map.map (fun project -> project.open_prs)
let refs t = t.projects |> Repo.Map.map (fun project -> project.refs)

let escape_ref path = List.map (fun p -> Uri.pct_encode ~scheme:"http" p) path

let metadata_branch = "github-metadata"

let take_snapshot t =
  t.dk >>= fun t ->
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.head metadata >|*= function
  | None   -> failf "Metadata branch does not exist!"
  | Some c -> DK.Commit.tree c


let update_status t ~message s =
  t.dk >>= fun t ->
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.with_transaction metadata (fun t ->
      Conv.update_elt t (`Status s) >>= fun () ->
      Log.debug (fun f -> f "set_state: %s" message);
      DK.Transaction.commit t ~message
    ) >>*= Lwt.return

let enable_monitoring t repos =
  t.dk >>= fun t ->
  DK.branch t metadata_branch >>*= fun metadata_branch ->
  DK.Branch.with_transaction metadata_branch (fun t ->
      Lwt_list.iter_s (fun r -> Conv.update_elt t (`Repo r)) repos >>= fun () ->
      let commit () = DK.Transaction.commit t ~message:"Add .monitor files" in
      DK.Transaction.parents t >>*= function
      | []   -> commit ()
      | h::_ ->
        DK.Transaction.diff t h >>*= fun diff ->
        if diff <> [] then commit ()
        else DK.Transaction.abort t >|= fun () -> Ok ()
    ) >>*= Lwt.return

let monitor t ?switch fn =
  t.dk >>= fun t ->
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.wait_for_head metadata ?switch (function
      | None   -> ok `Again
      | Some c -> fn (DK.Commit.tree c) >>= fun () -> ok `Again
    )
  >|*= function
  | `Abort -> `Abort
  | `Finish `Never -> assert false

let datakit_ci x = ["ci"; "datakit"; x]

let set_status t target name ~status ~descr =
  let { Repo.user; repo } = match target with
    | `PR x -> PR.repo x | `Ref x -> Ref.repo x
  in
  CI_prometheus.Counter.inc_one Metrics.status_updates;
  let commit, url =
    match target with
    | `PR pr ->
      Log.info (fun f -> f "Job %a:%s -> %s" PR.pp_id (PR.id pr) name descr);
      let url =
        Uri.with_path t.web_ui (Printf.sprintf "pr/%s/%s/%d" user repo (PR.number pr))
      in
      let commit = PR.commit pr in
      (commit, url)
    | `Ref r ->
      Log.info (fun f -> f "Job ref %a:%s -> %s" Ref.pp_id (Ref.id r) name descr);
      let url =
        Uri.with_path t.web_ui
          (Fmt.strf "ref/%s/%s/%a" user repo Ref.pp_name (escape_ref (Ref.name r)))
      in
      let commit = Ref.commit r in
      (commit, url)
  in
  let message =
    Fmt.strf "Set state of %a: %s = %a"
      Commit.pp_hash (Commit.hash @@ CI_target.head target)
      name Status_state.pp status
  in
  let status =
    let ci = datakit_ci name in
    Status.v ~description:descr ~url commit ci status
  in
  update_status t ~message status

let reconnect t =
  match Lwt.state t.dk with
  | Lwt.Sleep -> ()     (* Already reconnecting *)
  | Lwt.Fail _ | Lwt.Return _ ->
    Log.info (fun f -> f "Reconnecting to DataKit...");
    t.dk <- connect t.connect_dk

let rec auto_restart t ?switch label fn =
  t.dk >>= fun dk ->
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
            take_snapshot t >>= fun snapshot ->
            recalculate t ~snapshot job
          )
      )
  in
  job.cancel ();        (* Stop any previous evaluation *)
  let head = job.parent.head in
  Lwt.catch
    (fun () ->
       let r, cancel =
         CI_term.run ~snapshot ~job_id:(job_id job) ~recalc ~dk:(fun () -> t.dk)
           job.term
       in
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
  let new_hash = Commit.hash (commit head) in
  begin if (old_head, old_state.CI_state.status, old_state.CI_state.descr) <> (new_hash, status, descr) then (
      set_status t head job.name ~status ~descr
    ) else (
      Lwt.return ()
    )
  end >|= fun () ->
  let state = (new_hash, { CI_state.status; descr; logs }) in
  job.state <- state

let make_job snapshot ~parent name term =
  let head_commit = commit parent.head in
  let id = head_commit, datakit_ci name in
  Conv.status snapshot id >|= fun status ->
  let state = match status with None -> None | Some s -> Some (Status.state s) in
  let descr = match status with None -> None | Some s -> Status.description s in
  let state =
    match state, descr with
    | Some status, Some descr ->
      { CI_state.status; descr; logs = CI_result.Step_log.Empty }
    | _ ->
      { CI_state.status = `Pending; descr = "(new)";
        logs = CI_result.Step_log.Empty }
  in
  let hash = Commit.hash head_commit in
  { name;
    parent;
    term;
    cancel = ignore;
    state = (hash, state); }

let apply_canaries canaries prs refs =
  match canaries with
  | None -> (prs, refs)
  | Some canaries ->
    let prs =
      PR.Index.filter (fun id _ ->
          CI_target.Set.mem (`PR id) canaries
        ) prs
    in
    let refs =
      Ref.Index.filter (fun id _ ->
          CI_target.Set.mem (`Ref id) canaries
        ) refs
    in
    (prs, refs)

let listen ?switch t =
  auto_restart t ?switch "monitor" @@ fun () ->
  Log.info (fun f -> f "Starting monitor loop");
  let check_pr ~snapshot project (id, pr) =
    Log.debug (fun f -> f "Checking for work on %a" PR.pp_id id);
    begin match PR.Index.find id project.open_prs with
      | None ->
        let open_pr = { head = `PR pr; jobs = [] } in
        let terms = project.make_terms (`PR id) in
        String.Map.bindings terms
        |> Lwt_list.map_s (fun (name, term) ->
            make_job snapshot ~parent:open_pr name term)
        >>= fun jobs ->
        open_pr.jobs <- jobs;
        project.open_prs <- PR.Index.add id open_pr project.open_prs;
        Lwt.return open_pr
      | Some open_pr ->
        open_pr.head <- `PR pr; (* Update in all cases, because we read other things from the same snapshot.
                                   XXX: so compare is very misleading here! *)
        Lwt.return open_pr
    end >>= fun open_pr ->
    Lwt_list.iter_s (recalculate t ~snapshot) open_pr.jobs
  in
  let check_ref ~snapshot project (id, r) =
    Log.debug (fun f -> f "Checking for work on %a" Ref.pp_id id);
    begin match Ref.Index.find id project.refs with
      | None ->
        let target = {
          head = `Ref r;
          jobs = [];
        } in
        let terms = project.make_terms @@ `Ref id in
        String.Map.bindings terms
        |> Lwt_list.map_s (fun (name, term) ->
            make_job snapshot ~parent:target name term)
        >>= fun jobs ->
        target.jobs <- jobs;
        project.refs <- Ref.Index.add id target project.refs;
        Lwt.return target
      | Some target ->
        target.head <- `Ref r;
        Lwt.return target
    end >>= fun target ->
    Lwt_list.iter_s (recalculate t ~snapshot) target.jobs
  in
  enable_monitoring t (List.map fst (Repo.Map.bindings t.projects)) >>= fun () ->
  monitor ?switch t (fun snapshot ->
      CI_prometheus.Counter.inc_one Metrics.update_notifications;
      t.projects |> Repo.Map.bindings |> Lwt_list.iter_s (fun (repo, project) ->
          Conv.prs snapshot ~repos:(Repo.Set.singleton repo) >>= fun prs ->
          Conv.refs snapshot ~repos:(Repo.Set.singleton repo) >>= fun refs ->
          let prs = match Repo.Map.find repo (PR.index prs) with
            | None   -> PR.Index.empty
            | Some i -> i
          in
          let refs = match Repo.Map.find repo (Ref.index refs) with
            | None   ->  Ref.Index.empty
            | Some i -> i
              in
          Log.debug (fun f -> f "Monitor iter");
          let prs, refs = apply_canaries project.canaries prs refs in
          (* PRs *)
          let is_current id open_pr =
            let current = PR.Index.mem id prs in
            if not current then (
              Log.info (fun f -> f "Removing closed PR#%d" (snd id));
              List.iter (fun j -> j.cancel ()) open_pr.jobs
            );
            current
          in
          project.open_prs <- PR.Index.filter is_current project.open_prs;
          Lwt_mutex.with_lock t.term_lock (fun () ->
              PR.Index.bindings prs |> Lwt_list.iter_s (check_pr ~snapshot project)
            )
          >>= fun () ->
          (* Refs *)
          let is_current id target =
            let current = Ref.Index.mem id refs in
            if not current then (
              Log.info (fun f -> f "Removing closed branch %a" Ref.pp_name (snd id));
              List.iter (fun j -> j.cancel ()) target.jobs
            );
            current
          in
          project.refs <- Ref.Index.filter is_current project.refs;
          Lwt_mutex.with_lock t.term_lock (fun () ->
              Ref.Index.bindings refs |> Lwt_list.iter_s (check_ref ~snapshot project)
            )
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
  t.projects |> Repo.Map.iter (fun _ project ->
      project.open_prs |> PR.Index.iter (fun _ x -> check_target x);
      project.refs |> Ref.Index.iter (fun _ x -> check_target x);
    );
  match !triggers, !jobs_needing_recalc with
  | [], [] -> CI_utils.failf "No job depends on %S, so can't rebuild anything" branch_name
  | triggers, jobs_needing_recalc ->
    Lwt.join triggers >>= fun () ->
    Lwt_mutex.with_lock t.term_lock (fun () ->
        take_snapshot t >>= fun snapshot ->
        Lwt_list.iter_s (recalculate t ~snapshot) jobs_needing_recalc
      )
