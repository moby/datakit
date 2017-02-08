open Datakit_github
open CI_utils
open CI_utils.Infix
open Result
open! Astring
open Lwt.Infix
module Conv = Datakit_github_conv.Make(DK)

module Metrics = struct
  let namespace = "DataKitCI"
  let subsystem = "engine"

  let connection_attempts =
    let help = "Number of attempted connections to DataKit" in
    Prometheus.Counter.v ~help ~namespace ~subsystem "connection_attempts_total"

  let connection_failures =
    let help = "Number of failed attempts to connect to DataKit" in
    Prometheus.Counter.v ~help ~namespace ~subsystem "connection_failures_total"

  let status_updates =
    let help = "Number of status updates" in
    Prometheus.Counter.v ~help ~namespace ~subsystem "status_updates_total"

  let update_notifications =
    let help = "Number of notifications from DataKit" in
    Prometheus.Counter.v ~help ~namespace ~subsystem "update_notifications_total"

  let set_active_targets =
    let help = "Number of branches, tags and PRs currently being monitored" in
    let g = Prometheus.Gauge.v_label ~label_name:"type" ~help ~namespace ~subsystem "active_targets" in
    let tags = g "tag" in
    let branches = g "branch" in
    let prs = g "pr" in
    let set guage v = Prometheus.Gauge.set guage (float_of_int v) in
    function
    | `Tag    -> set tags
    | `Branch -> set branches
    | `PR     -> set prs
end

type job = {
  name : string;
  parent : target;
  term_lock : Lwt_mutex.t;         (* Held while evaluating term *)
  mutable term : string CI_term.t;
  mutable cancel : unit -> unit;   (* Cancel the previous evaluation, if any *)
  mutable state : string * string CI_output.t;  (* The last result of evaluating [term] (commit, state) *)
}
and target = {
  mutable v : CI_target.v;
  mutable jobs : job list;      (* (only mutable for init) *)
}

let job_id job =
  let target = match job.parent.v with
    | `PR pr -> `PR (PR.id pr)
    | `Ref r -> `Ref (Ref.id r)
  in
  target, job.name

let pp_target f target =
  match target.v with
  | `PR pr -> Fmt.pf f "%a/pr/%d" Repo.pp (PR.repo pr) (PR.number pr)
  | `Ref r -> Fmt.pf f "%a/ref/%a" Repo.pp (Ref.repo r) Ref.pp_name (Ref.name r)

let repo t = match t.v with
  | `PR r  -> PR.repo r
  | `Ref r -> Ref.repo r

let pp_job f j =
  Fmt.pf f "%a:%s" pp_target j.parent j.name

let state job = snd job.state
let target job = job.v

let title pr =
  match pr.v with
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
  mutable dk : DK.t Lwt.t;
  mutable snapshot : DK.Tree.t option;
}

let dk t = t.dk

let snapshot t =
  match t.snapshot with
  | None -> CI_utils.failf "CI engine not yet initialised!"
  | Some s -> s

let rec connect connect_dk =
  Lwt.catch
    (fun () ->
       Prometheus.Counter.inc_one Metrics.connection_attempts;
       connect_dk ()
    )
    (fun ex ->
       Prometheus.Counter.inc_one Metrics.connection_failures;
       Log.warn (fun f -> f "Failed to connect to DataKit: %s (will retry in 10s)" (Printexc.to_string ex));
       Lwt_unix.sleep 10.0 >>= fun () ->
       connect connect_dk
    )

let metadata_branch = "github-metadata"

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
  let dk = connect connect_dk in
  {
    web_ui;
    connect_dk;
    dk;
    projects;
    snapshot = None;
  }

let prs t = t.projects |> Repo.Map.map (fun project -> project.open_prs)
let refs t = t.projects |> Repo.Map.map (fun project -> project.refs)

let update_status_lock = Lwt_mutex.create ()
let update_status t ~message s =
  t.dk >>= fun t ->
  DK.branch t metadata_branch >>*= fun metadata ->
  (* Because multiple targets can be pointing to the same commit, we may try
     to update a single commit's status for several targets in parallel,
     leading to merge conflicts. This is a limitation of GitHub's design, so
     just let the most recent update to win. *)
  Lwt_mutex.with_lock update_status_lock @@ fun () ->
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

let set_status t target name result =
  let status = CI_result.status result in
  let descr = CI_result.descr result in
  Prometheus.Counter.inc_one Metrics.status_updates;
  let commit, url =
    match target with
    | `PR pr ->
      Log.info (fun f -> f "Job %a:%s -> %s" PR.pp_id (PR.id pr) name descr);
      let url = Uri.with_path t.web_ui (CI_target.path_v target) in
      let commit = PR.commit pr in
      (commit, url)
    | `Ref r ->
      Log.info (fun f -> f "Job ref %a:%s -> %s" Ref.pp_id (Ref.id r) name descr);
      let url = Uri.with_path t.web_ui (CI_target.path_v target) in
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
  Lwt.catch
    (fun () -> update_status t ~message status)
    (fun ex ->
       (* Most likely the bridge has deleted the commit because the target was deleted.
          Ideally we'd get the commit it tried to merge with and check, but for now just
          log a warning. *)
       Log.warn (fun f -> f "Failed to update status of %a: %a" CI_target.pp_v target CI_utils.pp_exn ex);
       Lwt.return ()
    )


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
         | Error err ->
           Log.warn (fun f -> f "%s: database connection failed: %a\n(probable cause of %s)"
                        label DK.pp_error err (Printexc.to_string ex));
           reconnect t;
           auto_restart t label fn
    )

let rec recalculate t job =
  Log.debug (fun f -> f "Recalculate %a" pp_job job);
  (* Need to avoid either recalculating the same term twice at the same time,
     or doing a second calculation with an earlier snapshot. *)
  Lwt_mutex.with_lock job.term_lock @@ fun () ->
  let snapshot = snapshot t in
  let recalc () =
    Lwt.async (fun () ->
        recalculate t job
      )
  in
  job.cancel ();        (* Stop any previous evaluation *)
  let head = job.parent.v in
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
        Lwt.return (Error (`Failure msg), CI_output.Empty)
      | ex ->
        Lwt.return (Error (`Failure (Printexc.to_string ex)), CI_output.Empty)
    )
  >>= fun new_output ->
  let (old_head, old_output) = job.state in
  let new_hash = Commit.hash (CI_target.head head) in
  let old_result = CI_output.result old_output in
  let new_result = CI_output.result new_output in
  begin if (old_head, old_result) <> (new_hash, new_result) then (
      set_status t head job.name new_result
    ) else (
      Lwt.return ()
    )
  end >|= fun () ->
  job.state <- (new_hash, new_output)

let make_job t ~parent name term =
  let snapshot = snapshot t in
  let head_commit = CI_target.head parent.v in
  let id = head_commit, datakit_ci name in
  Conv.status snapshot id >|= fun status ->
  let state = match status with None -> None | Some s -> Some (Status.state s) in
  let descr = match status with None -> None | Some s -> Status.description s in
  let result =
    match state, descr with
    | Some `Error, Some descr -> CI_result.v `Failure descr
    | Some (`Pending | `Success | `Failure as status), Some descr -> CI_result.v status descr
    | _ -> Error (`Pending "(new)")
  in
  let state = (result, CI_output.Empty) in
  let hash = Commit.hash head_commit in
  { name;
    parent;
    term_lock = Lwt_mutex.create ();
    term;
    cancel = ignore;
    state = (hash, state);
  }

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

let is_tag = function
  | _, "tags" :: _ ->  true
  | _ -> false

module Pool : sig
  type t

  val create : int -> t

  val iter : t -> ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
  (** [iter t fn xs] iterates over [xs], starting [f x] for each element.
      If the pool is full, it waits before starting new jobs.
      It does not wait for the jobs to complete. *)

  val wait : t -> unit Lwt.t
  (** [wait t] waits for all jobs to complete. *)
end = struct
  type t = {
    cond : unit Lwt_condition.t;
    mutable free : int;
    mutable outstanding : int;
  }

  let create free = { free; outstanding = 0; cond = Lwt_condition.create () }

  let rec push t f =
    match t.free with
    | 0 -> Lwt_condition.wait t.cond >>= fun () -> push t f
    | free ->
      t.free <- free - 1;
      t.outstanding <- t.outstanding + 1;
      Lwt.async (fun () ->
          Lwt.finalize f
            (fun () ->
               t.outstanding <- t.outstanding - 1;
               t.free <- t.free + 1;
               Lwt_condition.broadcast t.cond ();
               Lwt.return_unit
            )
        );
      Lwt.return_unit

  let iter t f =
    Lwt_list.iter_s (fun x -> push t (fun () -> f x))

  let rec wait t =
    if t.outstanding = 0 then Lwt.return_unit
    else Lwt_condition.wait t.cond >>= fun () -> wait t
end

let listen ?switch t =
  auto_restart t ?switch "monitor" @@ fun () ->
  let pool_size = 50 in
  let pool = Pool.create pool_size in
  Log.info (fun f -> f "Starting monitor loop");
  let check_pr project (id, pr) =
    Log.debug (fun f -> f "Checking for work on %a" PR.pp_id id);
    begin match PR.Index.find id project.open_prs with
      | None ->
        let open_pr = { v = `PR pr; jobs = [] } in
        let terms = project.make_terms (`PR id) in
        String.Map.bindings terms
        |> Lwt_list.map_s (fun (name, term) ->
            make_job t ~parent:open_pr name term)
        >>= fun jobs ->
        open_pr.jobs <- jobs;
        project.open_prs <- PR.Index.add id open_pr project.open_prs;
        Lwt.return open_pr
      | Some open_pr ->
        open_pr.v <- `PR pr; (* Update in all cases, because we read other things from the same snapshot.
                                   XXX: so compare is very misleading here! *)
        Lwt.return open_pr
    end >>= fun open_pr ->
    Lwt_list.iter_p (recalculate t) open_pr.jobs
  in
  let check_ref project (id, r) =
    Log.debug (fun f -> f "Checking for work on %a" Ref.pp_id id);
    begin match Ref.Index.find id project.refs with
      | None ->
        let target = { v = `Ref r; jobs = []; } in
        let terms = project.make_terms @@ `Ref id in
        String.Map.bindings terms
        |> Lwt_list.map_s (fun (name, term) ->
            make_job t ~parent:target name term)
        >>= fun jobs ->
        target.jobs <- jobs;
        project.refs <- Ref.Index.add id target project.refs;
        Lwt.return target
      | Some target ->
        target.v <- `Ref r;
        Lwt.return target
    end >>= fun target ->
    Lwt_list.iter_p (recalculate t) target.jobs
  in
  enable_monitoring t (List.map fst (Repo.Map.bindings t.projects)) >>= fun () ->
  monitor ?switch t (fun snapshot ->
      t.snapshot <- Some snapshot;
      Prometheus.Counter.inc_one Metrics.update_notifications;
      let active_tags = ref 0 in
      let active_braches = ref 0 in
      let active_prs = ref 0 in

      t.projects |> Repo.Map.bindings |> Lwt_list.iter_p (fun (repo, project) ->
          Log.debug (fun f -> f "Monitor iter");
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
          PR.Index.bindings prs |> Pool.iter pool (fun pr ->
              incr active_prs;
              check_pr project pr
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
          Ref.Index.bindings refs |> Pool.iter pool (fun r ->
              if is_tag (fst r) then incr active_tags else incr active_braches;
              check_ref project r
            )
        )
      >>= fun () ->
      Metrics.set_active_targets `Tag !active_tags;
      Metrics.set_active_targets `Branch !active_braches;
      Metrics.set_active_targets `PR !active_prs;
      Pool.wait pool
    )

let rebuild t ~branch_name =
  let jobs_needing_recalc = ref [] in
  let triggers = ref [] in
  let rec check_logs =
    let open CI_output in
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
    if check_logs (CI_output.logs state) then
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
    Lwt_list.iter_s (recalculate t) jobs_needing_recalc
