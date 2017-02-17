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

  mutable state : string * string CI_output.t option;
  (* The last result of evaluating [term] (src_commit, history_commit) *)
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
  mutable targets_of_commit : CI_target.t list String.Map.t;
}

type t = {
  web_ui : Uri.t;
  connect_dk : unit -> DK.t Lwt.t;
  projects : project Repo.Map.t;
  history : CI_history.t;
  mutable dk : DK.t Lwt.t;
  recalculate : unit Lwt_condition.t;   (* Fires when [snapshot] changes or a rebuild is triggered. *)
}

let dk t = t.dk

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
          targets_of_commit = String.Map.empty;
          canaries }
      ) projects
  in
  let dk = connect connect_dk in
  {
    web_ui;
    connect_dk;
    dk;
    projects;
    history = CI_history.create ();
    recalculate = Lwt_condition.create ();
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
      | Some c -> fn c >>= fun () -> ok `Again
    )
  >|*= function
  | `Abort -> `Abort
  | `Finish `Never -> assert false

let datakit_ci x = ["ci"; "datakit"; x]

let set_status t target name result =
  let status = CI_result.status result in
  let descr = CI_result.descr result in
  Prometheus.Counter.inc_one Metrics.status_updates;
  Log.info (fun f -> f "Job %a:%s -> %s" CI_target.pp_v target name descr);
  let commit = CI_target.head target in
  let { Repo.user; repo } = CI_target.repo_v target in
  let hash = Commit.hash commit in
  let url = Uri.with_path t.web_ui (Fmt.strf "/%s/%s/commit/%s" user repo hash) in
  let message =
    Fmt.strf "Set state of %a: %s = %a"
      Commit.pp_hash hash
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

let recalculate t ~snapshot job =
  Log.debug (fun f -> f "Recalculate %a" pp_job job);
  (* Need to avoid either recalculating the same term twice at the same time,
     or doing a second calculation with an earlier snapshot. *)
  Lwt_mutex.with_lock job.term_lock @@ fun () ->
  let recalc () = Lwt_condition.broadcast t.recalculate () in
  job.cancel ();        (* Stop any previous evaluation *)
  let head = job.parent.v in
  Lwt.catch
    (fun () ->
       let r, cancel =
         CI_term.run ~snapshot:(DK.Commit.tree snapshot) ~job_id:(job_id job) ~recalc ~dk:(fun () -> t.dk)
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
  t.dk >>= fun dk ->
  CI_history.lookup t.history dk job.parent.v >>= fun history ->
  CI_history.record history dk job.name snapshot new_output >>= fun () ->
  let new_hash = Commit.hash (CI_target.head head) in
  let new_result = CI_output.result new_output in
  begin match old_output with
  | Some old_commit when (old_head, CI_output.result old_commit) = (new_hash, new_result) -> Lwt.return ()
  | _ -> set_status t head job.name new_result
  end >|= fun () ->
  job.state <- (new_hash, Some new_output)

let make_job t ~parent name term =
  let head_commit = CI_target.head parent.v in
  t.dk >>= fun dk ->
  CI_history.lookup t.history dk parent.v >>= fun history ->
  let history =
    match CI_history.head history with
    | None -> None
    | Some head -> String.Map.find name (CI_history.jobs head)
  in
  let hash = Commit.hash head_commit in
  Lwt.return {
    name;
    parent;
    term_lock = Lwt_mutex.create ();
    term;
    cancel = ignore;
    state = (hash, history);
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

let index_targets ~prs ~refs =
  let targets_of_commit = ref String.Map.empty in
  let add commit target =
    let hash = Datakit_github.Commit.hash commit in
    let existing = String.Map.find hash !targets_of_commit |> CI_utils.default [] in
    let targets = target :: existing in
    targets_of_commit := String.Map.add hash targets !targets_of_commit
  in
  refs |> Ref.Index.iter (fun id x -> add (Datakit_github.Ref.commit x) (`Ref id));
  prs  |> PR.Index.iter  (fun id x -> add (Datakit_github.PR.commit x)  (`PR id));
  !targets_of_commit

(* A thread that rebuilds after [t.recalculate] is triggered. *)
let recalc_loop t ~snapshot_ref =
  let pool_size = 50 in
  let pool = Pool.create pool_size in
  let check_pr ~snapshot project (id, pr) =
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
    Lwt_list.iter_p (recalculate t ~snapshot) open_pr.jobs
  in
  let check_ref ~snapshot project (id, r) =
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
    Lwt_list.iter_p (recalculate t ~snapshot) target.jobs
  in
  let rec loop () =
    let recalc_needed = Lwt_condition.wait t.recalculate in
    match !snapshot_ref with
    | None ->
      Log.info (fun f -> f "recalc_thread got abort request");
      Lwt.return `Abort
    | Some snapshot ->
      let active_tags = ref 0 in
      let active_braches = ref 0 in
      let active_prs = ref 0 in

      t.projects |> Repo.Map.bindings |> Lwt_list.iter_p (fun (repo, project) ->
          Log.debug (fun f -> f "Monitor iter");
          let snapshot_tree = DK.Commit.tree snapshot in
          Conv.prs snapshot_tree ~repos:(Repo.Set.singleton repo) >>= fun prs ->
          Conv.refs snapshot_tree ~repos:(Repo.Set.singleton repo) >>= fun refs ->
          let prs = match Repo.Map.find repo (PR.index prs) with
            | None   -> PR.Index.empty
            | Some i -> i
          in
          let refs = match Repo.Map.find repo (Ref.index refs) with
            | None   ->  Ref.Index.empty
            | Some i -> i
          in
          project.targets_of_commit <- index_targets ~prs ~refs;
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
              check_pr ~snapshot project pr
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
              check_ref ~snapshot project r
            )
        )
      >>= fun () ->
      Metrics.set_active_targets `Tag !active_tags;
      Metrics.set_active_targets `Branch !active_braches;
      Metrics.set_active_targets `PR !active_prs;
      Pool.wait pool >>= fun () ->
      (* Wait until something changes (which might already have happened) *)
      recalc_needed >>= loop
  in
  loop ()

let listen ?switch t =
  Log.info (fun f -> f "Starting monitor loop");
  let snapshot_ref = ref None in
  let ready = Lwt_condition.wait t.recalculate in
  let recalc_thread =
    ready >>= fun () ->       (* Wait for [snapshot_ref] *)
    auto_restart t ?switch "recalc" @@ fun () ->
    recalc_loop t ~snapshot_ref in
  enable_monitoring t (List.map fst (Repo.Map.bindings t.projects)) >>= fun () ->
  let monitor_thread =
    auto_restart t ?switch "monitor" @@ fun () ->
    monitor ?switch t (fun snapshot ->
        Prometheus.Counter.inc_one Metrics.update_notifications;
        snapshot_ref := Some snapshot;
        Lwt_condition.broadcast t.recalculate ();
        Lwt.return ()
      )
    >>= fun `Abort ->
    snapshot_ref := None;
    Lwt_condition.broadcast t.recalculate ();    (* Ask [recalc_thread] to stop. *)
    Log.info (fun f -> f "Monitor thread done; waiting for recalc_thread to finish");
    recalc_thread
  in
  Lwt.choose [recalc_thread; monitor_thread]

let rebuild t ~branch_name =
  let triggers = ref [] in
  let rec check_logs =
    let open CI_output in
    function
    | Saved {branch; rebuild; _} when branch = branch_name ->
      if not (Lazy.is_val rebuild) then triggers := Lazy.force rebuild :: !triggers
    | Pair (a, b) -> check_logs a; check_logs b
    | Empty
    | Saved _
    | Live _ -> ()
  in
  let check_job job =
    match job.state with
    | _, Some state -> check_logs (CI_output.logs state)
    | _ -> ()
  in
  let check_target target = List.iter check_job target.jobs in
  t.projects |> Repo.Map.iter (fun _ project ->
      project.open_prs |> PR.Index.iter (fun _ x -> check_target x);
      project.refs |> Ref.Index.iter (fun _ x -> check_target x);
    );
  match !triggers with
  | [] -> CI_utils.failf "No job depends on %S, so can't rebuild anything" branch_name
  | triggers ->
    Lwt.join triggers >|= fun () ->
    Lwt_condition.broadcast t.recalculate ()

let targets_of_commit t repo c =
  match Repo.Map.find repo t.projects with
  | None -> []
  | Some p -> String.Map.find c p.targets_of_commit |> CI_utils.default []

let latest_state t target =
  t.dk >>= fun dk ->
  CI_history.lookup t.history dk target.v >|= CI_history.head
