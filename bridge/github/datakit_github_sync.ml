open Result
open Lwt.Infix
open Datakit_github
open Datakit_client.Path.Infix

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

let ( >>*= ) x f =
  x >>= function
  | Ok x         -> f x
  | Error _ as e -> Lwt.return e

let ok x = Lwt.return (Ok x)

module Make (API: API) (DK: Datakit_client.S) = struct

  module State = Datakit_github_state.Make(API)
  module Conv  = Datakit_github_conv.Make(DK)

  (*              [bridge]     [datakit]
                  [in memory Snapshot.t] [9p/datakit endpoint]
                      |            |
                  GH --events-->  |            | <--commits-- Users
                      |            |
                      | <--watch-- |
                      |            |
                  GH --API GET--> |            |
                  GH <--API SET-- |            |
                      | --write--> |
                      |            |
  *)

  type state = {
    bridge : Snapshot.t;     (* in-memory representation of the bridge state. *)
    datakit: Conv.t;                                        (* datakit state. *)
  }

  let pp_state ppf t =
    Fmt.pf ppf "@[<h2>bridge:@ %a@]@;@[<2>datakit:@ %a@]"
      Snapshot.pp t.bridge Conv.pp t.datakit

  let is_open tr = DK.Transaction.closed tr = false
  let is_closed tr = DK.Transaction.closed tr

  let create ~debug ?old br =
    let bridge = match old with
      | None   -> Snapshot.empty
      | Some o -> o.bridge
    in
    let old = match old with
      | None   -> None
      | Some o -> Some o.datakit
    in
    Conv.of_branch ~debug ?old br >|= fun (tr, datakit) ->
    tr, { datakit; bridge }

  let safe_abort tr =
    if DK.Transaction.closed tr then Lwt.return (Ok ())
    else DK.Transaction.abort tr

  let rec safe_commit ?(retry=5) tr ~message =
    DK.Transaction.commit tr ~message >>= function
    | Ok ()   -> Lwt.return true
    | Error e ->
      if retry <> 0 then safe_commit ~retry:(retry-1) tr ~message
      else (
        Log.info (fun l -> l "Abort: %a" DK.pp_error e);
        DK.Transaction.abort tr >|= fun _ ->
        false
      )

  (* Create and init [br] if it doesn't exist. *)
  let init_sync br =
    Log.debug (fun l -> l "init_sync %s" @@ DK.Branch.name br);
    let init =
      DK.Branch.head br  >>*= function
      | Some _ -> ok ()
      | None   ->
        DK.Branch.with_transaction br (fun tr ->
            let file = Datakit_client.Path.(empty / "README.md") in
            let data = Cstruct.of_string "### DataKit -- GitHub bridge\n" in
            DK.Transaction.create_or_replace_file tr file data
            >>= function
            | Ok ()   -> DK.Transaction.commit tr ~message:"Initial commit"
            | Error e ->
              DK.Transaction.abort tr >>*= fun () ->
              Lwt.fail_with @@ Fmt.strf "init_sync: %a" DK.pp_error e
          )
    in
    init >>= function
    | Ok () -> Lwt.return_unit
    | Error e ->
      Log.err (fun l -> l "init_sync: %a" DK.pp_error e);
      Lwt.fail_with "init_sync"

  type webhook = {
    watch : Repo.t -> unit Lwt.t;
    events: unit -> Event.t list Lwt.t;
  }

  let (>|*=) x f =
    x >>= function
    | Ok x    -> Lwt.return (f x)
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" DK.pp_error e

  let commit t tr =
    let diff = Snapshot.diff t.bridge (Conv.snapshot t.datakit) in
    let dirty = Conv.dirty t.datakit in
    if Diff.is_empty diff && Elt.IdSet.is_empty dirty then
      safe_abort tr >|*= fun () -> true
    else if not (Elt.IdSet.is_empty dirty) then
      let message = "Cleaning up .dirty files" in
      safe_commit tr ~message
    else (
      let message = Diff.commit_message diff in
      Conv.apply ~debug:"commit" diff tr >>= fun updated ->
      if updated then safe_commit tr ~message
      else safe_abort tr >|*= fun () -> true
    )

  let process_webhooks ~token ~webhook t repos = match webhook with
    | None                   -> Lwt.return t.bridge
    | Some { watch; events } ->
      State.add_webhooks token ~watch repos >>= fun () ->
      State.import_webhook_events token ~events t.bridge

  let update_datakit ?(retries=5) t tr =
    let rec aux n =
      assert (is_open tr);
      commit t tr >>= fun commited ->
      assert (is_closed tr);
      if commited then Lwt.return t
      else (
        Log.info (fun l -> l "Cannot commit, retry sync (%d/%d)" n retries);
        if n = retries then aux (n+1)
        else Lwt.return t
      ) in
    aux 1

  let call_github_api ~token ~first_sync t =
    let caps = State.capabilities token in
    let datakit = Conv.snapshot t.datakit in
    let op = if first_sync then `Excl else `Write in
    let full_diff = Snapshot.diff datakit t.bridge in
    let diff = Capabilities.filter_diff caps op full_diff in
    Log.debug
      (fun l -> l "call_github_api: %a %a" Diff.pp full_diff Diff.pp diff);
    State.apply token diff >|= fun () ->
    let bridge = Diff.apply diff t.bridge in
    { t with bridge }

  let sync ~token ~webhook ~first_sync ~resync t repos tr =
    process_webhooks ~token ~webhook t repos >>= fun bridge ->
    let dirty = Conv.dirty t.datakit in
    let to_import =
      let (++) = Elt.IdSet.union in
      let empty = Elt.IdSet.empty in
      let of_repos s =
        Repo.Set.fold (fun r -> Elt.IdSet.add (`Repo r)) s empty
      in
      of_repos repos
      ++ (if resync then of_repos (Snapshot.repos bridge) else empty)
      ++ dirty
    in
    State.import token bridge to_import >>= fun bridge ->
    let t = { t with bridge } in
    call_github_api ~token ~first_sync t >>= fun t ->
    (* FIXME: we should be able to configure if we want to
       prune or not. *)
    let t = { t with bridge = Snapshot.prune t.bridge } in
    Conv.clean tr dirty >>= fun () ->
    update_datakit t tr

  (* On startup, build the initial state by looking at the active
     repository in datakit. Import the new repositories and call the
     GitHub API with the diff between the GitHub state and datakit. *)
  let first_sync ~token ~webhook ~resync br =
    create ~debug:"first-sync" ?old:None br >>= fun (tr, t) ->
    Log.debug (fun l -> l "[first_sync]@ %a" pp_state t);
    let repos = Snapshot.repos (Conv.snapshot t.datakit) in
    if Repo.Set.is_empty repos then safe_abort tr >|= fun _ -> t
    else sync ~token ~webhook ~first_sync:true ~resync t repos tr

  (* The main synchonisation function: it is called on every change in
     the datakit branch and when new webhook events are received. *)
  let sync_once ~token ~webhook ~resync old br =
    create ~debug:"sync-once" ~old br >>= fun (tr, t) ->
    Log.debug (fun l -> l "[sync_once]@;old:%a@;new:%a" pp_state old pp_state t);
    let repos =
      Repo.Set.diff
        (Snapshot.repos @@ Conv.snapshot t.datakit)
        (Snapshot.repos t.bridge)
    in
    sync ~token ~webhook ~first_sync:false ~resync t repos tr

  type t = State of state | Starting

  let empty = Starting

  let continue = function
    | Some s -> Lwt_switch.is_on s
    | None   -> true

  let process_webhook = function
    | None   -> None, fun _ -> fst (Lwt.task ())
    | Some w ->
      let watch r = API.Webhook.watch w r in
      let events () =
        let e = API.Webhook.events w in
        API.Webhook.clear w;
        e
      in
      let rec wait s =
        API.Webhook.wait w >>= fun () ->
        s ();
        wait s
      in
      let run s =
        Lwt.pick [
          API.Webhook.run w;
          wait s
        ]
      in
      Some {watch; events}, run

  let run ~webhook ?resync_interval ?switch ~token br t policy =
    let webhook, run_webhook = process_webhook webhook in
    let sync_once = function
      | Starting -> first_sync ~token ~webhook br
      | State t  -> sync_once ~token ~webhook t br
    in
    match policy with
    | `Once   -> sync_once ~resync:false t >|= fun t -> State t
    | `Repeat ->
      let t = ref t in
      let updates = ref `None in
      let cond = Lwt_condition.create () in
      let pp ppf = function
        | Starting -> Fmt.string ppf "<starting>"
        | State t  ->
          let repos = Snapshot.repos t.bridge in
          Fmt.pf ppf "active repos: %a" Repo.Set.pp repos
      in
      let timer () = match resync_interval with
        | None   -> let t, _ = Lwt.task () in t
        | Some s ->
          let rec loop_for_ever () =
            (* FIXME: hanlde cancellation when [switch] if off. *)
            Lwt_unix.sleep s >>= fun () ->
            Log.debug (fun l -> l "polling: waking-up");
            updates := `Timer;
            Lwt_condition.signal cond ();
            loop_for_ever ()
          in
          loop_for_ever ()
      in
      let rec react () =
        if not (continue switch) then Lwt.return_unit
        else
          (if !updates = `None then Lwt_condition.wait cond else Lwt.return_unit)
          >>= fun () ->
          let u = !updates in
          updates := `None;
          Log.info (fun l -> l "Processing new entry -- %a" pp !t);
          Lwt.catch
            (fun () ->
               sync_once ~resync:(u = `Timer) !t >|= fun s ->
               t := State s)
            (fun e ->
               Log.err (fun l -> l "error: %s" (Printexc.to_string e));
               Lwt.return_unit)
          >>=
          react
      in
      let webhook () =
        run_webhook (fun () ->
            Log.debug (fun l -> l "webhook event received!");
            updates := `Webhook;
            Lwt_condition.signal cond ()
          )
      in
      let watch br =
        let notify _ =
          Log.info (fun l -> l "Change detected in %s" @@ DK.Branch.name br);
          updates := `Datakit;
          Lwt_condition.signal cond ();
          ok `Again
        in
        DK.Branch.wait_for_head ?switch br notify >>= function
        | Ok _    -> Lwt.return_unit
        | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
      in
      Lwt.choose [ react () ; timer (); watch br; webhook () ]
      >|= fun () ->
      !t

  let sync ~token ?webhook ?resync_interval ?switch ?(policy=`Repeat)
      ?(cap=Capabilities.all) br t =
    Log.debug (fun l -> l "[sync] %s" @@ DK.Branch.name br);
    let token = State.token token cap in
    init_sync br >>= fun () ->
    run ~webhook ?switch ?resync_interval ~token br t policy

end
