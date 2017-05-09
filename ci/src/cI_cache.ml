open CI_s
open CI_utils
open CI_utils.Infix
open! Astring
open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "DataKitCI"
  let subsystem = "cache"

  let builds_started_total =
    let help = "Total number of builds started" in
    Counter.v_label ~help ~label_name:"name" ~namespace ~subsystem "builds_started_total"

  let builds_succeeded_total =
    let help = "Total number of builds that succeeded" in
    Counter.v_label ~help ~label_name:"name" ~namespace ~subsystem "builds_succeeded_total"

  let builds_failed_total =
    let help = "Total number of builds that failed" in
    Counter.v_label ~help ~label_name:"name" ~namespace ~subsystem "builds_failed_total"

  let build_exceptions_total =
    let help = "Total number of builds that raised an exception" in
    Counter.v_label ~help ~label_name:"name" ~namespace ~subsystem "build_exceptions_total"

  let builds_in_progress =
    let help = "Number of builds in progress" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem "builds_in_progress"

  let build_time =
    let help = "Total build time" in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem "build_time"
end

let catch fn =
  Lwt.catch fn
    (function
      | Failure msg -> Lwt.return (Error (`Failure msg))
      | ex -> Lwt.return (Error (`Failure (Printexc.to_string ex)))
    )

module Path = struct
  (* Each entry in the cache has a branch in the database:
     - /log contains the build log
     - /failure (if present) indicates that the build failed and contains the error
     - /value may contain extra artifacts/data (depending on the builder)
     - /rebuild-requested (if present) indicates that the current results are not acceptable
  *)
  let v = Datakit_client.Path.of_string_exn
  let log     = v "log"
  let failure = v "failure"
  let rebuild = v "rebuild-requested"
  let value   = v "value"
end

let read_log dk { CI_output.commit; branch; _} =
  Log.debug (fun f -> f "Loading log from commit %s (branch %s)" commit branch);
  DK.commit dk commit >>*= fun commit ->
  DK.Commit.tree commit >>*= fun tree ->
  DK.Tree.read_file tree Path.log >|= function
  | Ok data -> Ok (Cstruct.to_string data)
  | Error e -> Error e

module Make(B : CI_s.BUILDER) = struct
  module Cache : sig
    type t

    val create : unit -> t
    val find : t -> string -> B.value status option
    val set : t -> string -> B.value status -> unit
    val remove : t -> string -> unit
    val with_lock : t -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  end = struct
    type entry = {
      lock : Lwt_mutex.t;
      mutable value : B.value status option;
    }

    type t = {
      mutable cache : entry String.Map.t;
    }

    let create () = { cache = String.Map.empty }

    let entry t k =
      match String.Map.find k t.cache with
      | Some e -> e
      | None ->
        let e = { lock = Lwt_mutex.create (); value = None } in
        t.cache <- String.Map.add k e t.cache;
        e

    let find t k = (entry t k).value
    let set t k value = (entry t k).value <- Some value
    let remove t k = t.cache <- String.Map.remove k t.cache

    let with_lock t k fn =
      Lwt_mutex.with_lock (entry t k).lock fn
  end

  type t = {
    logs : CI_live_log.manager;
    cache : Cache.t;            (* In-memory cache, including pending items *)
    builder : B.t;              (* The underlying builder *)
  }

  let create ~logs builder =
    {
      logs;
      cache = Cache.create ();
      builder;
    }

  let needs_rebuild tree =
    DK.Tree.exists tree Path.rebuild

  let ensure_removed tr path =
    DK.Transaction.remove tr path >|= fun (Ok () | Error _) -> ()

  let load_from_tree builder tree key =
    DK.Tree.read_file tree Path.failure >>= function
    | Ok failure -> Lwt.return @@ Ok (`Failure (Cstruct.to_string failure))
    | Error `Does_not_exist ->
      (* If we failed to load the failure, this result must be successful. *)
      catch (fun () -> B.load builder tree key >|= fun v -> Ok (`Success v))
    | Error e -> Lwt.return @@ Error (`Failure (Fmt.to_to_string DK.pp_error e))

  let mark_branch_for_rebuild conn branch =
    conn () >>= fun dk ->
    DK.branch dk branch >>*= fun branch ->
    DK.Branch.with_transaction branch (fun t ->
        DK.Transaction.create_or_replace_file t Path.rebuild (Cstruct.create 0) >>*= fun () ->
        DK.Transaction.commit t ~message:"Marked for rebuild"
      )
    >>*= Lwt.return

  let mark_for_rebuild t conn branch_name =
    Cache.with_lock t.cache branch_name @@ fun () ->
    match Cache.find t.cache branch_name with
    | Some {result = Error (`Pending _); _} -> Lwt.return ()   (* If already building, ignore rebuild request *)
    | _ ->
      mark_branch_for_rebuild conn branch_name >|= fun () ->
      Cache.remove t.cache branch_name

  (* Load previously-cached results from the database, or [None] if there aren't any. *)
  let load_from_db t conn key =
    conn () >>= fun dk ->
    let branch_name = B.branch t.builder key in
    DK.branch dk branch_name >>*= fun branch ->
    DK.Branch.head branch >>*= function
    | None -> Lwt.return None   (* Results branch doesn't exist *)
    | Some commit ->
      DK.Commit.tree commit >>*= fun tree ->
      needs_rebuild tree >>*= function
      | true -> Lwt.return None (* Results exist, but are flagged for rebuild *)
      | false ->
        let title = B.title t.builder key in
        let rebuild = `Rebuildable (lazy (mark_for_rebuild t conn branch_name)) in
        let logs ~failed = CI_output.(Saved { title; commit = DK.Commit.id commit; branch = branch_name; rebuild; failed }) in
        load_from_tree t.builder tree key >|= function
        | Ok (`Success data)   -> Some { result = Ok data; output = logs ~failed:false }
        | Ok (`Failure _ as f) -> Some { result = Error f; output = logs ~failed:true }
        | Error (`Failure msg) ->
          Log.err (fun f -> f "Failed to load value from previously cached result %s: %s" (DK.Commit.id commit) msg);
          None

  (* Monitor the pending state of the log and keep the pending state in our hash table
     updated. Also, notify the user of the cache each time it changes.
     Finishing the log will cause this to finish too. It will then return a function to call
     to notify the user that the final result is cached (once you've added it). *)
  let monitor_pending t k log =
    let pending_log = CI_output.Live log in
    (* Note: MUST initially set state to pending without sleeping *)
    let rec loop wake =
      let reason, update = CI_live_log.pending log in
      match update with
      | `Stop -> Lwt.return (`Done wake)
      | `Continue update ->
        (* Note: we give the user a separate [user_update] (not [update]) to ensure we
           update the cache before they check for the new status. *)
        let user_update, waker = Lwt.wait () in
        let r = {
          result = Error (`Pending (reason, user_update));
          output = pending_log;
        } in
        Cache.set t.cache k r;
        wake ();
        update >>= fun () ->
        loop (fun () -> Lwt.wakeup waker ())
    in
    loop ignore

  (* We don't have a suitable cached result.
     Mark this entry as in-progress and start generating the new value.
     Must hold the lock while this is called, to insert the pending entry. *)
  let do_build t conn ctx k =
    Prometheus.Counter.inc_one @@ Metrics.builds_started_total (B.name t.builder);
    let switch = Lwt_switch.create () in
    let title = B.title t.builder k in
    let branch_name = B.branch t.builder k in
    let log = CI_live_log.create ~switch ~pending:title t.logs ~branch:branch_name ~title in
    CI_live_log.heading log "%s" title;
    CI_live_log.log log "Starting...";
    let pending_thread = monitor_pending t branch_name log in      (* Sets initial state too *)
    let finish result =
      CI_live_log.finish log;
      pending_thread >|= fun (`Done wake) ->
      Cache.set t.cache branch_name result;
      wake ()
    in
    Lwt.async
      (fun () ->
         Lwt.catch
           (fun () ->
              (* Note: we don't hold the lock here. But that's OK; no-one else can change the entry while it's pending. *)
              conn () >>= fun dk ->
              DK.branch dk branch_name >>*= fun branch ->
              Prometheus.Gauge.track_inprogress (Metrics.builds_in_progress (B.name t.builder)) @@ fun () ->
              Prometheus.Summary.time (Metrics.build_time (B.name t.builder)) Unix.gettimeofday @@ fun () ->
              DK.Branch.with_transaction branch (fun trans ->
                  ensure_removed trans Path.rebuild >>= fun () ->
                  ensure_removed trans Path.value >>= fun () ->
                  DK.Transaction.create_dir trans Path.value >>*= fun () ->
                  ensure_removed trans Path.failure >>= fun () ->
                  let return x = Lwt.return (Ok x) in
                  let build () =
                    Lwt.finalize
                      (fun () -> B.generate t.builder ~switch ~log trans ctx k)
                      (fun () -> Lwt_switch.turn_off switch)
                  in
                  catch build >>= function
                  | Ok x ->
                    CI_live_log.log log "Success";
                    Prometheus.Counter.inc_one @@ Metrics.builds_succeeded_total (B.name t.builder);
                    DK.Transaction.create_or_replace_file trans Path.log (Cstruct.of_string (CI_live_log.contents log)) >>*= fun () ->
                    DK.Transaction.commit trans ~message:"Cached successful result" >>*= fun () ->
                    return (Ok x)
                  | Error (`Failure msg) ->
                    CI_live_log.log log "Failed: %s" msg;
                    Prometheus.Counter.inc_one @@ Metrics.builds_failed_total (B.name t.builder);
                    DK.Transaction.create_or_replace_file trans Path.log (Cstruct.of_string (CI_live_log.contents log)) >>*= fun () ->
                    DK.Transaction.create_file trans Path.failure (Cstruct.of_string msg) >>*= fun () ->
                    DK.Transaction.commit trans ~message:("Cached failure: " ^ msg) >>*= fun () ->
                    return (Error (`Failure msg))
                )
              >>*= fun result ->
              DK.Branch.head branch >>*= function
              | None -> failf "Branch deleted as we saved it!"
              | Some commit ->
                let failed = match result with Ok _ -> false | Error _ -> true in
                let rebuild = `Rebuildable (lazy (mark_for_rebuild t conn branch_name)) in
                let saved = { CI_output.commit = DK.Commit.id commit; branch = branch_name; title; rebuild; failed } in
                finish { result; output = CI_output.Saved saved }
                (* At this point, the entry is no longer pending and other people can update it. *)
           )
           (fun ex ->
              let msg = Printexc.to_string ex in
              Log.err (fun f -> f "Uncaught exception in do_build: %s" msg);
              Prometheus.Counter.inc_one @@ Metrics.build_exceptions_total (B.name t.builder);
              finish { result = Error (`Failure msg); output = CI_output.Empty }
           )
      );
    match Cache.find t.cache branch_name with
    | None -> assert false
    | Some v -> v

  let lookup t conn ctx k =
    let branch_name = B.branch t.builder k in
    Cache.with_lock t.cache branch_name @@ fun () ->
    match Cache.find t.cache branch_name with
    | Some v -> Lwt.return v
    | None ->
      (* Check cache in DB *)
      load_from_db t conn k >|= function
      | None -> do_build t conn ctx k
      | Some v ->
        Log.info (fun f -> f "Loaded cached result from %s" (B.branch t.builder k));
        Cache.set t.cache branch_name v;
        v

  let find t ctx k =
    let open! CI_term.Infix in
    CI_term.dk >>= fun dk ->
    CI_term.of_lwt_slow (fun () ->
        lookup t dk ctx k
      )

end
