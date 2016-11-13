open CI_utils
open Lwt.Infix

module Path = struct
  (* Each entry in the cache has a branch in the database:
     - /log contains the build log
     - /failure (if present) indicates that the build failed and contains the error
     - /value may contain extra artifacts/data (depending on the builder)
     - /rebuild-requested (if present) indicates that the current results are not acceptable
   *)
  let v = Datakit_path.of_string_exn
  let log     = v "log"
  let failure = v "failure"
  let rebuild = v "rebuild-requested"
  let value   = v "value"
end

let read_log dk { CI_result.Step_log.commit; branch; _} =
  Log.debug (fun f -> f "Loading log from commit %s (branch %s)" commit branch);
  let tree = DK.Commit.tree (DK.commit dk commit) in
  DK.Tree.read_file tree Path.log >|= function
  | Ok data -> Ok (Cstruct.to_string data)
  | Error e -> Error e

module Make(B : CI_s.BUILDER) = struct
  module M = Map.Make(B.Key)

  type t = {
    logs : CI_live_log.manager;
    mutex : Lwt_mutex.t;                        (* Held while updating [cache] *)
    mutable cache: B.value CI_s.lwt_status M.t;    (* In-memory cache, including pending items *)
    builder : B.t;                              (* The underlying builder *)
  }

  let create ~logs builder =
    {
      logs;
      cache = M.empty;
      mutex = Lwt_mutex.create ();
      builder;
    }

  let needs_rebuild tree =
    DK.Tree.exists tree Path.rebuild

  let ensure_removed tr path =
    DK.Transaction.remove tr path >|= fun (Ok () | Error _) -> ()

  (* Load previously-cached results from the database, or [None] if there aren't any. *)
  let load_from_db builder ~rebuild conn key =
    conn () >>= fun dk ->
    let branch_name = B.branch builder key in
    DK.branch dk branch_name >>*= fun branch ->
    DK.Branch.head branch >>*= function
    | None -> Lwt.return None   (* Results branch doesn't exist *)
    | Some commit ->
      let tree = DK.Commit.tree commit in
      needs_rebuild tree >>*= function
      | true -> Lwt.return None (* Results exist, but are flagged for rebuild *)
      | false ->
        let title = B.title builder key in
        let logs ~failed = CI_result.Step_log.(Saved { title; commit = DK.Commit.id commit; branch = branch_name; rebuild; failed }) in
        DK.Tree.read_file tree Path.failure >>= function
        | Ok failure -> Lwt.return (Some (Error (`Failure (Cstruct.to_string failure)), logs ~failed:true))
        | Error (`Msg "No such file or directory") ->
          (* If we failed to load the failure, this result must be successful. *)
          Lwt.catch
            (fun () ->
               B.load builder tree key >|= fun v ->
               Some (Ok v, logs ~failed:false)
            )
            (fun ex ->
               Log.err (fun f -> f "Failed to load value from previously cached result %s: %s"
                           (DK.Commit.id commit)
                           (Printexc.to_string ex)
                       );
               Lwt.return None
            )
        | Error (`Msg msg) ->
          Log.err (fun f -> f "Failed to load value from previously cached result %s: %s"
                      (DK.Commit.id commit)
                      msg
                  );
          Lwt.return None

  let lookup_mem t ~rebuild k =
    match M.find k t.cache with
    | (Ok _ | Error (`Failure _)), _ when rebuild -> None (* If pending, ignore rebuild request *)
    | v -> Some v
    | exception Not_found -> None

  let mark_branch_for_rebuild builder conn key =
    conn () >>= fun dk ->
    let branch = B.branch builder key in
    DK.branch dk branch >>*= fun branch ->
    DK.Branch.with_transaction branch (fun t ->
        DK.Transaction.create_or_replace_file t Path.rebuild (Cstruct.create 0) >>*= fun () ->
        DK.Transaction.commit t ~message:"Marked for rebuild"
      )
    >>*= Lwt.return

  let catch fn =
    Lwt.catch fn
      (function
        | Failure msg -> Lwt.return (Error (`Failure msg))
        | ex -> Lwt.return (Error (`Failure (Printexc.to_string ex)))
      )

  (* Monitor the pending state of the log and keep the pending state in our hash table
     updated. Also, notify the user of the cache each time it changes.
     Finishing the log will cause this to finish too. It will then return a function to call
     to notify the user that the final result is cached (once you've added it). *)
  let monitor_pending t k log =
    let pending_log = CI_result.Step_log.Live log in
    (* Note: MUST initially set state to pending without sleeping *)
    let rec loop wake =
      let reason, update = CI_live_log.pending log in
      match update with
      | `Stop -> Lwt.return (`Done wake)
      | `Continue update ->
        (* Note: we give the user a separate [user_update] (not [update]) to ensure we
           update the cache before they check for the new status. *)
        let user_update, waker = Lwt.wait () in
        t.cache <- M.add k (Error (`Pending (reason, user_update)), pending_log) t.cache;
        wake ();
        update >>= fun () ->
        loop (fun () -> Lwt.wakeup waker ())
    in
    loop ignore

  (* We don't have a suitable cached result.
     Mark this entry as in-progress and start generating the new value.
     Must hold the lock while this is called, to insert the pending entry. *)
  let do_build t ~rebuild dk ctx k =
    let switch = Lwt_switch.create () in
    let title = B.title t.builder k in
    let branch_name = B.branch t.builder k in
    let log = CI_live_log.create ~switch ~pending:title t.logs ~branch:branch_name ~title in
    CI_live_log.heading log "%s" title;
    CI_live_log.log log "Starting...";
    let pending_thread = monitor_pending t k log in      (* Sets initial state too *)
    let finish result =
      CI_live_log.finish log;
      pending_thread >|= fun (`Done wake) ->
      t.cache <- M.add k result t.cache;
      wake ()
    in
    Lwt.async
      (fun () ->
         Lwt.catch
           (fun () ->
              (* Note: we don't hold the lock here. But that's OK; no-one else can change the entry while it's pending. *)
              DK.branch dk branch_name >>*= fun branch ->
              DK.Branch.with_transaction branch (fun trans ->
                  ensure_removed trans Path.rebuild >>= fun () ->
                  ensure_removed trans Path.value >>= fun () ->
                  DK.Transaction.create_dir trans Path.value >>*= fun () ->
                  ensure_removed trans Path.failure >>= fun () ->
                  let return x = Lwt.return (Ok x) in
                  catch (fun () -> B.generate t.builder ~switch ~log trans ctx k) >>= function
                  | Ok x ->
                    CI_live_log.log log "Success";
                    DK.Transaction.create_or_replace_file trans Path.log (Cstruct.of_string (CI_live_log.contents log)) >>*= fun () ->
                    DK.Transaction.commit trans ~message:"Cached successful result" >>*= fun () ->
                    return (Ok x)
                  | Error (`Failure msg) ->
                    CI_live_log.log log "Failed: %s" msg;
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
                let saved = { CI_result.Step_log.commit = DK.Commit.id commit; branch = branch_name; title; rebuild; failed } in
                finish ((result, CI_result.Step_log.Saved saved) :> B.value CI_s.lwt_status)
                (* At this point, the entry is no longer pending and other people can update it. *)
           )
           (fun ex ->
              let msg = Printexc.to_string ex in
              Log.err (fun f -> f "Uncaught exception in do_build: %s" msg);
              finish (Error (`Failure msg), CI_result.Step_log.Empty)
           )
      );
    M.find k t.cache

  let rec lookup t conn ~rebuild ctx k =
    match lookup_mem t ~rebuild k with
    | Some v -> Lwt.return v
    | None ->
      let do_rebuild = lazy (
        lookup t conn ~rebuild:true ctx k >|= fun (_:B.value CI_s.lwt_status) -> ()
      ) in
      Lwt_mutex.with_lock t.mutex @@ fun () ->
      conn () >>= fun dk ->
      match rebuild with
        | true ->
          mark_branch_for_rebuild t.builder conn k >|= fun () ->
          do_build t ~rebuild:do_rebuild dk ctx k
        | false ->
          (* Check cache in DB *)
          load_from_db t.builder ~rebuild:do_rebuild conn k >>= function
          | None ->
            Lwt.return (do_build t ~rebuild:do_rebuild dk ctx k)
          | Some v ->
            Log.info (fun f -> f "Loaded cached result from %s" (B.branch t.builder k));
            t.cache <- M.add k v t.cache;
            Lwt.return v

  let term t ctx k =
    let open! CI_term.Infix in
    CI_term.dk >>= fun dk ->
    CI_term.of_lwt_slow (fun () ->
        lookup t dk ~rebuild:false ctx k
      )

end

