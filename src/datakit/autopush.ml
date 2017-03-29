open Astring
open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "DataKit"

  let push_duration_seconds =
    let help = "Time spent auto-pushing branches to remote" in
    Summary.v ~help ~namespace ~subsystem:"git" "push_duration_seconds"
end

let src = Logs.Src.create "DataKit.autopush" ~doc:"DataKit auto-push"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  local : string;                    (* Path to local repository *)
  remote : string;                   (* Remote repository *)
  mutable dirty : String.Set.t;      (* Branches to be pushed *)
  cond : unit Lwt_condition.t;       (* Fires whenever something is added to [dirty]. *)
}

let exec ~name cmd =
  Lwt_process.exec cmd >|= function
  | Unix.WEXITED 0   -> ()
  | Unix.WEXITED i   ->
    Log.err (fun l -> l "%s exited with code %d" name i)
  | Unix. WSIGNALED i ->
    Log.err (fun l -> l "%s killed by signal %d)" name i)
  | Unix.WSTOPPED i  ->
    Log.err (fun l -> l "%s stopped by signal %d" name i)

let daemon_thread t =
  let rec loop () =
    match String.Set.elements t.dirty with
    | [] ->
      Lwt_condition.wait t.cond >>= fun () ->
      (* Wait a bit in case some other branches need pushing immediately afterwards too *)
      Lwt_unix.sleep 2.0 >>= loop
    | dirty ->
      (* Note: must not block here, to ensure we don't erase new items. *)
      t.dirty <- String.Set.empty;
      Lwt.catch
        (fun () ->
           let cmd = ["git"; "-C"; t.local; "push"; "--force"; "--"; t.remote] @ dirty in
           let name = Fmt.strf "auto-push: %a" (Fmt.Dump.list String.dump) cmd in
           let t0 = Unix.gettimeofday () in
           exec ~name ("", Array.of_list cmd) >|= fun () ->
           let t1 = Unix.gettimeofday () in
           Prometheus.Summary.observe Metrics.push_duration_seconds (t1 -. t0);
        )
        (fun ex ->
           Log.err (fun l -> l "git push failed: %a" Fmt.exn ex);
           (* Should we re-queue [dirty] here? *)
           Lwt.return ()
        )
      >>= loop
  in
  loop ()

let create ~local ~remote =
  Log.info (fun l -> l "Auto-push to %s enabled" remote);
  let t = {
    local;
    remote;
    dirty = String.Set.empty;
    cond = Lwt_condition.create ();
  } in
  Lwt.async (fun () -> daemon_thread t);
  t

let push t ~branch =
  Log.info (fun l -> l "Marking %s/%s as dirty" t.local branch);
  t.dirty <- String.Set.add branch t.dirty;
  Lwt_condition.broadcast t.cond ()

