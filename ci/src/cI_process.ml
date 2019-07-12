open! Astring
open CI_utils
open Lwt.Infix

let child_src =
  Logs.Src.create "datakit-ci.child" ~doc:"Output from child process"

module Child = (val Logs.src_log child_src : Logs.LOG)

let pp_args =
  let sep = Fmt.(const string) " " in
  Fmt.array ~sep String.dump

let pp_cmd f = function
  | "", args -> pp_args f args
  | bin, args -> Fmt.pf f "(%S, %a)" bin pp_args args

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let check_status cmd = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED x -> failf "Command %a exited with status %d" pp_cmd cmd x
  | Unix.WSIGNALED x -> failf "Command %a failed with signal %d" pp_cmd cmd x
  | Unix.WSTOPPED x ->
      failf "Command %a stopped with signal %a" pp_cmd cmd pp_signal x

let open_in ?cwd ?env ?stdin ?stderr cmd =
  match cwd with
  | None -> Lwt.return (Lwt_process.open_process_in ?env ?stdin ?stderr cmd)
  | Some dir ->
      Lwt_mutex.with_lock chdir_lock (fun () ->
          Sys.chdir dir;
          let child = Lwt_process.open_process_in ?env ?stdin ?stderr cmd in
          (* Currently, the process is created before [open_process_in] returns and
           we can therefore release [chdir_lock].
           See: http://stackoverflow.com/questions/30862802/how-to-correctly-start-a-process-from-a-specific-directory-with-lwt
           and  https://github.com/ocsigen/lwt/issues/163 *)
          Sys.chdir "/";

          (* Just to detect problems with this scheme early *)
          Lwt.return child)

(* Copy to [dst] until end-of-file or [switch] is turned off. *)
let copy ?switch ~dst stream =
  let rec aux () =
    (* (passing a count tells it not to buffer) *)
    Lwt_io.read ~count:4096 stream >>= function
    | "" -> Lwt.return `Eof
    | data ->
        dst data;
        Child.debug (fun f -> f "%S" data);

        (* Hack because child#terminate may not kill sub-children.
         Hopefully closing [stream] will encourage them to exit. *)
        Lwt_switch.check switch;
        aux ()
  in
  aux ()

let run_with_exit_status ?switch ?log ?cwd ?env ?stdin ~output ?stderr ?log_cmd
    cmd =
  let log_cmd = CI_utils.default cmd log_cmd in
  let info fmt =
    fmt
    |> Format.kasprintf @@ fun msg ->
       Log.info (fun f -> f "%s" msg);
       match log with None -> () | Some log -> CI_live_log.log log "%s" msg
  in
  info "Running %a" pp_cmd log_cmd;
  let is_running = ref true in
  let stderr_r, stderr_w =
    match stderr with
    | None -> (None, `FD_copy Unix.stdout)
    | Some stderr ->
        let stderr_r, stderr_w = Unix.pipe () in
        let stderr_r = Lwt_io.of_unix_fd ~mode:Lwt_io.input stderr_r in
        (Some (stderr, stderr_r), `FD_move stderr_w)
  in
  Lwt.finalize
    (fun () ->
      open_in ?cwd ?env ?stdin ~stderr:stderr_w cmd >>= fun child ->
      Lwt.catch
        (fun () ->
          Lwt.try_bind
            (fun () ->
              Lwt_switch.add_hook_or_exec switch (fun () ->
                  if !is_running then (
                    info "Switch was turned off, so killing process %a" pp_cmd
                      log_cmd;
                    child#terminate );
                  Lwt.return ())
              >>= fun () ->
              let copy_stdout = copy ?switch ~dst:output child#stdout in
              match stderr_r with
              | None -> copy_stdout (* Also includes stderr *)
              | Some (dst, stderr_r) ->
                  let copy_stderr = copy ?switch ~dst stderr_r in
                  (* Wait for both streams to finish *)
                  copy_stdout >>= fun `Eof ->
                  copy_stderr)
            (fun `Eof ->
              child#close >|= fun stat ->
              is_running := false;
              stat)
            (fun ex ->
              child#close >>= fun _stat ->
              is_running := false;
              Lwt.fail ex))
        (fun ex ->
          info "Error %s from %a" (Printexc.to_string ex) pp_cmd log_cmd;
          Lwt.fail ex))
    (fun () ->
      match stderr_r with
      | None -> Lwt.return ()
      | Some (_, c) -> Lwt_io.close c)

let run ?switch ?log ?cwd ?env ?stdin ~output ?stderr ?log_cmd cmd =
  run_with_exit_status ?switch ?log ?cwd ?env ?stdin ~output ?stderr ?log_cmd
    cmd
  >|= fun status ->
  match switch with
  | Some switch when not (Lwt_switch.is_on switch) -> failf "Cancelled"
  | _ -> check_status (CI_utils.default cmd log_cmd) status
