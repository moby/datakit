let key_bits = 4096

open CI_utils
open! Astring
open Lwt.Infix

let logs = CI_live_log.create_manager ()

let connect protocol address =
  (* Connect to 9p server *)
  Log.info (fun f -> f "Connecting to DataKit server on %s:%s" protocol address);
  Lwt.catch
    (fun () -> Client9p.connect protocol address ())
    (fun ex ->
       failf "Failed to connect to DataKit server at proto=%S addr=%S: %s"
         protocol address (Printexc.to_string ex)
    )

let start_lwt ~pr_store ~web_ui ~secrets_dir ~canaries ~dashboards ~web_config projects =
  CI_secrets.create ~key_bits secrets_dir >>= fun secrets ->
  CI_web_utils.Auth.create (CI_secrets.passwords_path secrets) >>= fun auth ->
  let (proto, addr) = pr_store in
  let connect_dk () = connect proto addr >|*= DK.connect in
  let projects = projects
    |> List.map (fun (name, terms) -> (CI_projectID.of_string_exn name, fun () -> String.Map.of_list terms))
    |> CI_projectID.Map.of_list in
  let canaries =
    match canaries with
    | [] -> None
    | canaries -> Some (CI_target.Full.map_of_list canaries)
  in
  let dashboards = CI_target.Full.map_of_list dashboards in
  dashboards |> CI_projectID.Map.iter (fun id _ ->
      if not (CI_projectID.Map.mem id projects) then
        Log.warn (fun f -> f "Dashboard ref %a not in list of monitored projects" CI_projectID.pp id)
    );
  let ci = CI_engine.create ~web_ui ?canaries connect_dk projects in
  let main_thread = CI_engine.listen ci >|= fun `Abort -> assert false in
  let mode =
    `TLS (
      `Crt_file_path (CI_secrets.certificate_path secrets),
      `Key_file_path (CI_secrets.private_key_path secrets),
      `No_password,
      `Port 8443
    )
  in
  Lwt.pick [
    main_thread;
    CI_web.serve ~config:web_config ~logs ~auth ~mode ~ci ~dashboards;
  ]

let start ~web_config () pr_store web_ui secrets_dir projects canaries dashboards =
  if Logs.Src.level src < Some Logs.Info then Logs.Src.set_level src (Some Logs.Info);
  Lwt_main.run (start_lwt ~pr_store ~web_ui ~secrets_dir ~canaries ~dashboards ~web_config projects)

(* Command-line parsing *)

open Cmdliner

let pr_store =
  let doc =
    Arg.info ~doc:"DataKit store for metadata."
      ~docv:"ADDR" ["metadata-store"]
  in
  Arg.(required (opt (some (pair ~sep:':' string string)) None doc))

let secrets_dir =
  let doc =
    Arg.info ~doc:"Directory to store crypto secrets."
      ~docv:"DIR" ["secrets-dir"]
  in
  Arg.(value (opt Arg.dir "/secrets" doc))

let uri =
  let parse s =
    try `Ok (Uri.of_string s)
    with ex -> `Error (Printexc.to_string ex) in
  (parse, Uri.pp_hum)

let web_ui =
  let doc =
    Arg.info ~doc:"URL of main web page (for GitHub status URLs)"
      ~docv:"URL" ["web-ui"]
  in
  Arg.(value (opt uri (Uri.of_string "http://127.0.0.1:8080/") doc))

let canaries =
  let doc =
    Arg.info ~doc:"Only test these refs (e.g user/project/heads/master)"
      ~docv:"TARGET" ["canary"]
  in
  Arg.(value (opt_all CI_target.Full.arg [] doc))

let dashboards =
  let doc =
    Arg.info ~doc:"Add these refs (e.g user/project/heads/master) to the dashboard"
      ~docv:"TARGET" ["dashboard"]
  in
  Arg.(value (opt_all CI_target.Full.arg [] doc))

let run ~web_config projects =
  let spec = Term.(const (start ~web_config)
                   $ CI_log_reporter.setup_log
                   $ pr_store
                   $ web_ui
                   $ secrets_dir
                   $ projects
                   $ canaries
                   $ dashboards
                  ) in
  match Term.eval (spec, Term.info "DataKitCI") with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
