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

let start_lwt ~pr_store ~web_ui ~secrets_dir ~canaries ~config =
  let { CI_config.web_config; projects } = config in
  let dashboards = CI_projectID.Map.map (fun p -> p.CI_config.dashboards) projects in
  let projects = CI_projectID.Map.map (fun p -> p.CI_config.tests) projects in
  CI_secrets.create ~key_bits secrets_dir >>= fun secrets ->
  let github = CI_secrets.github_auth secrets in
  CI_web_utils.Auth.create ?github (CI_secrets.passwords_path secrets) >>= fun auth ->
  let (proto, addr) = pr_store in
  let connect_dk () = connect proto addr >|*= DK.connect in
  let canaries =
    match canaries with
    | [] -> None
    | canaries -> Some (CI_target.Full.map_of_list canaries)
  in
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
  let routes = CI_web.routes ~config:web_config ~logs ~auth ~ci ~dashboards in
  Lwt.pick [
    main_thread;
    CI_web_utils.serve ~routes ~mode
  ]

let start () pr_store web_ui secrets_dir config canaries =
  if Logs.Src.level src < Some Logs.Info then Logs.Src.set_level src (Some Logs.Info);
  try
    Lwt_main.run (start_lwt ~pr_store ~web_ui ~secrets_dir ~canaries ~config)
  with Failure msg ->
    Fmt.epr "Failure:@,%s@." msg;
    exit 1

(* Command-line parsing *)

open Cmdliner

let pr_store =
  let doc =
    Arg.info ~doc:"DataKit store for metadata."
      ~docv:"ADDR" ["metadata-store"]
  in
  Arg.(value (opt (pair ~sep:':' string string) ("tcp","localhost:5640") doc))

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

let default_info = Term.info "DataKitCI"

let run ?(info=default_info) config =
  let spec = Term.(const start
                   $ CI_log_reporter.setup_log
                   $ pr_store
                   $ web_ui
                   $ secrets_dir
                   $ config
                   $ canaries
                  ) in
  match Term.eval (spec, info) with
  | `Error _ -> exit 1
  | _ -> exit (if Logs.err_count () > 0 then 1 else 0)
