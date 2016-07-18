open Lwt.Infix
open Astring
open Result

let src = Logs.Src.create "github-bridge" ~doc:"Github bridge for Datakit "
module Log = (val Logs.src_log src : Logs.LOG)
module Client9p = Client9p_unix.Make(Log)
module DK = Datakit_client_9p.Make(Client9p)

module VG = struct
  open Vgithub
  include Make(Vgithub_api)
  module Sync = Sync(Vgithub_api)(DK)
end

(* Hyper-V socket applications use well-known GUIDs. This is ours: *)
let serviceid = "C378280D-DA14-42C8-A24E-0DE92A1028E3"

let token () =
  let cookie = "datakit" in
  Lwt_main.run (
    Lwt.catch (fun () ->
        let open Lwt.Infix in
        Github_cookie_jar.init () >>= fun jar ->
        Github_cookie_jar.get jar ~name:cookie >|= function
        | Some t -> Some (Github.Token.of_string t.Github_t.auth_token)
        | None   -> None
      ) (fun e ->
        Log.err (fun l ->
            l "Missing cookie: use git-jar to create cookie `%s`.\n%s%!"
              cookie (Printexc.to_string e)
          );
        Lwt.return_none)
  )

let parse_address address =
  match String.cut ~sep:":" address with
  | Some (proto, address) -> proto, address
  | _ -> address, "5640"

let set_signal_if_supported signal handler =
  try
    Sys.set_signal signal handler
  with Invalid_argument _ ->
    ()

let exec ~name cmd =
  Lwt_process.exec cmd >|= function
  | Unix.WEXITED 0   -> ()
  | Unix.WEXITED i   ->
    Logs.err (fun l -> l "%s exited with code %d" name i)
  | Unix. WSIGNALED i ->
    Logs.err (fun l -> l "%s killed by signal %d)" name i)
  | Unix.WSTOPPED i  ->
    Logs.err (fun l -> l "%s stopped by signal %d" name i)

let start () sandbox listen_urls
    datakit private_branch public_branch
    no_webhook gh_hooks webhook_secret webhook_port =
  set_signal_if_supported Sys.sigpipe Sys.Signal_ignore;
  set_signal_if_supported Sys.sigterm (Sys.Signal_handle (fun _ ->
      (* On Win32 we receive this signal on every failed Hyper-V
         socket connection *)
      if Sys.os_type <> "Win32" then begin
        Log.debug (fun l -> l "Caught SIGTERM, will exit");
      end
    ));
  set_signal_if_supported Sys.sigint (Sys.Signal_handle (fun _ ->
      Log.debug (fun l -> l "Caught SIGINT, will exit");
      exit 1
    ));
  Log.app (fun l ->
      l "Starting %s %%VERSION%% ...\npublic-branch: %s\nprivate-branch: %s"
        (Filename.basename Sys.argv.(0)) public_branch private_branch
    );
  let token = match token () with
    | None   -> failwith "Missing datakit GitHub token"
    | Some t -> t
  in
  let connect_to_datakit () =
    let proto, address = parse_address datakit in
    Log.info (fun l -> l "Connecting to %s (%s, %s)" datakit proto address);
    (Lwt.catch
       (fun () -> Client9p.connect proto address ())
       (fun _ -> Lwt.fail_with
           "%s is not a valid connect adress. Use 'tcp:hostname:port' or \
            'unix:path'."))
    >>= function
    | Error (`Msg e) ->
      Log.err (fun l -> l "cannot connect: %s" e);
      Lwt.fail_with "connecting to datakit"
    | Ok conn        ->
      Log.info (fun l -> l "Connected to %s" datakit);
      let dk = DK.connect conn in
      let t = VG.Sync.empty in
      DK.branch dk private_branch >>= function
      | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
      | Ok priv ->
        DK.branch dk public_branch >>= function
        | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
        | Ok pub  -> VG.Sync.sync t ~priv ~pub ~token >|= ignore
  in
  let accept_connections () =
    if listen_urls = [] then Lwt.return_unit
    else
      let root = VG.create token in
      let make_root () = Vfs.Dir.of_list (fun () -> [root]) in
      Lwt_list.iter_p
        (Datakit_conduit.accept_forever ~make_root ~sandbox ~serviceid)
        listen_urls
  in
  let start_datakit_gh_hooks () =
    if no_webhook then Lwt.return_unit
    else
      let secret = match webhook_secret with
        | None   -> ""
        | Some s -> Fmt.strf " -s %s" s
      in
      exec ~name:"datakit-gh-hooks"
        (Lwt_process.shell @@
         Fmt.strf "%s%s -v -l :%d -b %s -a %s"
           gh_hooks secret webhook_port private_branch datakit)
  in
  Lwt_main.run @@ Lwt.join [
    connect_to_datakit ();
    accept_connections ();
    start_datakit_gh_hooks ();
  ]

open Cmdliner

let env_docs = "ENVIRONMENT VARIABLES"

let setup_log =
  let env =
    Arg.env_var ~docs:env_docs
      ~doc:"Be more or less verbose. See $(b,--verbose)."
      "DATAKIT_VERBOSE"
  in
  Term.(const Datakit_log.setup $ Fmt_cli.style_renderer ()
        $ Datakit_log.log_destination $ Logs_cli.level ~env ())

let listen_urls =
  let doc =
    Arg.info ~doc:
      "A comma-separated list of URLs to listen on of the form \
       file:///var/tmp/foo or tcp://host:port or \\\\\\\\.\\\\pipe\\\\foo \
       or hyperv-connect://vmid/serviceid or hyperv-accept://vmid/serviceid"
      ["l"; "listen"]
  in
  (* FIXME: maybe we want to not listen by default *)
  Arg.(value & opt (list string) [ "tcp://127.0.0.1:5641" ] doc)

let sandbox =
  let doc =
    Arg.info ~doc:
      "Assume we're running inside an OSX sandbox but not a chroot. \
       All paths will be manually rewritten to be relative \
       to the current directory." ["sandbox"]
  in
  Arg.(value & flag & doc)

let datakit =
  let doc = Arg.info ~doc:"The DataKit instance to connect to" ["d"; "datakit"] in
  Arg.(value & opt string "tcp:127.0.0.1:5640" doc)

let private_branch =
  let doc =
    Arg.info ~doc:"Private DataKit branch where the GitHub events (persistent \
                   and webhook) is be mirrored."
      ["x"; "branch-x"]
  in
  Arg.(value & opt string "github-metadata-x" doc)

let public_branch =
  let doc =
    Arg.info
      ~doc:"Public DataKit branch. Writes to this branch will be translated into \
            GitHub API calls."
      ["b"; "branch"]
  in
  Arg.(value & opt string "github-metadata" doc)

let no_webhook =
  let doc = Arg.info ~doc:"Disable webhook handling" ["disable-webhooks"] in
  Arg.(value & flag doc)

let gh_hooks =
  let doc =
    Arg.info ~doc:"Location of the datakit-gh-hooks webhook command"
      ["gh-hooks"]
  in
  Arg.(value & opt string "datakit-gh-hooks" doc)

let webhook_secret =
  let doc = Arg.info ~doc:"Webhook secret" ["s";"secret"] in
  Arg.(value & opt (some string) None doc)

let webhook_port =
  let doc = Arg.info ~doc:"Webhook port" ["p";"port"] in
  Arg.(value & opt int 80 doc)

let term =
  let doc = "Bridge between GiHub API and Datakit." in
  let man = [
    `S "DESCRIPTION";
    `P "$(i, datakit-github-bridge) exposes a subset of the GitHub API as a 9p \
        filesystem. Also connect to a Datakit instance and ensure a \
        bi-directional mapping between the GitHub API and a Git branch.";
  ] in
  Term.(pure start $ setup_log $ sandbox $ listen_urls $
        datakit $ private_branch $ public_branch $
        no_webhook $ gh_hooks $ webhook_secret $ webhook_port),
  Term.info (Filename.basename Sys.argv.(0)) ~version:"%%VERSION%%" ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
