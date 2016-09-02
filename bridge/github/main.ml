open Lwt.Infix
open Astring
open Result

let src = Logs.Src.create "gh-bridge" ~doc:"Github bridge for Datakit "
module Log = (val Logs.src_log src : Logs.LOG)

let src9p = Logs.Src.create "g9p" ~doc:"Github bridge for Datakit (9p) "
module Log9p = (val Logs.src_log src9p : Logs.LOG)

let quiet_9p () =
  Logs.Src.set_level src9p (Some Logs.Info);
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "fs9p" then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let quiet_git () =
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "git.value" || Logs.Src.name src = "git.memory"
      then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let quiet_irmin () =
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "irmin.bc"
      || Logs.Src.name src = "irmin.commit"
      || Logs.Src.name src = "irmin.node"
      then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let quiet () =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ()

module Client9p = Client9p_unix.Make(Log9p)
module DK = Datakit_client_9p.Make(Client9p)

module VG = struct
  include Datakit_github_vfs.Make(Datakit_github_api)
  module Sync = Datakit_github.Sync(Datakit_github_api)(DK)
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
  | _ -> failwith (address ^ ": wrong address, use proto:address")

let parse_host host =
  match String.cut ~rev:true ~sep:":" host with
  | Some (host, port) -> host, port
  | _                 -> host, "5640"

let set_signal_if_supported signal handler =
  try
    Sys.set_signal signal handler
  with Invalid_argument _ ->
    ()

let start () sandbox no_listen listen_urls
    datakit private_branch public_branch dry_updates
    webhook =
  quiet ();
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
      l "Starting %s %s ...@.The public branch is %s@.The private branch is %s"
        (Filename.basename Sys.argv.(0)) Version.v public_branch private_branch
    );
  let token = match token () with
    | None   -> failwith "Missing datakit GitHub token"
    | Some t -> t
  in
  let webhook = match webhook with
    | None   -> None
    | Some u ->
      Log.app (fun l ->
          l "Starting webhook server listening at %s" @@ Uri.to_string u);
      Some (Datakit_github_api.Webhook.create token u)
  in
  let connect_to_datakit () =
    let proto, address = parse_address datakit in
    Log.app (fun l -> l "Connecting to %s." datakit);
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
        | Ok pub  ->
          VG.Sync.sync t ?webhook ~dry_updates ~priv ~pub ~token
          >|= ignore
  in
  let accept_connections () =
    if no_listen || listen_urls = [] then Lwt.return_unit
    else
      let root = VG.create token in
      let make_root () = Vfs.Dir.of_list (fun () -> Vfs.ok [root]) in
      Lwt_list.iter_p
        (Datakit_conduit.accept_forever ~make_root ~sandbox ~serviceid)
        listen_urls
    in
  Lwt_main.run @@ Lwt.join [
    connect_to_datakit ();
    accept_connections ();
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

let no_listen =
  let doc =
    Arg.info ~doc:"Do not expose the GitHub API over 9p" ["no-listen"]
  in
  Arg.(value & flag doc)

let listen_urls =
  let doc =
    Arg.info ~doc:
      "Expose the GitHub API over 9p endpoints. That command-line argument \
       takes a comma-separated list of URLs to listen on of the form \
       file:///var/tmp/foo or tcp://host:port or \\\\\\\\.\\\\pipe\\\\foo \
       or hyperv-connect://vmid/serviceid or hyperv-accept://vmid/serviceid"
      ["l"; "listen-urls"]
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
  let doc =
    Arg.info ~doc:"The DataKit instance to connect to" ["d"; "datakit"]
  in
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

let uri =
  let parse str = `Ok (Uri.of_string str) in
  let print ppf uri = Fmt.string ppf @@ Uri.to_string uri in
  parse, print

let webhook =
  let doc =
    Arg.info ~doc:"Public URI of the GitHub webook server" ["webhook"]
  in
  Arg.(value & opt (some uri) None doc)

let webhook_secret =
  let doc = Arg.info ~doc:"Webhook secret" ["s";"webhook-secret"] in
  Arg.(value & opt (some string) None doc)

let webhook_port =
  let doc = Arg.info ~doc:"Webhook port" ["p";"webhook-port"] in
  Arg.(value & opt int 80 doc)

let dry_updates =
  let doc =
    Arg.info ~doc:"Dry API updates: do not call the GitHub API, \
                   print a line in the logs instead." ["d"; "dry-updates"]
  in
  Arg.(value & flag doc)

let term =
  let doc = "Bridge between GiHub API and Datakit." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) exposes a subset of the GitHub API as a 9p \
        filesystem. Also connect to a Datakit instance and ensure a \
        bi-directional mapping between the GitHub API and a Git branch.";
  ] in
  Term.(pure start $ setup_log $ sandbox $ no_listen $ listen_urls $
        datakit $ private_branch $ public_branch $ dry_updates $
        webhook),
  Term.info (Filename.basename Sys.argv.(0)) ~version:Version.v ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
