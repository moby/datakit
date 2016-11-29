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
  module Sync = Datakit_github_sync.Make(Datakit_github_api)(DK)
end

(* Hyper-V socket applications use well-known GUIDs. This is ours: *)
let serviceid = "C378280D-DA14-42C8-A24E-0DE92A1028E3"

let token () =
  let cookie = "datakit" in
  let error_msg = "Missing cookie: use `git-jar make -s repo <user> " ^ cookie ^"`" in
  Lwt_main.run (
    Lwt.catch (fun () ->
        let open Lwt.Infix in
        Github_cookie_jar.init () >>= fun jar ->
        Github_cookie_jar.get jar ~name:cookie >|= function
        | Some t -> Ok (Github.Token.of_string t.Github_t.auth_token)
        | None   -> Error (`Msg error_msg)
      ) (fun e ->
           Log.err (fun l -> l "%s\n%s%!" error_msg (Printexc.to_string e));
           Lwt.return (Error (`Msg error_msg))
      )
   )

let set_signal_if_supported signal handler =
  try
    Sys.set_signal signal handler
  with Invalid_argument _ ->
    ()

let ( >>~ ) x f =
  x >>= function
  | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
  | Ok t    -> f t

let () =
  Lwt.async_exception_hook := (fun exn ->
      Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
    )

type datakit_config = {
  endpoint: Datakit_conduit.t;
  branch  : string;
}

let start () no_listen listen_urls datakit cap webhook resync_interval =
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
  Log.app (fun l -> l "Starting %s %s (%a)..."
              (Filename.basename Sys.argv.(0)) Version.v
              Datakit_github.Capabilities.pp cap);
  let token = match token () with
    | Error (`Msg m) -> failwith m
    | Ok t           ->
      let user_agent = "datakit-github/%%NUM_VERSION%%" in
      Datakit_github_api.token ~token:t ~user_agent
  in
  let webhook = match webhook with
    | None   -> None
    | Some u ->
      Log.app (fun l ->
          l "Starting webhook server listening at %s" @@ Uri.to_string u);
      Some (Datakit_github_api.Webhook.create token u)
  in
  let connect_to_datakit () =
    match datakit with
    | None   -> let t, _ = Lwt.task () in t
    | Some d ->
      Log.app (fun l -> l "Connecting to %a [%s]."
                  Datakit_conduit.pp d.endpoint d.branch);
      let proto, address = match d.endpoint with
        | `Tcp (host, port) -> "tcp" , Fmt.strf "%s:%d" host port
        | `File path        -> "unix", path (* FIXME: weird proto name for 9p *)
        | p ->
          Log.err (fun l ->
              l "Cannot connect over 9p to %a: transport not (yet) supported"
                Datakit_conduit.pp p);
          failwith "connect to datakit"
      in
      Lwt.catch
        (fun () -> Client9p.connect proto address ())
        (fun e  -> Lwt.fail_with @@ Fmt.strf "%a" Fmt.exn e)
      >>= function
      | Error (`Msg e) ->
        Log.err (fun l -> l "cannot connect: %s" e);
        Lwt.fail_with "connecting to datakit"
      | Ok conn        ->
        Log.info (fun l -> l "Connected to %a" Datakit_conduit.pp d.endpoint);
        let dk = DK.connect conn in
        let t = VG.Sync.empty in
        DK.branch dk d.branch >>~ fun br ->
        VG.Sync.sync ~token ?webhook ?resync_interval ~cap br t
        >|= ignore
  in
  let accept_9p_connections () =
    if no_listen || listen_urls = [] then []
    else
      let make_root = let r = VG.root token in fun () -> r in
      List.map (fun addr ->
          Datakit_conduit.accept_forever ~make_root ~serviceid addr
        ) listen_urls
  in
  Lwt_main.run @@ Lwt.choose (
    connect_to_datakit () :: accept_9p_connections ()
  )

open Cmdliner

let env_docs = "ENVIRONMENT VARIABLES"
let datakit_options = "DATAKIT OPTIONS"
let listen_options = "LISTEN OPTIONS"
let github_options = "GITHUB OPTIONS"

let setup_log =
  let env =
    Arg.env_var ~docs:env_docs
      ~doc:"Be more or less verbose. See $(b,--verbose)."
      "DATAKIT_VERBOSE"
  in
  Term.(const Datakit_log.setup $ Fmt_cli.style_renderer ()
        $ Datakit_log.log_destination $ Logs_cli.level ~env ()
        $ Datakit_log.log_clock)

let no_listen =
  let docs = listen_options in
  let doc =
    Arg.info ~docs ~doc:"Do not expose the GitHub API over 9p" ["no-listen"]
  in
  Arg.(value & flag doc)

let endpoint port = Datakit_conduit.(parse ~default_tcp_port:port, pp)

let listen_urls =
  let docs = listen_options in
  let doc =
    Arg.info ~docs ~doc:
      "Expose the GitHub API over 9p endpoints. That command-line argument \
       takes a comma-separated list of URLs to listen on of the form \
       file:///var/tmp/foo or tcp://host:port or \\\\\\\\.\\\\pipe\\\\foo \
       or hyperv-connect://vmid/serviceid or hyperv-accept://vmid/serviceid"
      ["l"; "listen-urls"]
  in
  (* FIXME: maybe we want to not listen by default *)
  Arg.(value & opt (list (endpoint 5641)) [ `Tcp ("127.0.0.1", 5641) ] doc)

let no_datakit =
  let docs = datakit_options in
  let doc = Arg.info ~doc:"Do not connect to datakit" ~docs ["no-datakit"] in
  Arg.(value & flag doc)

let datakit_endpoint =
  let docs = datakit_options in
  let doc =
    Arg.info ~docs ~doc:"The DataKit instance to connect to" ["d"; "datakit"]
  in
  Arg.(value & opt (endpoint 5640) (`Tcp ("127.0.0.1", 5640)) doc)

let branch =
  let docs = datakit_options in
  let doc =
    Arg.info ~docs
      ~doc:"DataKit/GitHub branch. Reflect the GitHub state. \
            Writes to this branch will be translated into    \
            GitHub API calls."
      ["b"; "branch"]
  in
  Arg.(value & opt string "github-metadata" doc)

let datakit =
  let create no endpoint branch =
    if no then None else Some { endpoint; branch }
  in
  Term.(pure create $ no_datakit $ datakit_endpoint $ branch)

let uri =
  let parse str = `Ok (Uri.of_string str) in
  let print ppf uri = Fmt.string ppf @@ Uri.to_string uri in
  parse, print

let webhook =
  let docs = github_options in
  let doc =
    Arg.info ~docs ~doc:"Public URI of the GitHub webhook server" ["webhook"]
  in
  Arg.(value & opt (some uri) None doc)

let resync_interval =
  let docs = github_options in
  let doc =
    Arg.info ~docs ~doc:"Interval to wait before doing full resyncrhonisation \
                         of the repositories." ["resync-interval"]
  in
  Arg.(value & opt float (60. *. 60. (* every hour *)) doc)

let no_resync =
  let docs = github_options in
  let doc = Arg.info ~docs ~doc:"Do not resync." ["no-resync"] in
  Arg.(value & flag doc)

let resync =
  Term.(pure (fun i -> function false -> Some i | true -> None)
        $ resync_interval $ no_resync)

let cap: Datakit_github.Capabilities.t Cmdliner.Arg.converter =
  Datakit_github.Capabilities.(parse, pp)

let capabilities =
  let docs = github_options in
  let doc =
    Arg.info ~docs ~doc:
      "A comma-separated list of API capabilities, for instance \
       `*:r,status:rw` to allow the read of all resources but only the write \
       of build status." ["c"; "capabilities"]
  in
  Arg.(value & opt cap Datakit_github.Capabilities.all doc)

let term =
  let doc = "Bridge between GiHub API and Datakit." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) exposes a subset of the GitHub API as a 9p \
        filesystem. Also connect to a DataKit instance and ensure a \
        bidirectional mapping between the GitHub API and a Git branch.";
  ] in
  Term.(pure start $ setup_log $ no_listen $ listen_urls
        $ datakit $ capabilities $ webhook $ resync),
  Term.info (Filename.basename Sys.argv.(0)) ~version:Version.v ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
