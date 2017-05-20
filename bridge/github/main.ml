open Lwt.Infix
open Result
open Astring

let src = Logs.Src.create "gh-bridge" ~doc:"Github bridge for Datakit"
module Log = (val Logs.src_log src : Logs.LOG)

let src9p = Logs.Src.create "g9p" ~doc:"Github bridge for Datakit (9p)"
module Log9p = (val Logs.src_log src9p : Logs.LOG)

let jar_paths = [
  (try Sys.getenv "HOME" with Not_found -> "") ^ "/.github/jar";
  "/run/secrets";
]

let jar_path =
  try List.find Sys.file_exists jar_paths
  with Not_found -> List.hd jar_paths

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

module Client9p = Protocol_9p_unix.Client9p_unix.Make(Log9p)
module DK = Datakit_client_9p.Make(Client9p)
module Sync = Datakit_github_sync.Make(Datakit_github_api)(DK)
module Sync_git =
  Datakit_github_sync.Make(Datakit_github_api)(Datakit_client_git)

let token () =
  let cookie = "datakit-github-cookie" in
  let error_msg =
    Fmt.strf "Missing cookie %s/%s: use `git-jar make -s repo <user> %s`"
      jar_path cookie cookie in
  Lwt_main.run (
    Lwt.catch (fun () ->
        let open Lwt.Infix in
        Github_cookie_jar.init ~jar_path () >>= fun jar ->
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

let () =
  Lwt.async_exception_hook := (fun exn ->
      Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
    )

type endpoint = [
  | `File of string
  | `Tcp of string * int
  | `Git of string
]

let pp_endpoint ppf = function
  | `File f     -> Fmt.pf ppf "file://%s" f
  | `Tcp (h, p) -> Fmt.pf ppf "tcp://%s:%d" h p
  | `Git d      -> Fmt.pf ppf "git://%s" d

let endpoint_of_string ~default_tcp_port str =
  match String.cut ~sep:"://" str with
  | Some ("file", f) -> `Ok (`File f)
  | Some ("tcp" , s) ->
    (match String.cut ~rev:true ~sep:":" s with
     | None              -> `Ok (`Tcp (s, default_tcp_port))
     | Some (host, port) ->
       try `Ok (`Tcp (host, int_of_string port))
       with Failure _ -> `Error "use tcp://host:port")
  | Some ("git", d) -> `Ok (`Git d)
  | _ -> `Error "invalid endpoint"

type datakit_config = {
  endpoint: endpoint;
  branch  : string;
}

let monitor (type a) (module DK: Datakit_client.S with type Branch.t = a)
    (br: a) repos =
  let module Conv = Datakit_github_conv.Make(DK) in
  if repos <> [] then (
    DK.Branch.with_transaction br (fun tr ->
        Lwt_list.iter_p (fun r ->
            match Datakit_github.Repo.of_string r with
            | None   -> Lwt.return_unit
            | Some r -> Conv.update_elt tr (`Repo r)
          ) repos
        >>= fun () ->
        DK.Transaction.commit tr ~message:"initial commit"
      ) >>= function
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" DK.pp_error e
    | Ok ()   -> Lwt.return_unit
  ) else
    Lwt.return_unit

let connect_9p ~token ?webhook ?resync_interval ~cap ~branch ~repositories
    endpoint =
  let proto, address = match endpoint with
    | `Tcp (host, port) -> "tcp" , Fmt.strf "%s:%d" host port
    | `File path        -> "unix", path (* FIXME: weird proto name for 9p *)
  in
  Lwt.catch
    (fun () ->
       Client9p.connect
         proto address ~send_pings:true ~max_fids:Int32.max_int ())
    (fun e  -> Lwt.fail_with @@ Fmt.strf "%a" Fmt.exn e)
  >>= function
  | Error (`Msg e) ->
    Log.err (fun l -> l "cannot connect: %s" e);
    Lwt.fail_with "connecting to datakit"
  | Ok conn        ->
    Log.info (fun l -> l "Connected to %a" pp_endpoint endpoint);
    let dk = DK.connect conn in
    let t = Sync.empty in
    DK.branch dk branch >>= function
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
    | Ok br   ->
      monitor (module DK) br repositories >>= fun () ->
      Sync.sync ~token ?webhook ?resync_interval ~cap br t
      >|= ignore

let connect_git ~token ?webhook ?resync_interval ~cap ~branch ~repositories
    root =
  Datakit_client_git.connect ~head:("refs/heads/" ^ branch) ~bare:false root
  >>= fun dk ->
  let t = Sync_git.empty in
  Datakit_client_git.branch dk branch >>= function
  | Error e -> Lwt.fail_with @@ Fmt.strf "%a" Datakit_client_git.pp_error e
  | Ok br   ->
    monitor (module Datakit_client_git) br repositories >>= fun () ->
    Sync_git.sync ~token ?webhook ?resync_interval ~cap br t
    >|= ignore

let connect ~token ?webhook ?resync_interval ~repositories ~cap d =
  let branch = d.branch in
  match d.endpoint with
  | `Tcp _ | `File _ as x ->
    connect_9p ~token ?webhook ?resync_interval ~cap ~branch ~repositories x
  | `Git root             ->
    connect_git ~token ?webhook ?resync_interval ~cap ~branch ~repositories root

let start () datakit repositories cap webhook resync_interval prometheus =
  quiet ();
  let prometheus_threads = Prometheus_unix.serve prometheus in
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
      Some (Datakit_github_api.Webhook.v token u)
  in
  let connect_to_datakit () =
    match datakit with
    | None   -> let t, _ = Lwt.task () in t
    | Some d ->
      Log.app (fun l -> l "Connecting to %a [%s]."
                  pp_endpoint d.endpoint d.branch);
      connect ~token ?webhook ?resync_interval ~cap ~repositories d
  in
  Lwt_main.run @@ Lwt.choose (
    [connect_to_datakit ()] @ prometheus_threads
  )

open Cmdliner

let env_docs = "ENVIRONMENT VARIABLES"
let datakit_options = "DATAKIT OPTIONS"
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

let endpoint port = (endpoint_of_string ~default_tcp_port:port, pp_endpoint)

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

let repositories =
  let docs = github_options in
  let doc =
    Arg.info ~docs ~doc:"A list of repository to monitor on startup."
      ["repositories";"r"]
  in
  Arg.(value & opt (list string) [] doc)

let term =
  let doc = "Bridge between GitHub API and Datakit." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) exposes a subset of the GitHub API as a 9p \
        filesystem. Also connect to a DataKit instance and ensure a \
        bidirectional mapping between the GitHub API and a Git branch.";
  ] in
  Term.(pure start $ setup_log
        $ datakit $ repositories
        $ capabilities $ webhook $ resync $ Prometheus_unix.opts),
  Term.info (Filename.basename Sys.argv.(0)) ~version:Version.v ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
