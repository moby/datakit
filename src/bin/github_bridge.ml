open Lwt.Infix
open Astring
open Result

let src = Logs.Src.create "github-bridge" ~doc:"Github bridge for Datakit "
module Log = (val Logs.src_log src : Logs.LOG)
module Client9p = Client9p_unix.Make(Log)
module DK = Datakit_client_9p.Make(Client9p)

module VG = struct
  open Vgithub
  include Make(API)
  module Sync = Sync(API)(DK)
end

(* Hyper-V socket applications use well-known GUIDs. This is ours: *)
let serviceid = "C378280D-DA14-42C8-A24E-0DE92A1028E3"

let token () =
  let cookie = "datakit" in
  Lwt_unix.run (
    let open Lwt.Infix in
    Github_cookie_jar.init () >>= fun jar ->
    Github_cookie_jar.get jar ~name:cookie >|= function
    | Some t -> Github.Token.of_string t.Github_t.auth_token
    | None   ->
      Printf.eprintf "Missing cookie: use git-jar to create cookie `%s`.\n%!"
        cookie;
      exit 1
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

let start () urls sandbox datakit_url datakit_branch =
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
  Log.app (fun l -> l "Starting datakit-github-bridge");
  let token = token () in
  let root = VG.create token in
  let make_root () = Vfs.Dir.of_list (fun () -> [root]) in
  Lwt_list.iter_p
    (Datakit_conduit.accept_forever ~make_root ~sandbox ~serviceid)
    urls
  >>= fun () ->
  Log.info (fun l -> l "Connecting to %s" datakit_url);
  let proto, address = parse_address datakit_url in
  Client9p.connect proto address () >>= function
  | Error (`Msg e) ->
    Log.info (fun l -> l "cannot connect: %s" e);
    Lwt.return (Error "datakit-github-bridge")
  | Ok conn        ->
    let dk = DK.connect conn in
    VG.Sync.sync token dk ~branch:datakit_branch >|= fun () ->
    Ok ()

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

let url =
  let doc =
    Arg.info ~doc:
      "A comma-separated list of URLs to listen on of the form \
       file:///var/tmp/foo or tcp://host:port or \\\\\\\\.\\\\pipe\\\\foo \
       or hyperv-connect://vmid/serviceid or hyperv-accept://vmid/serviceid"
      ["url"]
  in
  Arg.(value & opt (list string) [ "tcp://127.0.0.1:5641" ] doc)

let sandbox =
  let doc =
    Arg.info ~doc:
      "Assume we're running inside an OSX sandbox but not a chroot. \
       All paths will be manually rewritten to be relative \
       to the current directory." ["sandbox"]
  in
  Arg.(value & flag & doc)

let datakit_url =
  let doc = Arg.info ~doc:"URL of the DataKit server" ["datakit-url"] in
  Arg.(value & opt string "tcp://127.0.0.1:5641" doc)

let datakit_branch =
  let doc =
    Arg.info ~doc:"DataKit branch where the GitHub API will be mirrored."
      ["branch"]
  in
  Arg.(value & opt string "github-hook" doc)

let term =
  let doc = "Bridge between GiHub API and Datakit." in
  let man = [
    `S "DESCRIPTION";
    `P "$(i, datakit-github-bridge) exposes a subset of the GitHub API as a 9p \
        filesystem. Also connect to a Datakit instance and ensure a \
        bi-directional mapping between the GitHub API and a Git branch.";
  ] in
  Term.(pure start $ setup_log $ url $ sandbox $ datakit_url $datakit_branch),
  Term.info (Filename.basename Sys.argv.(0)) ~version:"%%VERSION%%" ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
