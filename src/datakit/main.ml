open Lwt.Infix
open Result

let src = Logs.Src.create "Datakit" ~doc:"Datakit 9p server"
module Log = (val Logs.src_log src : Logs.LOG)

let quiet_9p () =
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

(* Hyper-V socket applications use well-known GUIDs. This is ours: *)
let serviceid = "C378280D-DA14-42C8-A24E-0DE92A1028E2"

let error fmt = Printf.ksprintf (fun s ->
    Log.err (fun l -> l  "error: %s" s);
    Error (`Msg s)
  ) fmt

let max_chunk_size = Int32.of_int (100 * 1024)

let info msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  Irmin.Info.v ~date ~author:"datakit <datakit@mobyproject.org>" msg

module Git_fs_store = struct
  open Datakit_io
  module Maker = Irmin_git.FS.Make(IO)(Zlib)(FS)
  module Store = Ivfs_tree.Make(Maker)
  type t = Store.Repo.t
  module Filesystem = Ivfs.Make(Store)
  let listener = lazy (
    Irmin.Private.Watch.set_listen_dir_hook Irmin_watcher.hook
  )

  let repo path =
    let config = Irmin_git.config ~bare:true path in
    Store.Repo.v config

  let connect path =
    Lazy.force listener;
    Log.debug (fun l -> l "Using Git-format store %s" path);
    repo path >|= fun repo ->
    fun () -> Filesystem.create ~info repo
end

module In_memory_store = struct
  open Datakit_io
  module Maker = Irmin_git.Mem.Make(IO)(Zlib)
  module Store = Ivfs_tree.Make(Maker)
  type t = Store.Repo.t
  module Filesystem = Ivfs.Make(Store)

  let repo () =
    let config = Irmin_mem.config () in
    Store.Repo.v config

  let connect () =
    Log.debug (fun l ->
        l "Using in-memory store (use --git for a disk-backed store)");
    repo () >|= fun repo ->
    fun () -> Filesystem.create ~info repo
end

let set_signal_if_supported signal handler =
  try
    Sys.set_signal signal handler
  with Invalid_argument _ ->
    ()

module Date = struct
  let pretty d =
    let tm = Unix.localtime (Int64.to_float d) in
    Printf.sprintf "%02d:%02d:%02d"
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
end

let () =
  Lwt.async_exception_hook := (fun exn ->
      Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
    )

let start ~listen_9p prometheus git =
  quiet ();
  set_signal_if_supported Sys.sigpipe Sys.Signal_ignore;
  set_signal_if_supported Sys.sigterm (Sys.Signal_handle (fun _ ->
      (* On Win32 we receive this signal on every failed Hyper-V
         socket connection *)
      if Sys.os_type <> "Win32" then begin
        Log.debug (fun l -> l "Caught SIGTERM, will exit");
        exit 1
      end
    ));
  set_signal_if_supported Sys.sigint (Sys.Signal_handle (fun _ ->
      Log.debug (fun l -> l "Caught SIGINT, will exit");
      exit 1
    ));
  Log.app (fun l ->
      l "Starting %s %s ..." (Filename.basename Sys.argv.(0)) Version.v
    );
  let prometheus_threads = Prometheus_unix.serve prometheus in
  let serve_9p =
    begin match git with
      | None      -> In_memory_store.connect ()
      | Some path -> Git_fs_store.connect path
    end >|= fun make_root ->
    List.map (fun addr ->
        Datakit_conduit.accept_forever ~make_root ~serviceid addr
      ) listen_9p
  in
  serve_9p >>= fun serve_9p ->
  Lwt.choose (serve_9p @ prometheus_threads)

let start () listen_9p prometheus git auto_push =
  let start () = start ~listen_9p prometheus git in
  Lwt_main.run begin
    match auto_push with
    | None        -> start ()
    | Some remote ->
      match git with
      | None      -> Lwt.fail_with "TODO: push in-memory repositories"
      | Some local ->
        Lazy.force Git_fs_store.listener;
        Git_fs_store.repo local >>= fun repo ->
        let pusher = Autopush.create ~local ~remote in
        Git_fs_store.Store.Branch.watch_all repo
          (fun branch _ -> Autopush.push pusher ~branch; Lwt.return_unit)
        >>= fun w ->
        start () >>= fun () ->
        Git_fs_store.Store.unwatch w
  end

open Cmdliner

let env_docs = "ENVIRONMENT VARIABLES"
let listen_options = "LISTEN OPTIONS"
let git_options = "GIT OPTIONS"

let endpoint port = Datakit_conduit.(parse ~default_tcp_port:port, pp)

let setup_log =
  let env =
    Arg.env_var ~docs:env_docs
      ~doc:"Be more or less verbose. See $(b,--verbose)."
      "DATAKIT_VERBOSE"
  in
  Term.(const Datakit_log.setup $ Fmt_cli.style_renderer ()
        $ Datakit_log.log_destination $ Logs_cli.level ~env ()
        $ Datakit_log.log_clock)

let git =
  let docs = git_options in
  let doc =
    Arg.info ~docs ~doc:"The path of an existing Git repository to serve"
      ["git"]
  in
  Arg.(value & opt (some string) None doc)

let auto_push =
  let docs = git_options in
  let doc =
    Arg.info ~doc:"Auto-push the local repository to a remote source."
      ~docs ~docv:"URL" ["auto-push"]
  in
  Arg.(value & opt (some string) None doc)

let listen_9p =
  let docs = listen_options in
  let doc =
    Arg.info ~docs ~doc:
      "A comma-separated list of URLs to listen on for 9p connections, on \
       the form file:///var/tmp/foo or tcp://host:port or \
       \\\\\\\\.\\\\pipe\\\\foo or hyperv-connect://vmid/serviceid or \
       hyperv-accept://vmid/serviceid"
      ["url"; "listen-9p"]
  in
  Arg.(value & opt (list (endpoint 5640)) [ `Tcp ("127.0.0.1", 5640) ] doc)

let term =
  let doc = "A git-like database with a 9p interface." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) is a Git-like database with a 9p interface.";
  ] in
  Term.(pure start $ setup_log $ listen_9p $ Prometheus_unix.opts
        $ git $ auto_push),
  Term.info (Filename.basename Sys.argv.(0)) ~version:Version.v ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
