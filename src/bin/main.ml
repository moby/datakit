open Lwt.Infix
open Result
open Astring

module UnixServer = Fs9p.Make(Flow_lwt_unix)
module HyperVServer = Fs9p.Make(Flow_lwt_hvsock)

let src = Logs.Src.create "Datakit" ~doc:"Datakit 9p server"
module Log = (val Logs.src_log src : Logs.LOG)

(* Hyper-V socket applications use well-known GUIDs. This is ours: *)
let default_serviceid = "C378280D-DA14-42C8-A24E-0DE92A1028E2"

let error fmt = Printf.ksprintf (fun s ->
    Log.err (fun l -> l  "error: %s" s);
    Error (`Msg s)
  ) fmt

let max_chunk_size = Int32.of_int (100 * 1024)

let make_task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  Irmin.Task.create ~date ~owner:"datakit <datakit@docker.com>" msg

module Git_fs_store = struct
  open Irmin
  module Store =
    Irmin_git.FS(Ir_io.Sync)(Ir_io.Zlib)(Ir_io.Lock)(Ir_io.FS)
      (Contents.String)(Ref.String)(Hash.SHA1)
  type t = Store.Repo.t
  module Filesystem = Ivfs.Make(Store)
  let listener = lazy (Ir_io.Poll.install_dir_polling_listener 1.0)

  let repo ~bare path =
    let config = Irmin_git.config ~root:path ~bare () in
    Store.Repo.create config

  let connect ~bare path =
    Lazy.force listener;
    Log.debug (fun l -> l "Using Git-format store %S" path);
    repo ~bare path >|= fun repo ->
    let subdirs = Main_pp.subdirs () in
    fun () -> Filesystem.create make_task repo ~subdirs
end

module In_memory_store = struct
  open Irmin
  module Store = Irmin_git.Memory(Ir_io.Sync)(Ir_io.Zlib)(Contents.String)(Ref.String)(Hash.SHA1)
  type t = Store.Repo.t
  module Filesystem = Ivfs.Make(Store)

  let repo () =
    let config = Irmin_mem.config () in
    Store.Repo.create config

  let connect () =
    Log.debug (fun l ->
        l "Using in-memory store (use --git for a disk-backed store)");
    repo () >|= fun repo ->
    let subdirs = Main_pp.subdirs () in
    fun () -> Filesystem.create make_task repo ~subdirs
end

let handle_unix_flow ~make_root fd =
  Log.debug (fun l -> l "New unix client");
  Lwt.catch
    (fun () ->
      let flow = Flow_lwt_unix.connect fd in
      (* Re-build the filesystem for each client because command files
         need per-client state. *)
      let root = make_root () in
      UnixServer.accept ~root flow >|= function
      | Error (`Msg msg) ->
        Log.debug (fun l -> l "Error handling client connection: %s" msg)
      | Ok () -> ()
    ) (fun e ->
       Log.err (fun l ->
         l "Caught %s: closing connection" (Printexc.to_string e));
       Lwt.return ()
    )

let handle_hyperv_flow ~make_root fd =
  Log.debug (fun l -> l "New Hyper-V client");
  Lwt.catch
    (fun () ->
      let flow = Flow_lwt_hvsock.connect fd in
      (* Re-build the filesystem for each client because command files
         need per-client state. *)
      let root = make_root () in
      HyperVServer.accept ~root flow >|= function
      | Error (`Msg msg) ->
        Log.debug (fun l -> l "Error handling client connection: %s" msg)
      | Ok () -> ()
    ) (fun e ->
       Log.err (fun l ->
         l "Caught %s: closing connection" (Printexc.to_string e));
       Lwt.return ()
    )

let default d = function
  | Some x -> x
  | None -> d

let make_unix_socket path =
  Lwt.catch
    (fun () -> Lwt_unix.unlink path)
    (function
      | Unix.Unix_error(Unix.ENOENT, _, _) -> Lwt.return ()
      | e -> Lwt.fail e)
  >>= fun () ->
  let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.bind s (Lwt_unix.ADDR_UNIX path);
  Lwt.return s

let set_signal_if_supported signal handler =
  try
    Sys.set_signal signal handler
  with Invalid_argument _ ->
    ()

let start urls sandbox git ~bare =
  set_signal_if_supported Sys.sigpipe Sys.Signal_ignore;
  set_signal_if_supported Sys.sigterm (Sys.Signal_handle (fun _ ->
      (* On Win32 we receive this signal on every failed Hyper-V socket connection *)
      if Sys.os_type <> "Win32" then begin
        Log.debug (fun l -> l "Caught SIGTERM, will exit");
      end
    ));
  set_signal_if_supported Sys.sigint (Sys.Signal_handle (fun _ ->
      Log.debug (fun l -> l "Caught SIGINT, will exit");
      exit 1
    ));
  Log.app (fun l -> l "Starting com.docker.db...");
  let prefix = if sandbox then "." else "" in
  begin match git with
    | None -> In_memory_store.connect ()
    | Some path -> Git_fs_store.connect ~bare (prefix ^ path)
  end >>= fun make_root ->

  let unix_accept_forever url socket callback =
    Lwt_unix.listen socket 5;
    let rec aux () =
      Lwt_unix.accept socket >>= fun (client, _addr) ->
      let _ = (* background thread *)
        (* the callback will close the connection when its done *)
        callback client in
      aux () in
    Log.info (fun l -> l "Waiting for connections on Unix socket %a" Uri.pp_hum url);
    aux () in

  let hvsock_accept_forever url socket callback =
    Lwt_hvsock.listen socket 5;
    let rec aux () =
      Lwt_hvsock.accept socket >>= fun (client, _addr) ->
      let _ = (* background thread *)
        (* the callback will close the connection when its done *)
        callback client in
      aux () in
    Log.info (fun l -> l "Waiting for connections on hv socket %a" Uri.pp_hum url);
    aux () in

  let hvsock_connect_forever url sockaddr callback =
    let rec aux () =
      let socket = Lwt_hvsock.create () in
      Lwt.catch
        (fun () ->
          Lwt_hvsock.connect socket sockaddr
          >>= fun () ->
          callback socket
        ) (fun _e ->
          Lwt_hvsock.close socket
          >>= fun () ->
          Lwt_unix.sleep 1.
        )
      >>= fun () ->
      aux () in
    Log.info (fun l -> l "Waiting for connections on hv socket %a" Uri.pp_hum url);
    aux () in

  let hvsock_addr_of_uri uri =
    (* hyperv://vmid/servivceid *)
    let vmid = match Uri.host uri with None -> Hvsock.Loopback | Some x -> Hvsock.Id x in
    let serviceid =
      let p = Uri.path uri in
      if p = "" then default_serviceid
      else
        (* trim leading / *)
        String.drop ~sat:((=) '/') ~max:1 p in
    { Hvsock.vmid; serviceid } in

  let rec named_pipe_accept_forever path callback =
    let open Lwt.Infix in
    let p = Named_pipe_lwt.Server.create path in
    Named_pipe_lwt.Server.connect p
    >>= function
    | false ->
      Log.err (fun f -> f "Named-pipe connection failed on %s" path);
      Lwt.return ()
    | true ->
      let _ = (* background thread *)
        let fd = Named_pipe_lwt.Server.to_fd p in
        callback fd in
      named_pipe_accept_forever path callback in

  Lwt_list.iter_p (fun url ->
    Lwt.catch
      (fun () ->
         (* Check if it looks like a UNC name before a URI *)
         if Astring.String.is_prefix ~affix:"\\\\" url then begin
           Log.info (fun f -> f "Accepting connections on named pipe %s" url);
           named_pipe_accept_forever url (handle_unix_flow ~make_root)
         end else
         let uri = Uri.of_string url in
         match Uri.scheme uri with
         | Some "file" ->
           make_unix_socket (prefix ^ Uri.path uri)
           >>= fun socket ->
           unix_accept_forever uri socket (handle_unix_flow ~make_root)
         | Some "tcp" ->
           begin match Uri.path uri with
             | "" | "/" -> ()
             | path ->
               Printf.fprintf stderr
                 "tcp address should not have a path component (path=%S) - use tcp://addr:port" path;
               exit 1;
           end;
           let host = Uri.host uri |> default "127.0.0.1" in
           let port = Uri.port uri |> default 5640 in
           let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
           let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
           Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;       (* Makes testing easier *)
           Lwt_unix.bind socket addr;
           unix_accept_forever uri socket (handle_unix_flow ~make_root)
         | Some "hyperv-connect" ->
           hvsock_connect_forever uri (hvsock_addr_of_uri uri) (handle_hyperv_flow ~make_root)
         | Some "hyperv-accept" ->
           let socket = Lwt_hvsock.create () in
           Lwt_hvsock.bind socket (hvsock_addr_of_uri uri);
           hvsock_accept_forever uri socket (handle_hyperv_flow ~make_root)
         | _ ->
           Printf.fprintf stderr
             "Unknown URL schema. Please use file: or tcp:\n";
           exit 1
      )
      (fun ex ->
         Printf.fprintf stderr
           "Failed to set up server socket listening on %S: %s\n%!"
           url (Printexc.to_string ex);
         exit 1
      )
  ) urls

let start () url sandbox git bare auto_push =
  let start () = start url sandbox git ~bare in
  Lwt_main.run begin
    match auto_push with
    | None        -> start ()
    | Some remote ->
      let watch () = match git with
        | None      ->
          In_memory_store.repo () >>= fun repo ->
          In_memory_store.Store.Repo.watch_branches repo (fun _ _ ->
              Lwt.fail_with "TOTO"
            )
        | Some path ->
          Lazy.force Git_fs_store.listener;
          let prefix = if sandbox then "." else "" in
          let path = prefix ^ path in
          let exec ~name cmd =
            Lwt_process.exec cmd >|= function
            | Unix.WEXITED 0   -> ()
            | Unix.WEXITED i   ->
              Logs.err (fun l -> l "%s to %s exited with code %d" name remote i)
            | Unix. WSIGNALED i ->
              Logs.err (fun l -> l "%s to %s killed by signal %d)" name remote i)
            | Unix.WSTOPPED i  ->
              Logs.err (fun l -> l "%s to %s stopped by signal %d" name remote i)
          in
          let pull () =
            Logs.debug (fun l -> l "Pulling %s to %s" remote path);
            let cmd =
              Lwt_process.shell @@
              Printf.sprintf
                "mkdir -p %S && cd %S && git init && \
                 (git remote add origin %S || echo origin is already set) && \
                 git fetch origin && \
                 for remote in `git branch -r`; do \
                \  echo tracking $remote && \
                \  (git branch --track \"${remote#origin/}\" \"$remote\" \
                \    || echo $remote is already tracked) && \
                \  git pull origin \"${remote#origin/}\" --no-edit; \
                 done"
                path path remote
            in
            exec ~name:"initial pull" cmd
          in
          let push () =
            Logs.debug (fun l -> l "Pushing %s to %s" path remote);
            let cmd =
              Lwt_process.shell @@
              Printf.sprintf "cd %S && git push %S --all" path remote
            in
            exec ~name:"auto-push" cmd
          in
          pull () >>= fun () ->
          push () >>= fun () ->
          Git_fs_store.repo ~bare path >>= fun repo ->
          Git_fs_store.Store.Repo.watch_branches repo (fun _ _ -> push ())
      in
      watch () >>= fun unwatch ->
      start () >>= fun () ->
      unwatch ()
  end

open Cmdliner

let reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.to_us (Mtime.elapsed ()) in
      Fmt.kpf k ppf ("\r%0+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string) (Printf.sprintf "%10s" @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

module Log_destination = struct
  type t =
    | Stderr
    | Eventlog
    | ASL

  let parser x = match String.Ascii.lowercase x with
    | "stderr" -> `Ok Stderr
    | "eventlog" -> `Ok Eventlog
    | "asl" -> `Ok ASL
    | _ -> `Error("Unknown log destination: expected stderr / eventlog / asl")

  let printer fmt x = Format.pp_print_string fmt (match x with
    | Stderr -> "stderr"
    | Eventlog -> "eventlog"
    | ASL -> "asl")

  let conv = parser, printer
end

let setup_log style_renderer log_destination level =
  Logs.set_level level;
  let open Log_destination in
  match log_destination with
  | Eventlog ->
    let eventlog = Eventlog.register "Docker.exe" in
    Logs.set_reporter (Log_eventlog.reporter ~eventlog ())
  | Stderr ->
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_reporter (reporter ())
  | ASL ->
    let facility = Filename.basename Sys.executable_name in
    let client = Asl.Client.create ~ident:"Docker" ~facility () in
    Logs.set_reporter (Log_asl.reporter ~client ())

let env_docs = "ENVIRONMENT VARIABLES"

let log_destination =
  let doc =
    Arg.info ~doc:"Destination for the logs" [ "log-destination" ]
  in
  Arg.(value & opt Log_destination.conv Log_destination.Stderr & doc)

let setup_log =
  let env =
    Arg.env_var ~docs:env_docs
      ~doc:"Be more or less verbose. See $(b,--verbose)."
      "DATAKIT_VERBOSE"
  in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ log_destination $ Logs_cli.level ~env ())

let git =
  let doc =
    Arg.info ~doc:"The path of an existing Git repository to serve" ["git"]
  in
  Arg.(value & opt (some string) None doc)

let url =
  let doc =
    Arg.info ~doc:
      "A comma-separated list of URLs to listen on of the form \
      file:///var/tmp/foo or tcp://host:port or \\\\\\\\.\\\\pipe\\\\foo \
      or hyperv-connect://vmid/serviceid or hyperv-accept://vmid/serviceid" ["url"]
  in
  Arg.(value & opt (list string) [ "tcp://127.0.0.1:5640" ] doc)

let sandbox =
  let doc =
    Arg.info ~doc:
      "Assume we're running inside an OSX sandbox but not a chroot. \
       All paths will be manually rewritten to be relative \
       to the current directory." ["sandbox"]
  in
  Arg.(value & flag & doc)

let bare =
  let doc =
    Arg.info ~doc:"Use a bare Git repository (no working directory)" ["bare"]
  in
  Arg.(value & flag & doc)

let auto_push =
  let doc =
    Arg.info ~doc:"Auto-push the local repository to a remote source."
      ~docv:"URL" ["auto-push"]
  in
  Arg.(value & opt (some string) None doc)

let term =
  let doc = "A git-like database with a 9p interface." in
  let man = [
    `S "DESCRIPTION";
    `P "$(i, com.docker.db) is a Git-like database with a 9p interface.";
  ] in
  Term.(pure start $ setup_log $ url $ sandbox $ git $ bare $ auto_push),
  Term.info (Filename.basename Sys.argv.(0)) ~version:"%%VERSION%%" ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
