open Lwt.Infix
open Result

module Server = Fs9p.Make(Flow_lwt_unix)

let src = Logs.Src.create "Datakit" ~doc:"Datakit 9p server"
module Log = (val Logs.src_log src : Logs.LOG)

let error fmt = Printf.ksprintf (fun s ->
    Log.err (fun l -> l  "error: %s" s);
    Error (`Msg s)
  ) fmt

let max_chunk_size = Int32.of_int (100 * 1024)

let make_task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  Irmin.Task.create ~date ~owner:"irmin9p" msg

module Git_fs_store = struct
  open Irmin
  module Store =
    Irmin_git.FS(Ir_io.Sync)(Ir_io.Zlib)(Ir_io.Lock)(Ir_io.FS)
      (Contents.String)(Ref.String)(Hash.SHA1)
  type t = Store.Repo.t
  module Filesystem = Ivfs.Make(Store)
  let listener = lazy (Ir_io.Poll.install_dir_polling_listener 1.0)
  let connect ~bare path =
    Lazy.force listener;
    Log.debug (fun l -> l "Using Git-format store %S" path);
    let config = Irmin_git.config ~root:path ~bare () in
    Store.Repo.create config >|= fun repo ->
    let subdirs = Main_pp.subdirs () in
    fun () -> Filesystem.create make_task repo ~subdirs
end

module In_memory_store = struct
  open Irmin
  module Store = Irmin_git.Memory(Ir_io.Sync)(Ir_io.Zlib)(Contents.String)(Ref.String)(Hash.SHA1)
  type t = Store.Repo.t
  module Filesystem = Ivfs.Make(Store)
  let connect () =
    Log.debug (fun l ->
        l "Using in-memory store (use --git for a disk-backed store)");
    let config = Irmin_mem.config () in
    Store.Repo.create config >|= fun repo ->
    let subdirs = Main_pp.subdirs () in
    fun () -> Filesystem.create make_task repo ~subdirs
end

let handle_flow ~make_root flow =
  Log.debug (fun l -> l "New client");
  (* Re-build the filesystem for each client because command files
     need per-client state. *)
  let root = make_root () in
  Server.accept ~root flow >|= function
  | Error (`Msg msg) ->
    Log.debug (fun l -> l "Error handling client connection: %s" msg)
  | Ok () -> ()

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
  with Invalid_argument "Sys.signal: unavailable signal" ->
    ()

let start url sandbox git ~bare =
  set_signal_if_supported Sys.sigpipe Sys.Signal_ignore;
  set_signal_if_supported Sys.sigterm (Sys.Signal_handle (fun _ ->
      Log.debug (fun l -> l "Caught SIGTERM, will exit");
      exit 1
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
  (* Wrapper which guarantees not to fail *)
  let handle_flow client =
    Lwt.catch
      (fun () ->
         let flow = Flow_lwt_unix.connect client in
         handle_flow ~make_root flow
      ) (fun e ->
         Log.err (fun l ->
           l "Caught %s: closing connection" (Printexc.to_string e));
         Lwt.return ()
      ) in

  let unix_accept_forever url socket callback =
    Lwt_unix.listen socket 5;
    let rec aux () =
      Lwt_unix.accept socket >>= fun (client, _addr) ->
      let _ = (* background thread *)
        callback client
        >>= fun () ->
        Lwt_unix.close client in
      aux () in
    Log.debug (fun l -> l "Waiting for connections on socket %S" url);
    aux () in

  let url = url |> default "file:///var/tmp/com.docker.db.socket" in
  Lwt.catch
    (fun () ->
       let uri = Uri.of_string url in
       match Uri.scheme uri with
       | Some "file" ->
         make_unix_socket (prefix ^ Uri.path uri)
         >>= fun socket ->
         unix_accept_forever url socket handle_flow
       | Some "tcp" ->
         let host = Uri.host uri |> default "127.0.0.1" in
         let port = Uri.port uri |> default 5640 in
         let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
         let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
         Lwt_unix.bind socket addr;
         unix_accept_forever url socket handle_flow
       | Some "named-pipe" ->
         Main_pp.named_pipe_accept_forever (Uri.path uri) handle_flow
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

let start () url sandbox git bare = Lwt_main.run (start url sandbox git ~bare)

open Cmdliner

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.to_us (Mtime.elapsed ()) in
      Fmt.kpf k ppf ("\r%0+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let env_docs = "ENVIRONMENT VARIABLES"

let setup_log =
  let env =
    Arg.env_var ~docs:env_docs
      ~doc:"Be more or less verbose. See $(b,--verbose)."
      "DATAKIT_VERBOSE"
  in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ~env ())

let git =
  let doc =
    Arg.info ~doc:"The path of an existing Git repository to serve" ["git"]
  in
  Arg.(value & opt (some string) None doc)

let url =
  let doc =
    Arg.info ~doc:
      "The URL to listen on of the for file:///var/tmp/foo or \
       tcp://host:port" ["url"]
  in
  Arg.(value & opt (some string) None doc)

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

let term =
  let doc = "A git-like database with a 9p interface." in
  let man = [
    `S "DESCRIPTION";
    `P "$(i, com.docker.db) is a Git-like database with a 9p interface.";
  ] in
  Term.(pure start $ setup_log $ url $ sandbox $ git $ bare),
  Term.info (Filename.basename Sys.argv.(0)) ~version:Version.v ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
