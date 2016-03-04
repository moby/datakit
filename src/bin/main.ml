open Lwt.Infix
open Result

module Log : Protocol_9p.S.LOG = struct
  let debug fmt = Fmt.kstrf print_endline fmt
  let info  fmt = Fmt.kstrf (fun s -> print_endline s) fmt
  let error fmt = Fmt.kstrf (fun s -> print_endline s) fmt
end

let log fmt = Printf.ksprintf print_endline fmt

let error fmt = Printf.ksprintf (fun s ->
    ignore (log "error: %s" s);
    Error (`Msg s)
  ) fmt

let max_chunk_size = Int32.of_int (100 * 1024)

module Server = Fs9p_fs.Make(Log)(Flow_lwt_unix)

let make_task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  Irmin.Task.create ~date ~owner:"irmin9p" msg

module Git_fs_store = struct
  open Irmin
  module Store = Irmin_unix.Irmin_git.FS(Contents.String)(Ref.String)(Hash.SHA1)
  type t = Store.Repo.t
  module Filesystem = I9p_irmin.Make(Server.Inode)(Store)
  let listener = lazy (Irmin_unix.install_dir_polling_listener 1.0)
  let connect ~bare path =
    Lazy.force listener;
    log "Using Git-format store %S" path;
    let config = Irmin_unix.Irmin_git.config ~root:path ~bare () in
    Store.Repo.create config >|= fun repo ->
    fun () -> Filesystem.create make_task repo
end

module In_memory_store = struct
  open Irmin
  module Store = Irmin_mem.Make(Contents.String)(Ref.String)(Hash.SHA1)
  type t = Store.Repo.t
  module Filesystem = I9p_irmin.Make(Server.Inode)(Store)
  let connect () =
    log "Using in-memory store (use --git for a disk-backed store)";
    let config = Irmin_mem.config () in
    Store.Repo.create config >|= fun repo ->
    fun () -> Filesystem.create make_task repo
end

let handle_flow ~make_root flow =
  log "New client";
  (* Re-build the filesystem for each client because command files
     need per-client state. *)
  let root = make_root () in
  Server.accept ~root flow >|= function
  | Error (`Msg msg) -> log "Error handling client connection: %s" msg
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

let start url sandbox git ~bare =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
    log "Caught SIGTERM, will exit";
    exit 1
  ));
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    log "Caught SIGINT, will exit";
    exit 1
  ));
  log "Starting com.docker.db...";
  let prefix = if sandbox then "." else "" in
  begin match git with
  | None -> In_memory_store.connect ()
  | Some path -> Git_fs_store.connect ~bare (prefix ^ path)
  end >>= fun make_root ->
  let url = url |> default "file:///var/tmp/com.docker.db.socket" in
  Lwt.catch
    (fun () ->
      let uri = Uri.of_string url in
      match Uri.scheme uri with
      | Some "file" ->
          make_unix_socket (prefix ^ Uri.path uri)
      | Some "tcp" ->
          let host = Uri.host uri |> default "127.0.0.1" in
          let port = Uri.port uri |> default 5640 in
          let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
          let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
          Lwt_unix.bind socket addr;
          Lwt.return socket
      | _ ->
          Printf.fprintf stderr
            "Unknown URL schema. Please use file: or tcp:\n";
          exit 1
    )
    (fun ex ->
      Printf.fprintf stderr "Failed to set up server socket listening on %S: %s\n%!"
        url (Printexc.to_string ex);
      exit 1
    )
  >>= fun socket ->
  Lwt_unix.listen socket 5;
  let rec aux () =
    Lwt_unix.accept socket >>= fun (client, _addr) ->
    let flow = Flow_lwt_unix.connect client in
    Lwt.async (fun () ->
      Lwt.catch
        (fun () ->handle_flow ~make_root flow)
        (fun e ->
          Log.error "Caught %s: closing connection" (Printexc.to_string e);
          Lwt.return ()
        )
    );
    aux () in
  log "Waiting for connections on socket %S" url;
  aux ()

let start url sandbox git bare = Lwt_main.run (start url sandbox git ~bare)

open Cmdliner

let git =
  let doc =
    Arg.info ~doc:"The path of an existing Git repository to serve" ["git"]
  in
  Arg.(value & opt (some string) None doc)

let url =
  let doc =
    Arg.info ~doc:"The URL to listen on of the for \
                   file:///var/tmp/foo or \
                   tcp://host:port" ["url"]
  in
  Arg.(value & opt (some string) None doc)

let sandbox =
  let doc =
    Arg.info ~doc:"Assume we're running inside an OSX sandbox but not a chroot. \
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
  Term.(pure start $ url $ sandbox $ git $ bare),
  Term.info "com.docker.db" ~version:Version.v ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
