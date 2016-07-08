open Lwt.Infix
open Result
open Astring

let src = Logs.Src.create "Datakit" ~doc:"Datakit 9p server"
module Log = (val Logs.src_log src : Logs.LOG)

(* Hyper-V socket applications use well-known GUIDs. This is ours: *)
let serviceid = "C378280D-DA14-42C8-A24E-0DE92A1028E2"

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
    fun () -> Filesystem.create make_task repo
end

module In_memory_store = struct
  open Irmin
  module Store = Irmin_git.Memory
      (Ir_io.Sync)(Ir_io.Zlib)(Contents.String)(Ref.String) (Hash.SHA1)
  type t = Store.Repo.t
  module Filesystem = Ivfs.Make(Store)

  let repo () =
    let config = Irmin_mem.config () in
    Store.Repo.create config

  let connect () =
    Log.debug (fun l ->
        l "Using in-memory store (use --git for a disk-backed store)");
    repo () >|= fun repo ->
    fun () -> Filesystem.create make_task repo
end

let set_signal_if_supported signal handler =
  try
    Sys.set_signal signal handler
  with Invalid_argument _ ->
    ()

let start urls sandbox git ~bare =
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
  Log.app (fun l -> l "Starting %s..." @@ Filename.basename Sys.argv.(0));
  begin match git with
    | None      -> In_memory_store.connect ()
    | Some path ->
      let prefix = if sandbox then "." else "" in
      Git_fs_store.connect ~bare (prefix ^ path)
  end >>= fun make_root ->
  Lwt_list.iter_p
    (Datakit_conduit.accept_forever ~make_root ~sandbox ~serviceid)
    urls

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

let env_docs = "ENVIRONMENT VARIABLES"

let setup_log =
  let env =
    Arg.env_var ~docs:env_docs
      ~doc:"Be more or less verbose. See $(b,--verbose)."
      "DATAKIT_VERBOSE"
  in
  Term.(const Datakit_log.setup $ Fmt_cli.style_renderer ()
        $ Datakit_log.log_destination $ Logs_cli.level ~env ())

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
       or hyperv-connect://vmid/serviceid or hyperv-accept://vmid/serviceid"
      ["url"]
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
