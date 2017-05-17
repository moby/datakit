open Lwt.Infix

let src = Logs.Src.create "bridge-local-git" ~doc:"Local Git bridge for Datakit"
module Log = (val Logs.src_log src : Logs.LOG)

let src9p = Logs.Src.create "bridge-local-git.9p" ~doc:"Local Git bridge for Datakit (9p)"
module Log9p = (val Logs.src_log src9p : Logs.LOG)
module Client9p = Protocol_9p_unix.Client9p_unix.Make(Log9p)
module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)
module DK = Datakit_client_9p.Make(Client9p)

module Sync = Sync.Make(Store)(DK)

let failf fmt =
  Fmt.kstrf failwith fmt

let start () (protocol, address) repos =
  Log.info (fun f -> f "Connecting to DataKit server on %s:%s" protocol address);
  Irmin.Private.Watch.set_listen_dir_hook Irmin_watcher.hook;
  Lwt_main.run begin
    Lwt.catch
      (fun () ->
         Client9p.connect ~send_pings:true protocol address () >|= function
         | Ok c -> c
         | Error (`Msg m) -> failwith m
      )
      (fun ex ->
         failf "Failed to connect to DataKit server at proto=%S addr=%S: %s"
           protocol address (Printexc.to_string ex)
      )
    >|= DK.connect >>= fun dk ->
    repos |> Lwt_list.map_p (fun (name, root) ->
        Log.info (fun f -> f "Monitoring local repository %S" root);
        let config = Irmin_git.config root ~bare:true in
        Store.Repo.v config >|= fun store -> (name, store)
      )
    >>= Sync.run dk
  end

(* Command-line parsing *)

open Cmdliner

let datakit_endpoint =
  let doc =
    Arg.info ~doc:"DataKit store for metadata."
      ~docv:"ADDR" ["metadata-store"]
  in
  Arg.(value (opt (pair ~sep:':' string string) ("tcp","localhost:5640") doc))

let pp_level f lvl =
  let style, msg = match lvl with
    | Logs.App -> `Black, "APP"
    | Logs.Error -> `Red, "ERR"
    | Logs.Warning -> `Red, "WRN"
    | Logs.Info -> `None, "INF"
    | Logs.Debug ->  `Cyan, "DBG"
  in
  Fmt.pf f "%a" Fmt.(styled style string) msg

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let report src level ~over k msgf =
  let k _ = over (); k () in
  msgf @@ fun ?header:_ ?tags:_ fmt ->
  let src = Logs.Src.name src in
  Format.kfprintf k Format.err_formatter ("%a %a [%s] @[" ^^ fmt ^^ "@]@.")
    pp_timestamp (Unix.gettimeofday ())
    pp_level level
    src

let init style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.Src.set_level src9p (Some Logs.Info);
  Logs.set_reporter { Logs.report }

let setup_log =
  Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let repo_id =
  let parse s =
    match Datakit_github.Repo.of_string s with
    | None -> `Error (Fmt.strf "Bad repository name %S (format should be user/project)" s)
    | Some x -> `Ok x
  in
  (parse, Datakit_github.Repo.pp)

let git_dir =
  let parse, pp = Arg.dir in
  let parse s =
    match parse (Filename.concat s ".git") with
    | `Ok _ -> `Ok s
    | `Error _ as e -> e
  in
  (parse, pp)

let repo = Arg.(pair ~sep:':' repo_id git_dir)

let repos =
  let doc = Arg.info []
      ~doc:"A Git repository to monitor and the name to use for it. e.g. 'my/my-project:/tmp/my-project'"
      ~docv:"NAME:PATH" in
  Arg.(non_empty @@ pos_all repo [] doc)

let main =
  let doc = "Bridge between a local Git repository and Datakit." in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) is a local replacement for datakit-github. \
        It allows you to test DataKitCI against a local Git repository \
        without having to configure GitHub integration first.";
  ] in
  Term.(pure start $ setup_log $ datakit_endpoint $ repos),
  Term.info (Filename.basename Sys.argv.(0)) ~doc ~man

let () = match Term.eval main with
  | `Error _ -> exit 1
  | `Help | `Version | `Ok () -> ()
