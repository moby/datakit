open Astring
open Cmdliner

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

let pp_ptime f () =
  let open Unix in
  let tm = Unix.localtime (Unix.time ()) in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min

let pp_mtime f () =
  let dt = Mtime.to_us (Mtime.elapsed ()) in
  Fmt.pf f "%0+04.0fus" dt

let reporter log_clock =
  let pp_time = match log_clock with
    | `Posix     -> pp_ptime
    | `Monotonic -> pp_mtime
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      Fmt.kpf k ppf ("\r%a %a %a @[" ^^ fmt ^^ "@]@.")
        pp_time ()
        Fmt.(styled `Magenta string) (Printf.sprintf "%10s" @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let setup style_renderer log_destination level log_clock =
  Logs.set_level level;
  match log_destination with
  | Eventlog ->
    let eventlog = Eventlog.register "Docker.exe" in
    Logs.set_reporter (Log_eventlog.reporter ~eventlog ())
  | Stderr ->
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_reporter (reporter log_clock)
  | ASL ->
    let facility = Filename.basename Sys.executable_name in
    let client = Asl.Client.create ~ident:"Docker" ~facility () in
    Logs.set_reporter (Log_asl.reporter ~client ())

let log_destination =
  let doc = Arg.info ~doc:"Destination for the logs" [ "log-destination" ] in
  Arg.(value & opt conv Stderr & doc)

let log_clock =
  let doc = Arg.info ~doc:"Kind of clock" ["log-clock"] in
  Arg.(value & opt (enum [
      "monotonic", `Monotonic;
      "posix"    , `Posix
    ]) `Posix doc)
