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

let setup style_renderer log_destination level =
  Logs.set_level level;
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

let log_destination =
  let doc = Arg.info ~doc:"Destination for the logs" [ "log-destination" ] in
  Arg.(value & opt conv Stderr & doc)
