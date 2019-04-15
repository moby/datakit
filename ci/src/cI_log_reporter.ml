let pp_level f lvl =
  let style, msg =
    match lvl with
    | Logs.App -> (`Black, "APP")
    | Logs.Error -> (`Red, "ERR")
    | Logs.Warning -> (`Red, "WRN")
    | Logs.Info -> (`None, "INF")
    | Logs.Debug -> (`Cyan, "DBG")
  in
  Fmt.pf f "%a" Fmt.(styled style string) msg

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

module Metrics = struct
  open Prometheus

  let namespace = "DataKitCI"

  let subsystem = "logs"

  let inc_messages =
    let help = "Total number of messages logged" in
    let c =
      Counter.v_labels ~label_names:[ "level"; "src" ] ~help ~namespace
        ~subsystem "messages_total"
    in
    fun lvl src ->
      let lvl = Logs.level_to_string (Some lvl) in
      Counter.inc_one @@ Counter.labels c [ lvl; src ]
end

let report src level ~over k msgf =
  let k _ =
    over ();
    k ()
  in
  msgf @@ fun ?header:_ ?tags:_ fmt ->
  let src = Logs.Src.name src in
  Metrics.inc_messages level src;
  Format.kfprintf k Format.err_formatter
    ("%a %a [%s] @[" ^^ fmt ^^ "@]@.")
    pp_timestamp (Unix.gettimeofday ()) pp_level level src

let init style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter { Logs.report }

let setup_log =
  let open Cmdliner in
  Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())
