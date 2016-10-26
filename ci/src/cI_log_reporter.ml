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
  Format.kfprintf k Format.err_formatter ("%a %a [%s] @[" ^^ fmt ^^ "@]@.")
    pp_timestamp (Unix.gettimeofday ())
    pp_level level
    (Logs.Src.name src)

let init style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter { Logs.report }

let setup_log =
  let open Cmdliner in
  Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())
