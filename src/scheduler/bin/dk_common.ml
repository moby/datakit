(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Cmdliner
open Lwt.Infix

let info x y =
  if !Gol.verbose then Fmt.(pf stdout "%a %s\n%!" (styled `Cyan string) x y)

let err fmt =
  Fmt.kstrf (fun str ->
      Fmt.(pf stderr "%a %s\n%!" (styled `Red string) "Error:" str)
    ) fmt

let () =
  Irmin_unix.install_dir_polling_listener 0.2;
  Fmt.(set_style_renderer stdout `Ansi_tty);
  Fmt.(set_style_renderer stderr `Ansi_tty)

let cache =
  let doc = "The location of the datakit cache." in
  Arg.(value & opt (some string) None & info ["c";"cache"] ~docv:"DIR" ~doc)

let local =
  let doc = "The path to the local datakit store."in
  Arg.(value & opt (some string) None & info ["local"] ~docv:"DIR" ~doc)

let global =
  let doc = "The URI of the global datakit store." in
  Arg.(value & opt (some string) None & info ["global"] ~docv:"URI" ~doc)

let err_invalid_line l = err "invalid line: %S" l

let config_file () =
  let dot_datakit = ".datakit" in
  if not (Sys.file_exists dot_datakit) then Lwt.return (fun _ -> None)
  else
    let lines = Lwt_io.lines_of_file dot_datakit in
    Lwt_stream.to_list lines >|= fun lines ->
    let kvs =
      List.fold_left (fun acc l ->
          if String.length l = 0 || l.[0] = '#' then acc
          else match Stringext.cut l ~on:" " with
            | Some (k, v) -> (k, v) :: acc
            | _ -> err_invalid_line l; acc
        ) [] lines
    in
    let find name =
      try
        let v = List.assoc (name ^ ":") kvs in
        Some (String.trim v)
      with Not_found ->
        None
    in
    find

let choose_store local global = match local, global with
  | Some l, _    -> info "local " l ; Store.local ~root:l ()
  | None, Some r -> info "remote" r; Store.remote ~uri:(Uri.of_string r) ()
  | None, None   -> err "no store specified!"; exit 1

let store =
  let mk local global =
    match local, global with
    | None, None ->
      config_file () >>= fun config ->
      info "config" ".datakit";
      choose_store (config "local") (config "global")
    | _ -> choose_store local global
  in
  Term.(pure mk $ local $ global)

let block _ =
  let t, _ = Lwt.task () in
  t

(* Global options *)
type global = { verbose: bool; }

let app_global g = Gol.verbose := g.verbose

(* Help sections common to all commands *)
let global_option_section = "COMMON OPTIONS"
let help_sections = [
  `S global_option_section;
  `P "These options are common to all commands.";
  `S "AUTHORS";
  `P "Thomas Gazagnaire   <thomas@gazagnaire.org>"; `Noblank;
  `P "David Sheets        <sheets@alum.mit.edu>"; `Noblank;
  `P "Qi Li               <liqi0425@gmail.com>";
  `S "BUGS";
  `P "Check bug reports at https://github.com/docker/datakit/issues.";
]

let global_t =
  let verbose =
    let doc =
      Arg.info ~docs:global_option_section ~doc:"Be verbose." ["v";"verbose"] in
    Arg.(value & flag & doc)
  in
  Term.(pure (fun verbose -> { verbose }) $ verbose)

let term_info ~doc ?(man=[]) title =
  let man = man @ help_sections in
  Term.info
    ~version:Version.current ~sdocs:global_option_section ~doc ~man title

let global f =
  let g global f = app_global global; f in
  Term.(pure g $ global_t $ pure f)
