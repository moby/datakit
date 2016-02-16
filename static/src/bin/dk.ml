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
open Dk_common

let commands = [
  "show"    , "Show the tasks, jobs and workers.";
  "add"     , "Add a new task.";
  "schedule", "Schedule the tasks and jobs to the workers.";
  "work"    , "Start a new worker.";
  "publish" , "Publish the database over HTTP."
]

let default =
  let doc = "Datakit, a distributed dataflow enfine." in
  let man = [
    `S "DESCRIPTION";
    `P "TODO";
    `P "Use either $(b,$(mname) <command> --help) or $(b,$(mname) help <command>) \
        for more information on a specific command.";
  ] in
  let usage () =
    Fmt.(pf stdout)
      "usage: dk [--version]\n\
      \          [--help]\n\
      \          <command> [<args>]\n\
       \n\
       The most commonly used subcommands are:\n";
    List.iter (fun (name, doc) ->
        Fmt.(pf stdout) "    %-10s        %s\n" name doc
      ) commands;
    Fmt.(pf stdout)
      "\n\
       See `dk help <command>` for more information on a specific \
       command.\n%!"
  in
  Term.(global usage $ pure ()),
  term_info ~doc ~man "dk"

let run () =
  match Array.to_list Sys.argv with
  | [] | [_] -> ()
  | dk :: name :: args ->
    if String.length name <> 0 && name.[0] <> '-' then (
      let cmd = match dk with
        | "dk" -> "dk-" ^ name
        | s    ->
          if Filename.check_suffix s ".native" then
            let base = Filename.chop_suffix s ".native" in
            base ^ "_" ^ name ^ ".native"
          else (
            err "%s: sub-command %s not found" dk name;
            exit 1
          )
      in
      let exists =
        let test = Fmt.strf "/bin/sh -c 'command -v %s' > /dev/null 1>2" cmd in
        Sys.command test = 0
      in
      if exists then (
        let argv = Array.of_list (cmd :: args) in
        Unix.execvp cmd argv
      ) else (
        err "%s: command not found" cmd;
        exit 1
      )
    )

let commands =
  let mk (name, doc) = Term.pure (), term_info ~doc name in
  List.map mk commands

let () =
  run ();
  match Term.eval_choice default commands with
  | `Error _ -> exit 1
  | `Ok () | `Version | `Help -> ()
