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
include Dk_common

let main =
  let mk store =
    let flow = failwith "TODO" in
    let task = Task.create flow in
    Lwt_main.run begin
      store >>= fun store ->
      Store.Task.add store task >|= fun () ->
      Fmt.(pf stdout) "Task %a added!\n"
        Fmt.(styled `Cyan Id.pp) (Task.id task)
    end
  in
  Term.(global mk $ store),
  term_info ~doc:"Add new tasks to datakit" "dk-add" ~man:[`P "TODO"]

let () =
  match Term.eval main with
  | `Error _ -> exit 1
  | `Ok () | `Version | `Help -> ()
