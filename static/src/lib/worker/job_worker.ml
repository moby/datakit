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

open Lwt.Infix
include Common_worker

let debug fmt =
  section := "job-worker";
  debug fmt

module SSet = Set.Make(struct
    type t = Source.id
    let compare = Id.compare
  end)

let _add_output t job src =
  let id = Job.id job in
  Store.Job.add_output (store t) id src

let default_callback _t job =
  let cmd = Job.cmd job in
  Cmd.exec cmd >|= function
  | `Ok      -> (* TODO: add_output t job output *) `Success
  | `Error _ -> `Failure

type result = [`Success | `Failure]
type callback = t -> Job.t -> result Lwt.t

let worker = worker

let start ?(callback=default_callback) =
  let callback t = function
    | `Idle
    | `Task _ -> Lwt.return_unit
    | `Job id ->
      debug "Got a new job: %s" (Id.to_string id);
      let wid = Worker.id (worker t) in
      let store = store t in
      Store.Job.ack store id wid >>= fun () ->
      Store.Job.get store id >>= fun job ->
      callback t job >>= fun result ->
      begin match result with
        | `Success -> Store.Job.success store id
        | `Failure -> Store.Job.failure store id
      end >>= fun () ->
      Store.Worker.idle store wid
  in
  start ~kind:`Job ?cache:None callback
