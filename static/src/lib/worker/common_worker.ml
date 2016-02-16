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

let section = ref "worker"
let debug fmt = Gol.debug ~section:!section fmt

type t = {
  worker   : Worker.t;                           (* the worker configuration. *)
  store    : Store.t;                                     (* the Irmin store. *)
  opam_root: string;                          (* the OPAM root of the worker. *)
  tick     : float;                  (* how often do the worker need to tick. *)
  mutable heartbeat: int option;
  mutable stop: unit -> unit Lwt.t;                    (* stop the scheduler. *)
}

let opam t s = Opam.create ~root:t.opam_root s
let store t = t.store
let opam_root t = t.opam_root
let worker t = t.worker

let create ~tick ~store ~opam_root worker =
  let stop () = Lwt.return_unit in
  let heartbeat = None in
  { worker; store; opam_root; tick; stop; heartbeat; }

let pids = ref []
let add_to_kill pid = pids := pid :: !pids
let remove_from_kill pid = pids := List.filter ((<>)pid) !pids
let kill pid = Unix.kill pid Sys.sigkill

let () =
  at_exit (fun () -> List.iter kill !pids)

let kill_child t = match t.heartbeat with
  | None     -> ()
  | Some pid -> remove_from_kill pid; kill pid

let execution_loop t fn =
  Store.Worker.watch_status t.store (Worker.id t.worker) (function
      | Some s -> fn t s
      | None   ->
        Fmt.(pf stdout "%a" (styled `Cyan string) "Killed!\n");
        kill_child t;
        exit 1
    )

let heartbeat_loop t =
  let rec beat () =
    debug "tick %.0fs" t.tick;
    let id = Worker.id t.worker in
    Lwt_main.run (Store.Worker.tick t.store id (Unix.time ()));
    Unix.sleep (int_of_float t.tick);
    beat ()
  in
  Lwt_io.flush_all () >>= fun () ->
  match Lwt_unix.fork () with
  | 0   ->
    Store.cancel_all_watches t.store >>= fun () ->
    beat ()
  | pid ->
    t.heartbeat <- Some pid;
    add_to_kill pid;
    Lwt.return_unit

let start fn ?host ?(tick=5.) ~opam_root ~kind store =
  let host = match host with None -> Host.detect () | Some h -> h in
  let w = Worker.create kind host in
  let t = create ~tick ~store ~opam_root w in
  Store.Worker.add t.store w >>= fun () ->
  heartbeat_loop t >>= fun () ->
  execution_loop t fn >|= fun cancel ->
  t.stop <- cancel;
  t

let stop t =
  t.stop () >>= fun () ->
  Store.Worker.forget t.store (Worker.id t.worker) >|= fun () ->
  kill_child t
