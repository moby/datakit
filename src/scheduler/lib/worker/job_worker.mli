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

(** Job worker.

    A worker has a fixed host configuration. It builds OPAM packages
    and store the resultsc.

*)

type t
(** The type for job workers. *)

val worker: t -> Worker.t
(** [worker t] is [t]'s worker value. *)

type result = [`Success | `Failure]
(** The type for job results. *)

type callback = t -> Job.t -> result Lwt.t
(** The type for job workers' callback. *)

val default_callback: callback
(** [default_callback] is the function which builds the jobs using
    opam invocations. *)

val start: ?callback:callback -> ?tick:float -> Store.t -> t Lwt.t
(** [starts store] starts a job worker. *)

val stop: t -> unit Lwt.t
(** [stop t] stops the job worker. *)
