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

(** User-defined tasks.

    A task is a high-level description of user intents.

    Tasks are later translated into more specific {{!module:Job}jobs}
    by {{!module:Worker}workers}. These jobs are then processed by
    other {{!module:Worker}workers} The user can then access the jobs
    outputs and results.
*)

type id = [`Task] Id.t
(** The type for task identifiers. These identifiers are
    deterministic, i.e. similar tasks will have the same
    identifiers. This is done by hashing the concatenation of
    {!create} arguments (after normalisation) and calling {!Id.digest}
    on the result. *)

type t
(** The type for task values. *)

val id: t -> id
(** [id t] is [t]'s deterministic identifier. Is it obtaining by
    hashing a stable representation of [t]'s components. *)

val flow: t -> Flow.t
(** [flow t]'s is the dataflow graph that the task will have to
    execute. *)

val date: t -> float
(** [date t] is [t]'s date of creation. The date is number of seconds
    since 12:00 midnight January 1, 1970, UTC without accounting for
    leap seconds with an optional timezone info. *)

val create: Flow.t -> t
(** [create flow] is the task of executing the dataflow graph [flow],
    for the current versions of the inputs. *)

val equal: t -> t -> bool
(** [equal] is the task equality. *)

val compare: t -> t -> int
(** [compare] compares tasks. *)

val pp: t Fmt.t
(** [pp] formats tasks. *)

val json: t Jsont.codec
(** [json] is the JSON codec for tasks. *)

(** {1 Task Status} *)

type status = [
  | `New
  | `Dispatched of [`Worker] Id.t * [`Pending | `Started]
  | `Pending
  | `Complete of [ `Success | `Failure ]
  | `Cancelled
]
(** The type for task status. *)

val pp_status: status Fmt.t
(** [pp_status] formats tasks {!status}. *)

val json_status: status Jsont.codec
(** [json_status] is the JSON coded for task status. *)

val status: Job.status list -> status
(** [status s] is the status summary of s. If all status are
    [`Success] then it is a [`Success]. If all status are [`Failed]
    then it is also [`Failed]. Otherwise it is [`Pending]. *)
