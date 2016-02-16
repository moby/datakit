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

(** Build jobs. *)

type id = [`Job] Id.t
(** The type for job identifiers. Job identifiers are deterministic,
    i.e. similar jobs will have the same identifiers. As for
    {{!Task.id}tasks}, the identifier is built by calling {!Id.digest}
    on the concatenation of {!create} arguments (after
    normalisation). *)

type t
(** The type for job values. *)

val create: ?inputs: id list -> Cmd.t -> t
(** [create ?inputs cmd] is the job of running the command [cmd] once
    the given [inputs] are ready. *)

val id: t -> id
(** [id t] id [t]'s deterministic identifier. It is obtained by hasing
    a stable representation of [t]'s components. *)

val cmd: t -> Cmd.t
(** [cmd t] is [t]'s command to run. *)

val inputs: t -> id list
(** [input t] are [t]'s job inputs. *)

val equal: t -> t -> bool
(** [equal] is the job equality. *)

val compare: t -> t -> int
(** [compare] compares jobs. *)

val hash: t -> int
(** [hash] hashes jobs. *)

val json: t Jsont.codec
(** [json] is the JSON codec for jobs. *)

val pp: t Fmt.t
(** [pp] formats jobs. *)

(** {Job Status} *)

type status = [
  | `Pending
  | `Runnable
  | `Dispatched of [`Worker] Id.t * [`Pending | `Started]
  | `Complete of [`Success | `Failure]
  | `Cancelled
]
(** The type for job status. *)

val json_status: status Jsont.codec
(** [json_status] is the JSON codec for job status. *)

val pp_status: status Fmt.t
(** [pp_status] formats jobs {!status}. *)
