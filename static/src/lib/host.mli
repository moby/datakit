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

(** Detection of host configuration. *)

type t
(** The type for host configuration. *)

type id = [`Host] Id.t
(** The type for deterministic host identifiers. *)

val id: t -> id
(** [id t] is [t]'s deterministic identifier. Is it obtaining by
    hashing a stable representation of [t]'s components. *)

val detect: unit -> t
(** Detects the host configuration. *)

val compare: t -> t -> int
(** [compare] compares host configurations. *)

val equal: t -> t -> bool
(** [equal] is the equality for host configurations. *)

val short: t -> string
(** [short t] is the short represention of [t], useful to be displayed
    on a logging line. *)

val pp: t Fmt.t
(** [pp] formats a {{!t}host configuration}. *)

val json: t Jsont.codec
(** [json] is the JSON codec for host configurations. *)

val defaults: t list
(** [defaults] is the list of host configurations supported by
    default. *)

type os = [
  | `Darwin
  | `Linux
  | `Unix
  | `FreeBSD
  | `OpenBSD
  | `NetBSD
  | `DragonFly
  | `Win32
  | `Cygwin
  | `Other of string
]
(** The type for OS configuration. *)

val pp_os: os Fmt.t
(** [pp_os] format OS configurations. *)

val os: t -> os
(** [os t] is [t]'s OS. *)
