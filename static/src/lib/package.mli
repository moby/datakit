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

(** Package name with optional version. *)

type t
(** The type for package names with an optional version. *)

val name: t -> string
(** [name t] is [t]'s name. *)

val version: t -> string option
(** [version t] is [t]'s version or [None] it [t] does not have any
    version. *)

val create: ?version:string -> string -> t
(** [create ?version name] is the opam package [name.version].  *)

val of_string: string -> t
(** [of_string "n.v"] is the package with name [n] and version [v]. If
    [s] does not contain any string, it is the package with name [s]
    and no version. *)

val to_string: t -> string
(** [to_string t] is [name t ^ "." v] if [t] has the version [v],
    otherwise it is [name t]. *)

val equal: t -> t -> bool
(** [equal] is the equality for packages. *)

val compare: t -> t -> int
(** [compare] compares packages. *)

val json: t Jsont.codec
(** [json] is the JSON codec for packages. *)

val pp: t Fmt.t
(** [pp] formats packages. *)

(** {1 Package Metadata} *)

type meta
(** The type for package metdata. *)

val meta:
  opam:Cstruct.t ->
  ?descr:Cstruct.t ->
  ?url:Cstruct.t  ->
  ?files:(string * Cstruct.t) list ->
  t -> meta
(** [info ~opam ~descr ~url ~files t] are all the metadata needed to
    define a package. All files should contains only valid UTF-8
    characters. This is {b not} checked by CISO. *)

val pkg: meta -> t
(** [pkg m] is the package that [m] describes. *)

val opam: meta -> Cstruct.t
(** [opam m] is the contents of the {i opam} file. *)

val descr: meta -> Cstruct.t option
(** [descr m] is the contents of the {i descr} file (if present). *)

val url: meta -> Cstruct.t option
(** [url m] is the contents of the {i url} file (if present). *)

val files: meta -> (string * Cstruct.t) list
(** [files m] are the names and contents of files stored under {i
    files/} (if present). *)

val pp_meta: meta Fmt.t
(** [pp_meta] formats package metadata. *)

val json_meta: meta Jsont.codec
(** [json_meta] is the JSON codec for package metadata. *)
