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

(** Interaction with OPAM *)

type t
(** The type for OPAM state. *)

val create: root:string -> Switch.t option -> t
(** [create ~root s h] create an OPAM state using [root] as OPAM's
    root and [s] as the current switch. *)

val jobs: t -> Task.t -> (Job.t -> unit) -> unit
(** [jobs p] are the jobs needed to execute the plan [p]. *)

val atomic_jobs: t -> Task.t -> (Job.t -> unit) -> unit
(** [atomic_jobs t] is similar to {!jobs} but it builds jobs with only
    one package to install. *)

(** {1 OPAM files} *)

val read_installed: t -> Package.t list
(** [read_installed t] is the list of installed packages in on the
    current switch or [[]] if the switch does not exist. *)

val write_installed: t -> Package.t list -> unit
(** [write_installed t pkgs] update [t]'s metadata so that the packages
    [pkgs] are considered to be installed. *)

val write_pinned: t -> Task.pin list -> unit
(** [write_pinned t pkgs] update [t]'s metadata so that the packages
    [pkgs] are pinned. *)

(** {1 OPAM queries} *)

val rev_deps: t -> Package.t list -> Package.t list
(** [rev_deps t pkgs] is the list of direct reverse dependencies of
    the packages [pkgs]. Similar to {i opam list --depends-on
    [pkgs]}. *)

(** {1 OPAM commands} *)

val install: t -> Package.t list -> unit
(** [install t pkgs] is {i opam install [pkgs]}. *)

val remove: t -> Package.t list -> unit
(** [remove t pkgs] is {i opam remove [pkgs]}. *)

val switch_install: t -> unit
(** [switch_install t] is {i opam switch install [t.switch]}. *)

val update: t -> unit
(** [update t] is {i opam update}. *)

val eval_opam_config_env: t -> unit
(** [eval_opam_config_env t] is {i eval `opam config env`}. *)

val repo_clean: t -> unit
(** [repo_clean t] removes all the repositories. *)

val repo_add: t -> Task.repo list -> unit
(** [repo_add t r] is {i opam repo add r}. *)

val pin_clean: t -> unit
(** [pin_clean] removes all the pinned packages. *)

val pin_add: t -> Task.pin list -> unit
(** [repo_add t p] is {i opam pin add p}. *)

(* FIXME: review the doc *)

val get_var: t -> string -> string
