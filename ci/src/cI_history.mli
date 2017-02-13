open Astring
open CI_utils

type t
(** A cache of states. *)

type target
(** A mutable holder for the current state of a target. *)

type commit
(** An immutable snapshot of a target's state. *)

val create : unit -> t

val lookup : t -> DK.t -> CI_target.v -> target Lwt.t

val record : target -> DK.t -> string -> DK.Commit.t -> string CI_output.t -> unit Lwt.t
(** [record target dk job input output] records [job/output] as a new commit of [target], and
    records that it was calculated using metadata snapshot [input]. *)

val load : DK.Commit.t -> commit Lwt.t
(** [load commit] loads a saved commit from the database. *)

val parents : commit -> string list

val head : target -> commit option
(** [head t] is the current state of [t]. *)

val jobs : commit -> string CI_output.t String.Map.t
(** [jobs commit] returns the list of jobs and their outputs at [commit]. *)

val empty : commit
(** [empty] is a commit with no jobs and no parents. *)
