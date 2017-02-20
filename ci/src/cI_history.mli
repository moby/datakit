open Astring
open CI_utils

type t
(** A cache of states. *)

type target
(** A mutable holder for the current state of a target. *)

module State : sig
  type t
  (** An immutable snapshot of a target's state. *)

  val parents : t -> string list
  (** [parents t] is the list of hashes of [t]'s parent commits. *)

  val jobs : t -> string CI_output.t String.Map.t
  (** [jobs t] returns the list of jobs and their outputs at [t]. *)

  val empty : t
  (** [empty] is a state with no jobs and no parents. *)

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

val create : unit -> t

val lookup : t -> DK.t -> CI_target.v -> target Lwt.t

val record : target -> DK.t -> DK.Commit.t -> string CI_output.t String.Map.t -> unit Lwt.t
(** [record target dk input jobs] records the new output of each job in [jobs]
    as a new commit of [target], and records that it was calculated using metadata snapshot [input]. *)

val load : DK.Commit.t -> State.t Lwt.t
(** [load commit] loads a saved state from the database. *)

val head : target -> State.t option
(** [head t] is the current state of [t]. *)
