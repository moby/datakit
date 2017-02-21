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

  val metadata_commit : t -> string option
  (** [metadata_commit t] is the hash of the Git commit in github-metadata that was the build context. *)

  val source_commit : t -> string option
  (** [source_commit t] is the hash of the Git commit that triggered the build. *)

  val empty : t
  (** [empty] is a state with no jobs and no parents. *)

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

val create : unit -> t

val lookup : t -> DK.t -> CI_target.t -> target Lwt.t

val record : target -> DK.t -> source_commit:string -> DK.Commit.t -> string CI_output.t String.Map.t -> unit Lwt.t
(** [record target dk ~source_commit input jobs] records the new output of each job in [jobs]
    as a new commit of [target], and records that it was calculated using metadata snapshot [input].
    The commit index of [source_commit] is updated to include the new result (for [builds_of_commit]). *)

val load : DK.Commit.t -> State.t Lwt.t
(** [load commit] loads a saved state from the database. *)

val head : target -> State.t option
(** [head t] is the current state of [t]. *)

val builds_of_commit : DK.t -> Datakit_github.Commit.t -> DK.Commit.t CI_target.Map.t Lwt.t
(** [builds_of_commit dk c] finds the latest build results for source commit [c]. *)
