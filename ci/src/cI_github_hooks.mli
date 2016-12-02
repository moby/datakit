open Datakit_github
open CI_utils
open! Result

type t

module CI : sig
  type t = string list
  val circle_ci : t
  val datakit_ci : string -> t
end

val connect : DK.t -> t
val update_status: t -> message:string -> Status.t -> unit Lwt.t

module Target : sig
  type t = [ `PR of PR.t | `Ref of Ref.t ]
  val head : t -> Commit.t
end

module Snapshot : sig
  type t
  val prs: t -> Repo.t -> PR.t PR.Index.t Lwt.t
  val refs: t -> Repo.t -> Ref.t Ref.Index.t Lwt.t
  val pr: t -> PR.id -> PR.t option Lwt.t
  val ref: t -> Ref.id -> Ref.t option Lwt.t
  val status: t -> Status.id -> Status.t option Lwt.t
  val target: t -> CI_target.t -> Target.t option Lwt.t
end

val snapshot : t -> Snapshot.t Lwt.t
(** [snapshot t] is a snapshot of the current head of the metadata branch. *)

val enable_monitoring : t -> Repo.t list -> unit Lwt.t
(** [enable_monitoring t projects] ensures that a [".monitor"] file exists for each project in [projects], creating them as needed. *)

val monitor : t -> ?switch:Lwt_switch.t -> (Snapshot.t -> unit Lwt.t) -> [`Abort] Lwt.t
(** [monitor t fn] is a thread that watches the "github-metadata" branch and calls [fn snapshot] on each update.
    Returns [`Abort] when the switch is turned off. *)
