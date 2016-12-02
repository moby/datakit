open Asetmap
open Datakit_github
open CI_utils
open! Result

type t

module Commit_state : sig
  type t

  val status : t -> Status_state.t option Lwt.t
  val descr : t -> string option Lwt.t
  val target_url : t -> Uri.t option Lwt.t
end

module CI : sig
  type t = string list
  val circle_ci : t
  val datakit_ci : string -> t
end

module Commit : sig
  type t

  val hash : t -> string
  val pp : t Fmt.t
  val state : string list -> t -> Commit_state.t
  val repo : t -> Repo.t
end

module PR : sig
  type t

  val id : t -> int
  val head : t -> Commit.t
  val title : t -> string
  val repo : t -> Repo.t
  val dump : t Fmt.t
  val compare : t -> t -> int
  module Index: Map.S with type key = int
end

module Ref : sig
  type t

  val repo : t -> Repo.t
  val name : t -> string list
  val head : t -> Commit.t
  val dump : t Fmt.t
  val compare : t -> t -> int
  val pp_name: string list Fmt.t
  module Index: Map.S with type key = string list
end

val connect : DK.t -> t

val pr : t -> repo:Repo.t -> int -> PR.t option Lwt.t

val set_state : t -> string list -> status:Status_state.t -> descr:string ->
  ?target_url:Uri.t -> message:string -> Commit.t -> unit Lwt.t

module Target : sig
  type t = [ `PR of PR.t | `Ref of Ref.t ]

  val head : t -> Commit.t
  val dump : t Fmt.t
end

module Snapshot : sig
  type t

  val repo: t -> Repo.t -> (PR.t PR.Index.t * Ref.t Ref.Index.t) Lwt.t
  (** [repo snapshot r] is the state of the open PRs, branches and
      tags in [snapshot] for repository [r]. *)

  val find : CI_target.t -> t -> Target.t option Lwt.t
end

val snapshot : t -> Snapshot.t Lwt.t
(** [snapshot t] is a snapshot of the current head of the metadata branch. *)

val enable_monitoring : t -> Repo.t list -> unit Lwt.t
(** [enable_monitoring t projects] ensures that a [".monitor"] file exists for each project in [projects], creating them as needed. *)

val monitor : t -> ?switch:Lwt_switch.t -> (Snapshot.t -> unit Lwt.t) -> [`Abort] Lwt.t
(** [monitor t fn] is a thread that watches the "github-metadata" branch and calls [fn snapshot] on each update.
    Returns [`Abort] when the switch is turned off. *)
