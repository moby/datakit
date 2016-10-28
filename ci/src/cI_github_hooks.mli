open CI_utils
open! Result

type t

type snapshot

module Commit_state : sig
  type t

  val status : t -> Datakit_S.status_state option Lwt.t
  val descr : t -> string option Lwt.t
  val target_url : t -> Uri.t option Lwt.t
end

module CI : sig
  type t
  val of_string : string -> t
  val circle_ci : t
  val datakit_ci : string -> t
end

module Commit : sig
  type t

  val hash : t -> string
  val pp : t Fmt.t
  val state : CI.t -> t -> Commit_state.t
  val project : t -> CI_projectID.t
end

module PR : sig
  type t

  val id : t -> int
  val head : t -> Commit.t
  val title : t -> string
  val project : t -> CI_projectID.t
  val dump : t Fmt.t
  val compare : t -> t -> int
end

module Ref : sig
  type t

  val project : t -> CI_projectID.t
  val name : t -> Datakit_path.t
  val head : t -> Commit.t
  val dump : t Fmt.t
  val compare : t -> t -> int
end

val connect : DK.t -> t

val pr : t -> project_id:CI_projectID.t -> int -> PR.t option Lwt.t

val set_state : t -> CI.t -> status:Datakit_S.status_state -> descr:string -> ?target_url:Uri.t -> Commit.t -> unit Lwt.t

val project : snapshot -> CI_projectID.t -> (PR.t list * Ref.t list) Lwt.t
(** [project snapshot p] is the state of the open PRs, branches and tags in [snapshot] for project [p]. *)

val snapshot : t -> snapshot Lwt.t
(** [snapshot t] is a snapshot of the current head of the metadata branch. *)

val monitor : t -> ?switch:Lwt_switch.t -> (snapshot -> unit Lwt.t) -> [`Abort] Lwt.t
(** [monitor t fn] is a thread that watches the "github-metadata" branch and calls [fn snapshot] on each update.
    Returns [`Abort] when the switch is turned off. *)
