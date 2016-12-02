open Datakit_github

module CI : sig
  type t = string list
  val circle_ci : t
  val datakit_ci : string -> t
end

module Snapshot : sig
  type t = CI_utils.DK.Tree.t
  val prs: t -> Repo.t -> PR.t PR.Index.t Lwt.t
  val refs: t -> Repo.t -> Ref.t Ref.Index.t Lwt.t
  val pr: t -> PR.id -> PR.t option Lwt.t
  val ref: t -> Ref.id -> Ref.t option Lwt.t
  val status: t -> Status.id -> Status.t option Lwt.t
  val target: t -> CI_target.t -> CI_target.v option Lwt.t
end
