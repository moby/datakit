(** Management of Github webhook events. *)

open Datakit_github

type t
val create: Github.Token.t -> Uri.t -> t
val pop: t -> Github_t.event list
val listen: t -> unit Lwt.t
val repos: t -> Repo.Set.t
val watch: t -> Repo.t -> unit Lwt.t
