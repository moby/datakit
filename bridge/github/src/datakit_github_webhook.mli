(** Management of Github webhook events. *)

open Datakit_github

type t
val create: Github.Token.t -> Uri.t -> (Github_t.event -> unit Lwt.t) -> t
val run: t -> unit Lwt.t
val repos: t -> Repo.Set.t
val watch: t -> Repo.t -> unit Lwt.t
