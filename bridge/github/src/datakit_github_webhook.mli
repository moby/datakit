(** Management of Github webhook events. *)

open Datakit_github

type t
(** The type for the webhook server state. *)

val create: Uri.t -> (Github_t.event -> unit Lwt.t) -> t
(** [create uri f] is a pair is the webhook server state configured to
    listen for incoming webhook events to the public address
    [uri]. Every new event [e] will trigger an asynchronous call to [f
    e].  *)

val listen: t -> unit Lwt.t
(** [listen t] runs the webook listener. *)

val repos: t -> Repo.Set.t
(** The list of watched repository. *)

val watch: t -> Github.Token.t -> Repo.t -> unit Lwt.t
(** [watch t tok r] makes [t] watch the repo [r] using the token
    [tok]. *)
