(** Management of Github webhook events. *)

type t
(** The type for the webhook server state. *)

val serve: Uri.t -> (Github_t.event -> unit Lwt.t) -> t * (unit -> unit Lwt.t)
(** [serve uri f] is a pair [(t, l)] where [t] is the webhook server
    state and [l ()] an Lwt blocking thread listening for incoming
    webhook events to the public address [uri]. Every new event [e]
    trigger an asynchronous call to [f e].  *)

val watch: t -> dry_updates:bool -> Github.Token.t -> Datakit_github.Repo.t ->
  unit Lwt.t
(** [watch t tok r] makes [t] watch the repo [r] using the token
    [tok]. *)
