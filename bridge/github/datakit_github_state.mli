open Datakit_github

(** API State: TODO find a better name? *)
module Make (API: API): sig

  (** {1 Token} *)

  type token
  (** The type for state token. *)

  val token: API.token -> Capabilities.t -> token
  (** [token t c] is the token using the GitHub API token [t] limited
      by the capabilities [c]. *)

  val capabilities: token -> Capabilities.t
  (** [capabilities t] is the token [t]'s capabilities. *)

  val with_capabilities: Capabilities.t -> token -> token
  (** [with_capabilities c t] is [t] with the capabilities [c]. *)

  (** {1 Synchronisation} *)

  val import: token -> Snapshot.t -> Elt.IdSet.t -> Snapshot.t Lwt.t
  (** [import token t r] imports the state of GitHub for the elements
      in [r] into [t]. API calls use the token [token]. *)

  val apply: token -> Diff.t -> unit Lwt.t
  (** [apply token d] applies the snapshot diff [d] as a series of
      GitHub API calls, using the token [token]. *)

  (** {1 Webhooks} *)

  val add_webhooks:
    token -> watch:(Repo.t -> unit Lwt.t) -> Repo.Set.t -> unit Lwt.t
  (** [add_webhooks t rs] adds webhooks for the repositories [rs]. *)

  val import_webhook_events:
    token -> events:(unit ->  Event.t list Lwt.t) -> Snapshot.t -> Snapshot.t Lwt.t
  (** [import_webhook_events t ~events s] applies [events ()] on top
      of [s]. Note: it ensure that all the metadata are correctly
      updated by inserting (possibly) missing events in the mix. For
      instance, GitHub never sends {{!Event.Status}status} events, so
      [import_events] has to reconstruct them. *)

end
