open Datakit_github

module Make (API: API) (DK: Datakit_client.S): sig

  type t
  (** The type for synchronizer state. *)

  val empty: t
  (** Create an empty sync state. *)

  (** [sync ~token b t] mirror GitHub changes in the DataKit branch
      [b]. The GitHub API calls use the token [token]. The default
      [policy] is [`Repeat] and [cap] is [Cap.all]. *)
  val sync:
    token:API.token -> ?webhook:API.Webhook.t -> ?resync_interval:float ->
    ?switch:Lwt_switch.t -> ?policy:[`Once|`Repeat] -> ?cap:Capabilities.t ->
    DK.Branch.t -> t -> t Lwt.t

end
