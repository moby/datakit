(** Virtual filesystem for the GitHub API. *)

(** Signature for status states. *)
module Status_state: sig

  type t = [ `Error | `Pending | `Success | `Failure ]
  (** The type for status states. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for status states. *)

  val to_string: t -> string
  (** [to_string v] is the string represenation of [v]. *)

  val of_string: string -> t option
  (** [of_string s] is the value v such that [of_string s] is [Some
      v]. *)
end

module PR: sig

  (** The type for pull-requests values. *)
  type t = {
    number: int;
    state: [`Open | `Closed];
    head: string; (* SHA1 *)
  }

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for pull-request values. *)

end

module Status: sig

  (** The type for status values. *)
  type t = {
    context: string option;
    url: string option;
    description: string option;
    state: Status_state.t;
    commit: string;
  }

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for status values. *)

end

module Event: sig

  (** The type for event values. *)
  type t =
    | PR of PR.t
    | Status of Status.t
    | Other of string

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for event values. *)

end

(** Signature for the GitHub API. *)
module type API = sig

  type token
  (** The type for API tokens. *)

  val user_exists: token -> user:string -> bool
  (** [exist_user t ~user] is true iff [user] exists. *)

  val repo_exists: token -> user:string -> repo:string -> bool
  (** [exists_repo t ~user ~repo] is true iff [user/repo] exists. *)

  val repos: token -> user:string -> string list
  (** [repos t ~user] is the list of repositories owned by user
      [user]. *)

  val status: token -> user:string -> repo:string -> commit:string ->
    Status.t list
  (** [status t ~user ~repo ~commit] returns the list of status
      attached to [commit]. *)

  val set_status: token -> user:string -> repo:string ->  Status.t -> unit
  (** [status t ~user ~repo s] updates [Status.commit s]'s status with
      [s]. *)

  val prs: token -> user:string -> repo:string -> PR.t list
  (** [pr t ~user ~repo] is the list of open pull-requests for the
      repo [user/repo]. *)

  val events: token -> user:string -> repo:string -> Event.t list Lwt.t
  (** [event t ~user ~repo] is the list of events attached to
      [user/repo]. Note: can be slow/costly if multiple pages of
      events. *)

end

module Make (API: API): sig

  val create: API.token -> Vfs.Inode.t
  (** [create token] is the virtual filesystem in which GitHub API calls
      are replaced to filesystem accesses. *)

end

module Sync (API: API) (DK: Datakit_S.CLIENT): sig

  type t
  (** The type for synchronizer state. *)

  val empty: t
  (** Create an empty sync state. *)

  val sync: ?switch:Lwt_switch.t -> ?policy:[`Once|`Repeat] ->
    t -> DK.Branch.t -> writes:DK.Branch.t -> API.token -> t Lwt.t
  (** [sync t b ~writes tok] mirror GitHub changes in the DataKit
      branch [b] and translate change requests from the branch
      [writes] into GitHub API calls. It connects to the GitHub API
      using the token [tok]. The default [policy] is [`Repeat]. *)

end
