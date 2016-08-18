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

module Repo: sig

  type t = { user: string; repo: string }
  (** The type for Github repositories. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for Github repositories. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
  end
  (** Sets of repositories. *)

end

module PR: sig

  (** The type for pull-requests values. *)
  type t = {
    user: string;
    repo: string;
    number: int;
    state: [`Open | `Closed];
    head: string; (* SHA1 *)
    title: string;
  }

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for pull-request values. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
  end
  (** Sets of pull requests. *)

end

module Status: sig

  (** The type for status values. *)
  type t = {
    user: string;
    repo: string;
    commit: string;
    context: string list;
    url: string option;
    description: string option;
    state: Status_state.t;
  }

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for status values. *)

  val path: t -> Datakit_path.t
  (** [path t] is path corresponding to [t]'s context. The empty list
      is rewritten into ["default"] to match the GitHub
      API. Otherwise, segments are concatenated using ["/"] as a
      separator. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
  end
  (** Sets of build status. *)

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

  val user_exists: token -> user:string -> bool Lwt.t
  (** [exist_user t ~user] is true iff [user] exists. *)

  val repo_exists: token -> user:string -> repo:string -> bool Lwt.t
  (** [exists_repo t ~user ~repo] is true iff [user/repo] exists. *)

  val repos: token -> user:string -> string list Lwt.t
  (** [repos t ~user] is the list of repositories owned by user
      [user]. *)

  val status: token -> user:string -> repo:string -> commit:string ->
    Status.t list Lwt.t
  (** [status t ~user ~repo ~commit] returns the list of status
      attached to [commit]. *)

  val set_status: token -> Status.t -> unit Lwt.t
  (** [set_status t s] updates [Status.commit s]'s status with [s]. *)

  val set_pr: token -> PR.t -> unit Lwt.t
  (** [set_pr t pr] updates the PR number [PR.number pr] with [pr]. *)

  val prs: token -> user:string -> repo:string -> PR.t list Lwt.t
  (** [pr t ~user ~repo] is the list of open pull-requests for the
      repo [user/repo]. *)

  val events: token -> user:string -> repo:string -> Event.t list Lwt.t
  (** [event t ~user ~repo] is the list of events attached to
      [user/repo]. Note: can be slow/costly if multiple pages of
      events. *)

end

module Snapshot: sig

  (** {1 File-system snapshots} *)

  type t
  (** The type for filesystem snapshots. *)

  val empty: t
  (** The empty snapshot. *)

  val create:
    ?repos:Repo.Set.t -> status:Status.Set.t -> prs:PR.Set.t -> unit -> t
  (** [create ?repos ~status ~prs ()] is a new snapshot [t] with
      pull-requests [prs], build status [status] and repositories the
      unions of [repos], the repositories of [status] and [prs]. *)

  val union: t -> t -> t
  (** [union x y] is the union of the snapshots [x] and [y]. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for snapshots. *)

  val compare: t -> t -> int
  (** [compare] is the comparison function for snapshots. *)

  val repos: t -> Repo.Set.t
  (** [repos t] are [t]'s repository. *)

  val prs: t -> PR.Set.t
  (** [prs t] are [t]'s pull-requests. *)

  val status: t -> Status.Set.t
  (** [status t] are [t]'s build status. *)

end

module Diff: sig

  (** {1 Github diffs} *)

  type t = {
    user: string;
    repo: string;
    id  : [ `PR of int | `Status of string * string list | `Unknown ]
  }
  (** The type for filesystem diffs. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for diff values. *)

  val compare: t -> t -> int
  (** [compare] is the comparison function for diff values. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
  end
  (** Set of changes. *)

  val changes: Datakit_path.t Datakit_S.diff list -> Set.t
  (** [changes d] is the set of GitHub changes carried over in the
      filesystem changes [d]. *)

  val repos: Datakit_path.t Datakit_S.diff list -> Repo.Set.t
  (** [repos d] is the set of repositories appearing in [d]. *)

end

(** Conversion between GitHub and DataKit states. *)
module Conv (DK: Datakit_S.CLIENT): sig

  type nonrec 'a result = ('a, DK.error) Result.result Lwt.t
  (** The type for conversion results. *)

  (** {1 Trees} *)

  type tree
  (** The type for readable filesystem trees. *)

  val tree_of_transaction: DK.Transaction.t -> tree
  (** [tree_of_transaction t] is [t]'s filesystem {!tree}. *)

  val tree_of_commit: DK.Commit.t -> tree
  (** [tree_of_commit c] is [c]'s filesystem {!tree}. *)

  (** {1 Repositories} *)

  val repos: tree -> Repo.Set.t result
  (** [repos t] is the list of repositories stored in [t]. *)

  (** {1 Status} *)

  val status: tree ->
    user:string -> repo:string -> commit:string -> context:string list ->
    Status.t option result
  (** [status t ~user ~repo ~commit ~context] is the commit's
      build status for the repository [user/repo] in the tree [t]. *)

  val statuses: ?repos:Repo.Set.t -> tree -> Status.Set.t result
  (** [statuses t] is the list of status stored in [t].. *)

  val update_status: DK.Transaction.t -> Status.t -> unit result
  (** [update_status t ~user ~repo s] applies the status [s] of the
      repository [user/repo] to the transaction [t]. *)

  (** {1 Pull requests} *)

  val pr: tree -> user:string -> repo:string -> int -> PR.t option result
  (** [pr t ~user ~repo n] is the [n]'th pull-request of the repostiry
      [user/repo] in [t]. *)

  val prs: ?repos:Repo.Set.t -> tree -> PR.Set.t result
  (** [prs t] is the list of pull requests stored in [t]. *)

  val update_pr: DK.Transaction.t -> PR.t -> unit result
  (** [update_pr t ~user ~repo pr] applies the pull-request [pr] of
      the repository [user/repo] to the transaction [t]. *)

  (** {1 Snapshots and diffs} *)

  val diff: tree -> DK.Commit.t -> Diff.Set.t result
  (** [diff tree c] computes the Github diff between the branch [b]
      and the commit [c]. *)

  val snapshot: ?old:(DK.Commit.t * Snapshot.t) -> tree -> Snapshot.t result
  (** [snapshot ?old t] is a snapshot of the tree [t]. Note: this is
      expensive, so try to provide a previous (recent) snapshot [prev]
      if possible. *)

  val apply: Snapshot.t -> (tree * Diff.Set.t) -> Snapshot.t result
  (** [apply s d] is the snapshot obtained by applying [d] on top of
      [s]. [d] is a pair [tree * diff] where [diff] contains the
      pull-requests and status to consider while [tree] is holding the
      respective state of these objects. *)

end

module Sync (API: API) (DK: Datakit_S.CLIENT): sig

  type t
  (** The type for synchronizer state. *)

  val empty: t
  (** Create an empty sync state. *)

  val sync:
    ?switch:Lwt_switch.t -> ?policy:[`Once|`Repeat] -> ?dry_updates:bool ->
    pub:DK.Branch.t -> priv:DK.Branch.t -> token:API.token ->
    t -> t Lwt.t
(** [sync t ~pub ~priv ~token] mirror GitHub changes in the DataKit
    public branch [pub]. It uses the private branch [priv] to store
    the received webhook event states. It connects to the GitHub API
    using the token [tok]. The default [policy] is [`Repeat]. If
    [dry_updates] is set (by default it is not), do not do the update
    API calls but print in the logs (with an [Logs.App] level) them
    instead. *)

end
