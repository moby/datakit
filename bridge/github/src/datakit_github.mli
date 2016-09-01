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

  type state = [`Monitored | `Ignored]
  (** The type for repository state. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for Github repositories. *)

  val pp_state: state Fmt.t
  (** [pp_state] is the pretty-printer for repository state. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
  end
  (** Sets of repositories. *)

end

module Commit: sig

  type t = { repo: Repo.t; id: string }
  (** The type for commits. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for commits. *)

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

  val id: t -> string
  (** [id t] is [t]'s SHA1. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
    val repos: t -> Repo.Set.t
  end
  (** Sets of commits. *)

end

module PR: sig

  (** The type for pull-requests values. *)
  type t = {
    head: Commit.t;
    number: int;
    state: [`Open | `Closed];
    title: string;
  }

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repostiory. *)

  val commit: t -> Commit.t
  (** [commit t] is [t]'s commit. *)

  val commit_id: t -> string
  (** [commit_id t] is the SHA1 of [t]'s commit. *)

  val number: t -> int
  (** [number t] is [t]'s number. *)

  val state: t -> [`Open | `Closed]
  (** [state t] is [t]'s state. *)

  val title: t -> string
  (** [title t] is [t]'s title. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for pull-request values. *)

  val same: t -> t -> bool
  (** [same x y] is true if [x] and [y] have the same repository and
      number. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
    val repos: t -> Repo.Set.t
    val commits: t -> Commit.Set.t
  end
  (** Sets of pull requests. *)

end

module Status: sig

  (** The type for status values. *)
  type t = {
    commit: Commit.t;
    context: string list;
    url: string option;
    description: string option;
    state: Status_state.t;
  }

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

  val commit: t -> Commit.t
  (** [commit t] is [t]'s commit. *)

  val commit_id: t -> string
  (** [commit_id t] is [t]'s commit ID. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for status values. *)

  val path: t -> Datakit_path.t
  (** [path t] is path corresponding to [t]'s context. The empty list
      is rewritten into ["default"] to match the GitHub
      API. Otherwise, segments are concatenated using ["/"] as a
      separator. *)

  val same: t -> t -> bool
  (** [same x y] is true if [x] and [y] have the same commit and
      context. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
    val repos: t -> Repo.Set.t
    val commits: t -> Commit.Set.t
  end
  (** Sets of build status. *)

end

module Ref: sig

  type t = {
    head: Commit.t;
    name: string list;
  }
  (** The type for Git references. *)

  val name: t -> string list
  (** [name t] is [t]'s name. *)

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

  val commit: t -> Commit.t
  (** [commit t] is [t]'s commit. *)

  val commit_id: t -> string
  (** [commit_id t] is [t]'s commit ID. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for references. *)

  val same: t -> t -> bool
  (** [same x y] is true if [x] and [y] have the same repository and
      name. *)

  module Set: sig
    include Set.S with type elt = t
    val pp: t Fmt.t
    val repos: t -> Repo.Set.t
    val commits: t -> Commit.Set.t
  end
  (** Sets of Git references. *)

  type state = [`Created | `Updated | `Removed]
  (** The type for reference state. *)

end

module Event: sig

  (** The type for event values. *)
  type t =
    | Repo of (Repo.state * Repo.t)
    | PR of PR.t
    | Status of Status.t
    | Ref of (Ref.state * Ref.t)
    | Other of (Repo.t * string)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for event values. *)

  val repo': Repo.state -> Repo.t -> t
  val pr: PR.t -> t
  val status: Status.t -> t
  val ref: Ref.state -> Ref.t -> t
  val other: Repo.t -> string -> t

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

end

(** Signature for the GitHub API. *)
module type API = sig

  type token
  (** The type for API tokens. *)

  type 'a result = ('a, string) Result.result Lwt.t
  (** The type for results. *)

  val user_exists: token -> user:string -> bool result
  (** [exist_user t ~user] is true iff [user] exists. *)

  val repo_exists: token -> Repo.t -> bool result
  (** [exists_repo t r] is true iff the repository [r] exists. *)

  val repos: token -> user:string -> Repo.t list result
  (** [repos t ~user] is the list of repositories owned by user
      [user]. *)

  val status: token -> Commit.t -> Status.t list result
  (** [status t c] returns the list of status attached to the commit
      [c]. *)

  val set_status: token -> Status.t -> unit result
  (** [set_status t s] updates [Status.commit s]'s status with [s]. *)

  val set_ref: token -> Ref.t -> unit result
  (** [set_ref t r] updates the reference named [Ref.name r] with
      [r]. *)

  val remove_ref: token -> Repo.t -> string list -> unit result
  (** [remove_ref t n] removes the reference named [n]. *)

  val set_pr: token -> PR.t -> unit result
  (** [set_pr t pr] updates the PR number [PR.number pr] with [pr]. *)

  val prs: token -> Repo.t -> PR.t list result
  (** [prs t r] is the list of open pull-requests for the repo [r]. *)

  val refs: token -> Repo.t -> Ref.t list result
  (** [refs t r] is the list of references for the the repo [r]. *)

  val events: token -> Repo.t -> Event.t list result
  (** [event t r] is the list of events attached to the repository
      [r]. Note: can be slow/costly if multiple pages of events. *)

  module Webhook: sig

    type t
    (** The type for the webhook server state. *)

    val create: token -> Uri.t -> t
    (** [create tok uri] is the webhook server state configured to
        listen for incoming webhook events to the public address [uri]
        and using the token [tok] to perform GitHub API calls. The
        function [f] will be called everytime a new event is
        received. *)

    val run: t -> unit Lwt.t
    (** [run t] is a blocking lwt thread which runs the webook
        listener. *)

    val repos: t -> Repo.Set.t
    (** The list of watched repository. *)

    val watch: t -> Repo.t -> unit Lwt.t
    (** [watch t r] makes [t] watch the repo [r]. *)

    val events: t -> Event.t list
    (** [events t] is the list of events stored in [t]. *)

    val wait: t -> unit Lwt.t
    (** [wait t] waits for new events to be available. *)

    val clear: t -> unit
    (** [clear t] clears the list of events stored in [t]. *)

  end

end

module Snapshot: sig

  (** {1 File-system snapshots} *)

  type t
  (** The type for filesystem snapshots. *)

  val empty: t
  (** The empty snapshot. *)

  val create:
    repos:Repo.Set.t -> commits:Commit.Set.t -> status:Status.Set.t ->
    prs:PR.Set.t -> refs:Ref.Set.t -> t
  (** [create ?repos ~status ~prs] is a new snapshot [t] with
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

  val commits: t -> Commit.Set.t
  (** [commits t] are [t]'s commits. *)

  val prs: t -> PR.Set.t
  (** [prs t] are [t]'s pull-requests. *)

  val status: t -> Status.Set.t
  (** [status t] are [t]'s build status. *)

  val refs: t -> Ref.Set.t
  (** [refs t] are [t]'s Git references. *)

  val prune: t -> t * t option
  (** [prune t] is either a clean snapshot and an optional snapshot
      representing the the commits and prs entries to remove. *)

end

module Diff: sig

  (** {1 Github diffs} *)


  type id = [
    | `Repo
    | `PR of int
    | `Commit of string
    | `Status of string * string list
    | `Ref of string list
    | `Unknown
  ]
  (** The type for diff identifiers. *)

  type t = {
    repo  : Repo.t;
    id    : id;
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

  val repos: tree -> Repo.Set.t Lwt.t
  (** [repos t] is the list of repositories stored in [t]. *)

  val update_repo: DK.Transaction.t -> Repo.state -> Repo.t -> unit result
  (** [update_repo t s r] applies the repository [r] to the
      transaction [t]. Depending on the state [s] it can either remove
      the directory or create a [monitored] file. *)

  (** {1 Status} *)

  val status: tree -> Commit.t -> string list -> Status.t option Lwt.t
  (** [status t c s] is the commit's build status [s] for the commit
      [c] in the tree [t]. *)

  val statuses: ?commits:Commit.Set.t -> tree -> Status.Set.t Lwt.t
  (** [statuses t] is the list of status stored in [t].. *)

  val update_status: DK.Transaction.t -> Status.t -> unit result
  (** [update_status t s] applies the status [s] to the transaction
      [t]. *)

  (** {1 Pull requests} *)

  val pr: tree -> Repo.t -> int -> PR.t option Lwt.t
  (** [pr t r n] is the [n]'th pull-request of the repostiry [r] in
      [t]. *)

  val prs: ?repos:Repo.Set.t -> tree -> PR.Set.t Lwt.t
  (** [prs t] is the list of pull requests stored in [t]. *)

  val update_pr: DK.Transaction.t -> PR.t -> unit result
  (** [update_pr t pr] applies the pull-request [pr] to the
      transaction [t]. *)

  (** {1 Git References} *)

  val update_ref: DK.Transaction.t -> Ref.state -> Ref.t -> unit result
  (** [update_ref t s r] applies the Git reference [r] to the
      transaction [t]. Depending on the state [s] it can either remove
      the directory or create an [head] file. *)

  (** {1 Events} *)

  val update_event: DK.Transaction.t -> Event.t -> unit result
  (** [update_event t e] applies the (webhook) event [e] to the
      transaction [t]. *)

  (** {1 Snapshots and diffs} *)

  val safe_diff: tree -> DK.Commit.t -> Diff.Set.t Lwt.t
  (** [diff tree c] computes the Github diff between the branch [b]
      and the commit [c]. *)

  val snapshot: string -> ?old:(DK.Commit.t * Snapshot.t) -> tree ->
    Snapshot.t Lwt.t
  (** [snapshot dbg ?old t] is a snapshot of the tree [t]. Note: this
      is expensive, so try to provide a previous (recent) snapshot
      [prev] if possible. *)

  val apply: Snapshot.t -> (tree * Diff.Set.t) -> Snapshot.t Lwt.t
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
    ?webhook:API.Webhook.t ->
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
