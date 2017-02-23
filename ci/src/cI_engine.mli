open Datakit_github
open Astring
open CI_utils

type t
(** A DataKit CI instance. *)

type target
(** The state of an open PR or branch. *)

type job
(** A job keeps up-to-date one state within an open PR. *)

val create :
  web_ui:Uri.t ->
  ?canaries:CI_target.Set.t Repo.Map.t ->
  (unit -> DK.t Lwt.t) ->
  (CI_target.t -> string CI_term.t String.Map.t) Repo.Map.t ->
  t
(** [create ~web_ui connect projects] is a new DataKit CI that calls [connect] to connect to the database.
    Once [listen] has been called, it will handle CI for [projects].
    [projects] maps projects to the status reports to produce.
    [web_ui] is the URL of the main web-page (used when adding links to PRs on GitHub).
    If [canaries] is given, only those targets will be considered. *)

val listen : ?switch:Lwt_switch.t -> t -> [`Abort] Lwt.t
(** [listen t] runs a loop that watches for PRs and branches that need building.
    Returns [`Abort] if the switch is turned off. *)

val dk : t -> DK.t Lwt.t
(** [dk t] is the connection to DataKit. If not currently connected, this will be a sleeping
    thread that will resolve to the next successful connection. *)

val prs: t -> target PR.Index.t Repo.Map.t
(** [prs t] is a snapshot of the current state of all known PRs. *)

val refs: t -> target Ref.Index.t Repo.Map.t
(** [targets t] is a snapshot of the current state of all branches. *)

val latest_state : t -> CI_target.t -> CI_history.State.t option Lwt.t
(** [latest_state t target] is the current state of [target]. *)

val jobs : target -> job list
(** [jobs t] is the list of jobs for a target. *)

val job_name : job -> string
(** [job_name j] is the name of the GitHub status that this job computes. *)

val state : job -> string CI_output.t option
(** [state job] is the current state of [job]. *)

val target : target -> CI_target.v
(** [target target] is the GitHub metadata about this target. *)

val targets_of_commit : t -> Repo.t -> string -> CI_target.t list
(** [targets_of_commit t repo c] is the list of targets in [repo] with head commit [c]. *)

val repo : target -> Repo.t
(** [repo t] is the GitHub repository that contains [target]. *)

val title : target -> string
(** [title t] is the title of PR [t]. *)

val rebuild : t -> branch_name:string -> unit Lwt.t
(** [rebuild t ~branch_name] triggers a rebuild for results branch [branch_name] and recalculates any terms that depend on it.
    An error is reported if no term currently depends on [branch_name]. *)
