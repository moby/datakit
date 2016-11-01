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
  ?canaries:CI_target.ID_Set.t CI_projectID.Map.t ->
  (unit -> DK.t Lwt.t) ->
  (string CI_term.t String.Map.t) CI_projectID.Map.t ->
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

val targets : t -> (target IntMap.t * target Datakit_path.Map.t) CI_projectID.Map.t
(** [targets t] is a snapshot of the current state of all known PRs and branches. *)

val jobs : target -> job list
(** [jobs t] is the list of jobs for a target. *)

val job_name : job -> string
(** [job_name j] is the name of the GitHub status that this job computes. *)

val state : job -> CI_state.t
(** [state job] is the current state of [job]. *)

val git_target : target -> [`PR of CI_github_hooks.PR.t | `Ref of CI_github_hooks.Ref.t]
(** [git_target target] is the GitHub metadata about this target. *)

val project : target -> CI_projectID.t
(** [project t] is the GitHub project that contains [target]. *)

val title : target -> string
(** [title t] is the title of PR [t]. *)

val rebuild_actions : job -> string list
(** [rebuild_actions job] is the list of cached results from [job] that can be rebuilt. *)

val rebuild : t -> CI_projectID.t -> target:[`PR of int | `Ref of Datakit_path.t] -> job:string -> string -> unit Lwt.t
(** [rebuild t project ~target ~job action] triggers [target/job]'s [action] as needing to be rebuilt. *)
