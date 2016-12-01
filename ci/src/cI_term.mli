open Datakit_github
include CI_s.TERM

val pp_target : [`PR of PR.t | `Ref of Ref.t] Fmt.t

val github : CI_utils.DK.Tree.t t
(** [github t] evaluates to the state of the GitHub metadata. *)

val github_target : CI_target.t -> CI_target.v t
(** [github_target id] evaluates to the GitHub metadata of the named target. *)

val job_id : CI_s.job_id t
(** [job_id] evaluates to the job that evaluates the term.
    This is useful for logging. *)

val head : CI_target.t -> Commit.t t
(** [head target] evaluates to the commit at the head [target]. *)

val branch_head : Repo.t -> string -> Commit.t t
(** [branch_head project b] evaluates to the commit at the head of branch [b] in [project]. *)

val tag : Repo.t -> string -> Commit.t t
(** [tag project t] evaluates to the commit of tag [t] in [project]. *)

val dk : (unit -> CI_utils.DK.t Lwt.t) t
(** [dk] is a function for getting the current DataKit connection. *)

val ci_status : string list -> CI_target.t -> Status_state.t option t
(** [ci_status ci target] is the status reported by CI [ci] for [target].
    Note that even if the CI is e.g. pending, this returns a successful result with
    the value [`Pending], not a pending result. *)

val ci_target_url : string list -> CI_target.t -> Uri.t option t
(** [ci_target_url ci target] is the target URL reported by CI [ci]. *)

val ci_success_target_url : string list -> CI_target.t -> Uri.t t
(** [ci_success_target_url ci target] is the URL of the *successful* build [ci].
    It is pending until a successful URL is available. *)

val run :
  snapshot:CI_utils.DK.Tree.t ->
  job_id:CI_s.job_id ->
  recalc:(unit -> unit) ->
  dk:(unit -> CI_utils.DK.t Lwt.t) ->
  'a t -> ('a CI_result.t * CI_result.Step_log.t) Lwt.t * (unit -> unit)
(* [run ~snapshot ~job_id ~recalc ~dk] is the pair [(state, cancel)]. *)
