include CI_s.TERM

val target : [`PR of CI_github_hooks.PR.t | `Ref of CI_github_hooks.Ref.t] t
(** [target] evaluates to the PR or branch being tested. *)

val pp_target : [`PR of CI_github_hooks.PR.t | `Ref of CI_github_hooks.Ref.t] Fmt.t

val head : CI_github_hooks.Commit.t t
(** [head] evaluates to the commit at the head of the context's PR. *)

val github : CI_github_hooks.snapshot t
(** [github t] evaluates to the state of the GitHub metadata. *)

val branch_head : CI_projectID.t -> string -> CI_github_hooks.Commit.t t
(** [branch_head project b] evaluates to the commit at the head of branch [b] in [project]. *)

val tag : CI_projectID.t -> string -> CI_github_hooks.Commit.t t
(** [tag project t] evaluates to the commit of tag [t] in [project]. *)

val dk : (unit -> CI_utils.DK.t Lwt.t) t
(** [dk] is a function for getting the current DataKit connection. *)

val add_rebuild_action : string -> (unit -> unit Lwt.t) -> unit t
(** [add_rebuild_action label callback] adds a button the user can click to rebuild an artifact. *)

val ci_status : string -> [`Pending | `Success | `Failure | `Error] option t
(** [ci_status ci] is the status reported by CI [ci].
    Note that even if the CI is e.g. pending, this returns a successful result with
    the value [`Pending], not a pending result. *)

val ci_target_url : string -> Uri.t option t
(** [ci_target_url ci] is the target URL reported by CI [ci]. *)

val ci_success_target_url : string -> Uri.t t
(** [ci_success_target_url ci] is the URL of the *successful* build [ci].
    It is pending until a successful URL is available. *)

val run :
  snapshot:CI_github_hooks.snapshot ->
  target:[`PR of CI_github_hooks.PR.t | `Ref of CI_github_hooks.Ref.t] ->
  recalc:(unit -> unit) ->
  dk:(unit -> CI_utils.DK.t Lwt.t) ->
  rebuild_actions:(string * (unit -> unit Lwt.t)) list ref ->
  'a t -> ('a CI_result.t * CI_result.Step_log.t) Lwt.t * (unit -> unit)
(* [run ~snapshot ~target ~recalc ~dk ~rebuild_actions] is the pair [(state, cancel)]. *)
