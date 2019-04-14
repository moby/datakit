open Datakit_github

include CI_s.TERM

val pp_target : [ `PR of PR.t | `Ref of Ref.t ] Fmt.t

val github : CI_utils.DK.Tree.t t

val target : CI_target.t -> CI_target.v t

val job_id : CI_s.job_id t

val head : CI_target.t -> Commit.t t

val branch_head : Repo.t -> string -> Commit.t t

val tag : Repo.t -> string -> Commit.t t

val dk : (unit -> CI_utils.DK.t Lwt.t) t

val ci_status : string list -> CI_target.t -> Status_state.t option t

val ci_target_url : string list -> CI_target.t -> Uri.t option t

val ci_success_target_url : string list -> CI_target.t -> Uri.t t

val run :
  snapshot:CI_utils.DK.Tree.t ->
  job_id:CI_s.job_id ->
  recalc:(unit -> unit) ->
  dk:(unit -> CI_utils.DK.t Lwt.t) ->
  'a t ->
  ('a CI_result.t * CI_output.logs) Lwt.t * (unit -> unit)
