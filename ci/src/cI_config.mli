open Datakit_github

type test = string CI_term.t

type project = {
  dashboards : CI_target.Set.t;
  tests : CI_target.t -> test CI_utils.Job_map.t;
}

type t = private {
  web_config : CI_web_templates.t;
  projects : project Repo.Map.t;
}

val project : id:string -> ?dashboards:string list -> (CI_target.t -> (string * test) list) -> Repo.t * project

val v:
  web_config:CI_web_templates.t ->
  projects:(Repo.t * project) list ->
  t
