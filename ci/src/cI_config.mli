open Astring

type test = string CI_term.t

type project = {
  dashboards : CI_target.ID_Set.t;
  tests : test String.Map.t;
}

type t = private {
  web_config : CI_web_templates.t;
  projects : project CI_projectID.Map.t;
}

val project : id:string -> ?dashboards:string list -> (string * test) list -> CI_projectID.t * project

val ci :
  web_config:CI_web_templates.t ->
  projects:(CI_projectID.t * project) list ->
  t
