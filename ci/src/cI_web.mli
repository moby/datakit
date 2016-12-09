open Datakit_github

val routes :
  logs:CI_live_log.manager ->
  ci:CI_engine.t ->
  server:CI_web_utils.server ->
  dashboards:CI_target.ID_Set.t Repo.Map.t ->
  (string * (unit -> Cohttp_lwt_body.t CI_web_utils.Wm.resource)) list
(** [routes ~config ~logs ~ci ~auth ~dashboards] is the configuration for a web-server providing a UI to [ci]. *)
