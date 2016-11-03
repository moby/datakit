val routes :
  config:CI_web_templates.t ->
  logs:CI_live_log.manager ->
  ci:CI_engine.t ->
  auth:CI_web_utils.Auth.t ->
  dashboards:CI_target.ID_Set.t CI_projectID.Map.t ->
  (string * (unit -> Cohttp_lwt_body.t CI_web_utils.Wm.resource)) list
(** [routes ~config ~logs ~ci ~auth ~dashboards] is the configuration for a web-server providing a UI to [ci]. *)
