open Datakit_github

(** Generate HTML for the various pages in the UI. *)

type t = private {
  name : string;
  state_repo : Uri.t option;
  metrics_token : [`SHA256 of Cstruct.t] option;
  listen_addr: [`HTTP of int | `HTTPS of int];
  can_read : CI_ACL.t;
  can_build : CI_ACL.t;
}

val config:
  ?name:string ->
  ?state_repo:Uri.t ->
  ?metrics_token:[`SHA256 of string] ->
  ?listen_addr:[`HTTP of int | `HTTPS of int] ->
  can_read:CI_ACL.t ->
  can_build:CI_ACL.t ->
  unit -> t
(** [config ~name ~state_repo ()] is a web configuration.
    If [name] is given, it is used as the main heading, and also as the name of the session cookie
    (useful if you run multiple CIs on the same host, on different ports).
    If [state_repo] is given, it is used to construct links to the state repository on GitHub. *)

type page = user:string option -> [`Html] Tyxml.Html.elt

module Error : sig
  type t

  val permission_denied : t
  val logout_needed : t

  val uri_path : t -> string
  (** Path to redirect users to to see this error. *)

  val uri : t -> Uri.t
  (** [uri t] is [Uri.of_string (uri_path t)] *)
end

val login_page :
  ?github:Uri.t ->
  csrf_token:string ->
  CI_form.State.t ->
  is_configured:bool ->
  t ->
  page

val auth_setup :
  csrf_token:string ->
  CI_form.State.t ->
  t ->
  page

val user_page :
  csrf_token:string ->
  t ->
  page

val main_page :
  csrf_token:string ->
  ci:CI_engine.t ->
  dashboards:CI_target.Set.t Repo.Map.t ->
  t ->
  page

val prs_page :
  ci:CI_engine.t ->
  t ->
  page

val branches_page :
  ci:CI_engine.t ->
  t ->
  page

val tags_page :
  ci:CI_engine.t ->
  t ->
  page

val commit_page :
  commit:string ->
  archived_targets:(CI_target.t * CI_utils.DK.Commit.t) list ->
  CI_target.t list ->
  t ->
  page

val target_page :
  csrf_token:string ->
  ?title:string ->
  target:CI_target.t ->
  CI_history.State.t ->
  t ->
  page

val live_log_frame :
  branch:string ->
  have_history:bool ->
  t ->
  page

val saved_log_frame :
  commit:string ->
  branch:string ->
  t ->
  page

(** A basic page just the error text and no header, footer, etc. *)
val plain_error :
  string ->
  t ->
  page

val error_page :
  string ->
  t ->
  page

module Settings : sig
  val index :
    t ->
    page

  val github_auth :
    csrf_token:string ->
    CI_form.State.t ->
    t ->
    page
end

val saved_log_frame_link : branch:string -> commit:string -> string
(** [saved_log_frame_link ~branch ~commit] is the path component of the iframe link for the given saved log. *)
