module Wm : Webmachine.S with type 'a io = 'a Lwt.t

module User : sig
  type t
  val name : t -> string
end

module Auth : sig
  type t

  val create : string -> t Lwt.t
  (** [create path] is a user authenticator with configuration at [path]. If [path] does not exist, the
      user is prompted to create one. *)

  val lookup : t -> user:string -> password:string -> User.t option
  (** [lookup t (username, password)] returns the user with name [username] if the user exists and
      the password is correct. *)
end

type server

val server : auth:Auth.t -> web_config:CI_web_templates.t -> server
val web_config : server -> CI_web_templates.t

module Session_data : sig
  type t
  val csrf_token : t -> string
end

class static : valid:Str.regexp -> mime_type:(Uri.t -> string option) -> string -> object
  inherit [Cohttp_lwt_body.t] Wm.resource
  method content_types_accepted : ((string * Cohttp_lwt_body.t Wm.acceptor) list, Cohttp_lwt_body.t) Wm.op
  method content_types_provided : ((string * Cohttp_lwt_body.t Wm.provider) list, Cohttp_lwt_body.t) Wm.op
end
(** [new static ~valid ~mime_type dir] serves up files from the directory [dir], taking the leafname from the context.
    Names must match the RE [valid] and the MIME type returned will be [mime_type uri]. *)

class static_crunch : mime_type:(Uri.t -> string option) -> (string -> string option) -> object
  inherit [Cohttp_lwt_body.t] Wm.resource
  method content_types_accepted : ((string * Cohttp_lwt_body.t Wm.acceptor) list, Cohttp_lwt_body.t) Wm.op
  method content_types_provided : ((string * Cohttp_lwt_body.t Wm.provider) list, Cohttp_lwt_body.t) Wm.op
end
(** [new static_crunch ~mime_type read] serves up files using the function [read], taking the path from the context.
    The MIME type returned will be [mime_type uri]. *)

class virtual resource_with_session : server -> object
  inherit [Cohttp_lwt_body.t] Wm.resource
  method private session : Cohttp_lwt_body.t Webmachine.Rd.t -> Session_data.t Lwt.t
end
(** [resource_with_session] ensures there is a session for each request. *)

class login_page : server -> object
    inherit resource_with_session
    method content_types_accepted : ((string * Cohttp_lwt_body.t Wm.acceptor) list, Cohttp_lwt_body.t) Wm.op
    method content_types_provided : ((string * Cohttp_lwt_body.t Wm.provider) list, Cohttp_lwt_body.t) Wm.op
  end
(** Page to serve at [/auth/login]. *)

class virtual protected_page : server -> object
  inherit resource_with_session
  method private authenticated_user : string option
end
(** The [is_authorized] method checks that the session has an associated user and asks the
    user to log in if not.
    [authenticated_user] returns the name of the user once [is_authorized] has completed. *)

class virtual post_page : server -> object
  inherit protected_page
  method content_types_accepted : ((string * Cohttp_lwt_body.t Wm.acceptor) list, Cohttp_lwt_body.t) Wm.op
  method content_types_provided : ((string * Cohttp_lwt_body.t Wm.provider) list, Cohttp_lwt_body.t) Wm.op
end
(** [post_page] accepts form POST submissions.
    It overrides [forbidden] to check that the CSRF token is present and correct. *)

class logout_page : server -> object
    inherit post_page
  end
(** Posting to this page logs the user out and redirects to [/]. *)

val serve :
  mode:Conduit_lwt_unix.server ->
  routes:(string * (unit -> Cohttp_lwt_body.t Wm.resource)) list ->
  unit Lwt.t
(** [serve ~mode ~routes] runs a web-server listening on [mode] that dispatches incoming requests using [routes]. *)
