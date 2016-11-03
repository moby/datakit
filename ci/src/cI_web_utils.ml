open! Astring
open Sexplib.Std
open Lwt.Infix

module Log = CI_utils.Log
module Server = Cohttp_lwt_unix.Server

module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix_io)
end
module Session = struct
  module Backend = struct
    include Session.Lift.IO(Lwt)(Session.Memory)        (* TODO: Disk *)
  end
  include Session_webmachine.Make(Lwt)(Backend)
  let create () = Session.Memory.create ()
end

let () =
  Nocrypto_entropy_unix.initialize ()

type role = [`Reader | `LoggedIn | `Builder]

module Hashed_password = struct
  type t = {
    prf : [`SHA1];
    salt : Cstruct.t;
    hashed_password : Cstruct.t;
    count : int;
    dk_len : int32;
  } [@@deriving sexp]

  let matches t ~password =
    let hashed_user_input =
      Pbkdf.pbkdf2
        ~prf:(t.prf :> Nocrypto.Hash.hash)
        ~count:t.count
        ~dk_len:t.dk_len
        ~salt:t.salt
        ~password:(Cstruct.of_string password)
    in
    Cstruct.equal hashed_user_input t.hashed_password

  let of_plain ~password =
    let prf = `SHA1 in
    let count = 5000 in
    let dk_len = 20l in
    let salt = Nocrypto.Rng.generate 16 in
    let hashed_password = Pbkdf.pbkdf2 ~prf ~count ~dk_len ~salt ~password:(Cstruct.of_string password) in
    { prf; count; dk_len; salt; hashed_password }
end

module User = struct
  type t = {
    name : string;
    password : Hashed_password.t;
  }

  let name t = t.name
end

module Auth = struct
  type password_file = (string * Hashed_password.t) list [@@deriving sexp]

  type t = User.t String.Map.t

  let lookup t ~user ~password =
    match String.Map.find user t with
    | Some ({ User.password = stored_pw; _ } as user) when Hashed_password.matches ~password stored_pw -> Some user
    | Some _ -> Log.info (fun f -> f "Incorrect password for user %S" user); None
    | None -> Log.info (fun f -> f "No such user %S" user); None

  let with_echo_off fn =
    let terminal = Unix.tcgetattr Unix.stdin in
    let no_echo = {terminal with Unix.c_echo = false} in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW no_echo;
    let result = fn () in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW terminal;
    result

  let ensure_initialised passwd_file =
    if Sys.file_exists passwd_file then Lwt.return ()
    else (
      let user = "admin" in
      Fmt.pr "Enter password for %S user:@.%!" user;
      let password = with_echo_off (fun () -> input_line stdin) in
      let entry = Hashed_password.of_plain ~password in
      let contents =
        [user, entry]
        |> sexp_of_password_file
        |> Sexplib.Sexp.to_string in
      Lwt_io.with_file ~mode:Lwt_io.output passwd_file (fun ch -> Lwt_io.write ch contents)
    )

  let create passwd =
    ensure_initialised passwd >>= fun () ->
    Lwt_io.with_file ~mode:Lwt_io.input passwd (fun ch -> Lwt_io.read ch) >|= fun contents ->
    password_file_of_sexp (Sexplib.Sexp.of_string contents)
    |> String.Map.of_list
    |> String.Map.mapi (fun name password -> { User.name; password })
end

type server = {
  auth : Auth.t;
  session_backend : Session.Backend.t;
  web_config : CI_web_templates.t;
  has_role : role -> user:string option -> bool;
}

let cookie_key t =
  "__ci_session:" ^ t.web_config.CI_web_templates.name

let server ~auth ~web_config ~has_role =
  let session_backend = Session.create () in
  { auth; session_backend; web_config; has_role }

let web_config t = t.web_config

class type resource = object
  inherit [Cohttp_lwt_body.t] Wm.resource
  method content_types_accepted : ((string * Cohttp_lwt_body.t Wm.acceptor) list, Cohttp_lwt_body.t) Wm.op
  method content_types_provided : ((string * Cohttp_lwt_body.t Wm.provider) list, Cohttp_lwt_body.t) Wm.op
end

class static ~valid ~mime_type dir =
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided rd =
      match mime_type rd.Wm.Rd.uri with
      | None      -> Wm.continue [] rd
      | Some mime -> Wm.continue [mime, self#data] rd

    method content_types_accepted rd =
      Wm.continue [] rd

    method private data rd =
      (* Reload each time to make testing easier *)
      let ( / ) = Filename.concat in
      let name = Wm.Rd.lookup_path_info_exn "name" rd in
      if Str.string_match valid name 0 then (
        let path = dir / name in
        if Sys.file_exists path then (
          Lwt_io.with_file ~mode:Lwt_io.input path (fun ch -> Lwt_io.read ch) >>= fun body ->
          Wm.continue (`String body) rd
        ) else (
          Log.debug (fun f -> f "Missing static resource %S" name);
          Wm.respond 404 rd
        )
      ) else (
        Log.debug (fun f -> f "Invalid static resource name %S" name);
        Wm.respond 404 rd
      )
  end

class static_crunch ~mime_type read =
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource

    method content_types_provided rd =
      match mime_type rd.Wm.Rd.uri with
      | None      -> Wm.continue [] rd
      | Some mime -> Wm.continue [mime, self#data] rd

    method content_types_accepted rd =
      Wm.continue [] rd

    method private data rd =
      let dir = Wm.Rd.lookup_path_info_exn "dir" rd in
      let name = Wm.Rd.lookup_path_info_exn "name" rd in
      match read (Printf.sprintf "%s/%s" dir name) with
      | Some data ->
        Wm.continue (`String data) rd
      | None ->
        Log.info (fun f -> f "Missing static resource %S" name);
        Wm.respond 404 rd
  end

module Session_data = struct
  type t = {
    csrf_token : string;
    login_redirect : string option;     (* Redirect here when login succeeeds. *)
    username : string option;
  } [@@deriving sexp]

  let csrf_token t = t.csrf_token
  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)
  let of_string s = t_of_sexp (Sexplib.Sexp.of_string s)
end

class virtual resource_with_session t =
  object(self)
    inherit [Cohttp_lwt_body.t] Wm.resource
    inherit [Cohttp_lwt_body.t] Session.manager ~cookie_key:(cookie_key t) t.session_backend

    method private session rd =
      self#session_of_rd rd >>= function
      | Ok (Some session) -> Lwt.return (Session_data.of_string session.Session.value)
      | Ok None | Error _ ->
        Log.info (fun f -> f "Generating new session");
        let csrf_token = B64.encode (Nocrypto.Rng.generate 16 |> Cstruct.to_string) in
        let value = { Session_data.csrf_token; username = None; login_redirect = None } in
        self#session_set (Session_data.to_string value) rd >>= fun () ->
        Lwt.return value

    method! finish_request rd =
      let rd = self#session_set_hdrs ~path:"/" ~secure:true rd in
      Wm.continue () rd
  end

class virtual protected_page t =
  object(self)
    inherit resource_with_session t

    val mutable authenticated_user = None
    method private authenticated_user = authenticated_user

    method virtual private required_roles : role list

    method! is_authorized rd =
      let roles_needed = self#required_roles in
      self#session rd >>= fun session ->
      match session.Session_data.username with
      | Some _ as username ->
        authenticated_user <- username;
        if List.for_all (t.has_role ~user:username) roles_needed then
          Wm.continue `Authorized rd
        else
          Wm.respond 403 ~body:(`String "Permission denied") rd
      | None ->
        if List.for_all (t.has_role ~user:None) roles_needed then
          Wm.continue `Authorized rd
        else (
          let login_redirect =
            match Uri.path rd.Wm.Rd.uri with
            | "/auth/logout" -> None
            | _ -> Some (Uri.path_and_query rd.Wm.Rd.uri)
          in
          let value = {session with Session_data.login_redirect} in
          self#session_set (Session_data.to_string value) rd >>= fun () ->
          Wm.continue (`Redirect (Uri.of_string "/auth/login")) rd
        )
  end

class virtual post_page t = object(self)
  inherit protected_page t as super

  method! allowed_methods rd =
    Wm.continue [`POST] rd

  method content_types_provided rd =
    Wm.continue [
      "text/html", (fun _ -> assert false);
    ] rd

  method content_types_accepted rd =
    Wm.continue [
      "application/x-www-form-urlencoded", (fun _ -> assert false);
    ] rd

  method! forbidden rd =
    self#session rd >>= fun session_data ->
    let expected_token = session_data.Session_data.csrf_token in
    match Uri.get_query_param rd.Wm.Rd.uri "CSRFToken" with
    | Some provided_token when provided_token = expected_token -> super#forbidden rd
    | None -> Wm.respond 403 ~body:(`String "Missing CSRFToken") rd
    | Some provided_token ->
      Log.info (fun f -> f "Expecting CSRFToken %S; got %S" expected_token provided_token);
      Wm.respond 403 ~body:(`String "Incorrect CSRFToken") rd
end

class logout_page t = object(self)
  inherit post_page t

  method private required_roles = []

  method! private process_post rd =
    self#session rd >>= fun session_data ->
    let session = {session_data with Session_data.username = None} in
    self#session_set (Session_data.to_string session) rd >>= fun () ->
    Wm.continue true (Wm.Rd.redirect "/" rd)
end

class login_page t = object(self)
  inherit resource_with_session t

  method! allowed_methods rd =
    Wm.continue [`GET; `POST] rd

  method content_types_provided rd =
    Wm.continue [
      "text/html", self#to_html;
    ] rd

  method private to_html rd =
    self#session rd >>= fun {Session_data.username; csrf_token; _} ->
    let html = CI_web_templates.login_page ~csrf_token ~user:username t.web_config in
    let body = Fmt.to_to_string (Tyxml.Html.pp ()) html in
    Wm.continue (`String body) rd

  method content_types_accepted rd =
    Wm.continue [
      "multipart/form-data", (fun _ -> assert false);
    ] rd

  method! private process_post rd =
    self#session rd >>= fun session_data ->
    let expected_token = session_data.Session_data.csrf_token in
    match Uri.get_query_param rd.Wm.Rd.uri "CSRFToken" with
    | None -> Wm.respond 403 ~body:(`String "Missing CSRFToken") rd
    | Some provided_token when provided_token <> expected_token ->
      Log.info (fun f -> f "Expecting CSRFToken %S; got %S" expected_token provided_token);
      Wm.respond 403 ~body:(`String "Incorrect CSRFToken") rd
    | Some _ ->
      match Cohttp.Header.get rd.Wm.Rd.req_headers "content-type" with
      | None -> Wm.respond 403 ~body:(`String "Missing Content-Type header") rd
      | Some content_type ->
        let body = rd.Wm.Rd.req_body in
        Multipart.parse_stream ~stream:(Cohttp_lwt_body.to_stream body) ~content_type >>= fun parts ->
        Multipart.get_parts parts >>= fun parts ->
        let get name =
          match Multipart.StringMap.find name parts with
          | `String s -> Ok s
          | `File _ -> Error "File upload in form!"
          | exception Not_found -> Error (Printf.sprintf "Missing %S in form submission" name)
        in
        match get "user", get "password" with
        | Error msg, _ -> Wm.respond 403 ~body:(`String msg) rd
        | Ok _, Error msg -> Wm.respond 403 ~body:(`String msg) rd
        | Ok user, Ok password ->
          match Auth.lookup t.auth ~user ~password with
          | Some _ -> 
            let session = {session_data with Session_data.username = Some user} in
            self#session_set (Session_data.to_string session) rd >>= fun () ->
            begin match session.Session_data.login_redirect with
              | None -> Lwt.return "/"
              | Some redirect ->
                let value = {session with Session_data.login_redirect = None} in
                self#session_set (Session_data.to_string value) rd >>= fun () ->
                Lwt.return redirect
            end >>= fun redirect ->
            Wm.continue true (Wm.Rd.redirect redirect rd)
          | None ->
            Wm.respond 403 ~body:(`String "Invalid username/password") rd
end

let pp_path =
  Fmt.list ~sep:(Fmt.(const string) ", ") Fmt.string

let callback ~routes _conn request body =
  Wm.dispatch' routes ~body ~request
  >|= begin function
    | None        -> (`Not_found, Cohttp.Header.init (), `String "Not found", [])
    | Some result -> result
  end
  >>= fun (status, headers, body, path) ->
  Log.info (fun f ->
      let open Cohttp in
      f "%d - %s %s"
        (Code.code_of_status status)
        (Code.string_of_method (Request.meth request))
        (Uri.path (Request.uri request))
    );
  Log.debug (fun f -> f "Webmachine path: %a" pp_path path);
  Server.respond ~headers ~body ~status ()

let serve ~mode ~routes =
  let callback = callback ~routes in
  let http = Server.make ~callback () in
  Conduit_lwt_unix.serve ~mode ~ctx:Conduit_lwt_unix.default_ctx (fun flow ic oc ->
      Lwt.catch
        (fun () -> Server.callback http flow ic oc)
        (fun ex ->
           Log.info (fun f -> f "Error handling HTTP connection: %s" (Printexc.to_string ex));
           Lwt.return ()
        )
    )
