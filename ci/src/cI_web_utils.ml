open! Astring
open Sexplib.Std
open Lwt.Infix

module Log = CI_utils.Log
module Server = Cohttp_lwt_unix.Server

module Metrics = struct
  open CI_prometheus

  let namespace = "DataKitCI"
  let subsystem = "web"

  let record_request =
    let help = "HTTP requests to web UI" in
    let family = Counter.v_labels ~help ~label_names:[|"method"|] ~namespace ~subsystem "requests_total" in
    fun req ->
      let c = Counter.labels family [| Cohttp.(Code.string_of_method (Request.meth req)) |] in
      Counter.inc_one c

  let record_response =
    let help = "HTTP responses from web UI" in
    let family = Counter.v_labels ~help ~label_names:[|"code"|] ~namespace ~subsystem "responses_total" in
    fun code ->
      let c = Counter.labels family [| Cohttp.(Code.string_of_status code) |] in
      Counter.inc_one c

  let requests_in_progress =
    let help = "HTTP requests currently being handled by the web UI" in
    Gauge.v ~help ~namespace ~subsystem "requests_in_progress"

  let local_login_ok_total =
    let help = "Number of successful local login attempts" in
    Counter.v ~help ~namespace ~subsystem "local_login_ok_total"

  let local_login_rejected_total =
    let help = "Number of unsuccessful local login attempts" in
    Counter.v ~help ~namespace ~subsystem "local_login_rejected_total"

  let github_login_ok_total =
    let help = "Number of successful GitHub login attempts" in
    Counter.v ~help ~namespace ~subsystem "github_login_ok_total"

  let github_login_rejected_total =
    let help = "Number of unsuccessful GitHub login attempts" in
    Counter.v ~help ~namespace ~subsystem "github_login_rejected_total"

  let response_time_seconds =
    let help = "Time to handle one web request" in
    Summary.v ~help ~namespace ~subsystem "response_time_seconds"
end

module Wm = struct
  module Rd = Webmachine.Rd
  include Webmachine.Make(Cohttp_lwt_unix_io)
end
module Session = struct
  module Memory = Session.Lift.IO(Lwt)(Session.Memory)
  module Backend = struct
    type t =
      [ `Memory of Memory.t
      | `Redis of Session_redis_lwt.t ]

    type 'a io = 'a Lwt.t
    type key = string
    type value = string
    type period = int64

    let default_period = function
      | `Memory t -> Memory.default_period t
      | `Redis t -> Session_redis_lwt.default_period t

    let generate ?expiry ?value = function
      | `Memory t -> Memory.generate ?expiry ?value t
      | `Redis t -> Session_redis_lwt.generate ?expiry ?value t

    let clear t key =
      match t with
      | `Memory t -> Memory.clear t key
      | `Redis t -> Session_redis_lwt.clear t key

    let get t key =
      match t with
      | `Memory t -> Memory.get t key
      | `Redis t -> Session_redis_lwt.get t key

    let set ?expiry t key value =
      match t with
      | `Memory t -> Memory.set ?expiry t key value
      | `Redis t -> Session_redis_lwt.set ?expiry t key value
  end
  include Session_webmachine.Make(Lwt)(Backend)
  let connect = function
    | `Memory -> `Memory (Session.Memory.create ())
    | `Redis pool -> `Redis (Session_redis_lwt.of_connection_pool pool)
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

let memo generate =
  let cached  = Hashtbl.create 5 in
  fun k ->
    try Hashtbl.find cached k
    with Not_found ->
      let v = generate k in
      Hashtbl.add cached k v;
      v

module Auth = struct
  type password_file = (string * Hashed_password.t) list [@@deriving sexp]

  type user_attributes = {
    github_orgs : string list Lwt.t Lazy.t;
    can_read_github : CI_projectID.t -> bool Lwt.t;
  }

  type github_auth = {
    client_id : string;
    client_secret : string;
    callback : Uri.t option;
  }

  type t = {
    github : github_auth option;
    local_users : User.t String.Map.t;
    mutable attributes : user_attributes String.Map.t;
  }

  let lookup t ~user ~password =
    match String.Map.find user t.local_users with
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

  let create ?github passwd =
    ensure_initialised passwd >>= fun () ->
    Lwt_io.with_file ~mode:Lwt_io.input passwd (fun ch -> Lwt_io.read ch) >|= fun contents ->
    let local_users =
      password_file_of_sexp (Sexplib.Sexp.of_string contents)
      |> String.Map.of_list
      |> String.Map.mapi (fun name password -> { User.name; password })
    in
    { github; local_users; attributes = String.Map.empty }

  let scopes = [`Read_org; `Repo]

  let github_login_url ~csrf_token t =
    match t.github with
    | None -> None
    | Some github ->
      let url = Github.URI.authorize
          ~scopes
          ~client_id:github.client_id
          ?redirect_uri:github.callback
          ~state:csrf_token
          ()
      in
      Some url

  let handle_github_callback t ~code =
    match t.github with
    | None -> Lwt.return @@ Error "GitHub auth is not configured!"
    | Some github ->
      Github.Token.of_code ~client_id:github.client_id ~client_secret:github.client_secret ~code () >>= function
       | None -> Lwt.return @@ Error "Token.of_code failed (no further information available)"
       | Some token ->
         Github.Monad.run (Github.User.current_info ~token ()) >|= fun resp ->
         let user_info = Github.Response.value resp in
         let github_orgs = lazy (
           Github.Monad.run begin
             let open! Github.Monad in
             Github.User.current_info ~token () >|= Github.Response.value >>= fun user_info ->
             let user = user_info.Github_t.user_info_login in
             Github.Organization.user_orgs ~token ~user () |> Github.Stream.to_list >|= fun orgs ->
             let orgs = List.map (fun org -> org.Github_t.org_login) orgs in
             Log.info (fun f -> f "User %S belongs to %a" user (Fmt.Dump.list Fmt.string) orgs);
             orgs
           end
         ) in
         let user = "github:" ^ user_info.Github_t.user_info_login in
         let can_read_github project =
           Lwt.try_bind (fun () ->
               Github.Monad.run begin
                 let open! Github.Monad in
                 let {CI_projectID.user; project = repo} = project in
                 Github.Repo.info ~token ~user ~repo () >|= Github.Response.value
               end
             )
             (fun (_:Github_t.repository) ->
                 Log.info (fun f -> f "%S can read %a" user CI_projectID.pp project);
                 Lwt.return true
             )
             (fun ex ->
                Log.info (fun f -> f "%S can't read %a" user CI_projectID.pp project);
                Log.debug (fun f -> f "%S can't read %a: %a" user CI_projectID.pp project Fmt.exn ex);
                Lwt.return false
             )
         in
         let attributes = {
           github_orgs;
           can_read_github = memo can_read_github;
         } in
         t.attributes <- String.Map.add user attributes t.attributes;
         Ok user

  let github_orgs t ~user =
    match String.Map.find user t.attributes with
    | Some attrs -> Lazy.force attrs.github_orgs
    | None -> Lwt.return []

  let can_read_github t ~user project =
    match String.Map.find user t.attributes with
    | Some attrs -> attrs.can_read_github project
    | None -> Lwt.return false
end

type server = {
  auth : Auth.t;
  session_backend : Session.Backend.t;
  web_config : CI_web_templates.t;
  has_role : role -> user:string option -> bool Lwt.t;
}

let cookie_key t =
  "__ci_session:" ^ t.web_config.CI_web_templates.name

let rec matches_acl ~auth ~user acl =
  match acl, user with
  | `Everyone, _ -> Lwt.return true
  | `Username required, Some actual -> Lwt.return (required = actual)
  | `Github_org org, Some user -> Auth.github_orgs auth ~user >|= List.mem org
  | `Can_read project, Some user -> Auth.can_read_github auth ~user project
  | `Any xs, _ -> Lwt_list.exists_s (matches_acl ~auth ~user) xs
  | (`Username _ | `Github_org _ | `Can_read _), None -> Lwt.return false

let server ~auth ~web_config ~session_backend =
  let has_role r ~user =
    match r with
    | `Reader -> matches_acl web_config.CI_web_templates.can_read ~auth ~user
    | `Builder -> matches_acl web_config.CI_web_templates.can_build ~auth ~user
    | `LoggedIn -> Lwt.return (user <> None)
  in
  let session_backend = Session.connect session_backend in
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
          Wm.respond 404 rd ~body:(`String "No such static resource")
        )
      ) else (
        Log.debug (fun f -> f "Invalid static resource name %S" name);
        Wm.respond 404 rd ~body:(`String "Invalid static resource name")
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
        Wm.respond 404 rd ~body:(`String "No such static resource")
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
        begin Lwt_list.for_all_s (t.has_role ~user:username) roles_needed >>= function
          | true -> Wm.continue `Authorized rd
          | false -> Wm.continue (`Redirect (CI_web_templates.Error.(uri permission_denied))) rd
        end
      | None ->
        Lwt_list.for_all_s (t.has_role ~user:None) roles_needed >>= function
        | true -> Wm.continue `Authorized rd
        | false ->
          let login_redirect =
            match Uri.path rd.Wm.Rd.uri with
            | "/auth/logout" -> None
            | _ -> Some (Uri.path_and_query rd.Wm.Rd.uri)
          in
          let value = {session with Session_data.login_redirect} in
          self#session_set (Session_data.to_string value) rd >>= fun () ->
          Wm.continue (`Redirect (Uri.of_string "/auth/login")) rd
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
    let github = Auth.github_login_url ~csrf_token t.auth in
    let html = CI_web_templates.login_page ?github ~csrf_token ~user:username t.web_config in
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
            CI_prometheus.Counter.inc_one Metrics.local_login_ok_total;
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
            CI_prometheus.Counter.inc_one Metrics.local_login_rejected_total;
            Wm.respond 403 ~body:(`String "Invalid username/password") rd
end

class github_callback t = object(self)
  inherit resource_with_session t

  method! allowed_methods rd =
    Wm.continue [`GET] rd

  method content_types_provided rd =
    Wm.continue [
      "text/html", self#to_html;
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private to_html rd =
    self#session rd >>= fun session_data ->
    let expected_token = session_data.Session_data.csrf_token in
    let reject msg =
      CI_prometheus.Counter.inc_one Metrics.github_login_rejected_total;
      Wm.respond 403 ~body:(`String msg) rd
    in
    match Uri.get_query_param rd.Wm.Rd.uri "state" with
    | None -> reject "Missing state"
    | Some provided_token when provided_token <> expected_token ->
      Log.info (fun f -> f "Expecting state %S; got %S" expected_token provided_token);
      reject "Incorrect state"
    | Some _ ->
      match Uri.get_query_param rd.Wm.Rd.uri "code" with
      | None -> reject "Missing code"
      | Some code ->
        Auth.handle_github_callback t.auth ~code >>= function
        | Error err -> reject err
        | Ok user ->
            let session = {session_data with Session_data.username = Some user} in
            self#session_set (Session_data.to_string session) rd >>= fun () ->
            begin match session.Session_data.login_redirect with
              | None -> Lwt.return "/"
              | Some redirect ->
                let value = {session with Session_data.login_redirect = None} in
                self#session_set (Session_data.to_string value) rd >>= fun () ->
                Lwt.return redirect
            end >>= fun redirect ->
            CI_prometheus.Counter.inc_one Metrics.github_login_ok_total;
            Wm.respond 303 (Wm.Rd.redirect redirect rd)
end

let pp_path =
  Fmt.list ~sep:(Fmt.(const string) ", ") Fmt.string

let callback ~routes _conn request body =
  Metrics.record_request request;
  CI_prometheus.Summary.time Metrics.response_time_seconds @@ fun () ->
  CI_prometheus.Gauge.track_inprogress Metrics.requests_in_progress @@ fun () ->
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
  Metrics.record_response status;
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
