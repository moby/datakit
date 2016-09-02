(*  Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>        *)
(*  Copyright (c) 2016 Thomas Gazagnaire <thomas@gazagnaire.org> *)

let src =
  Logs.Src.create "dkt-github-webhook" ~doc:"Github to Git bridge webhooks"
module Log = (val Logs.src_log src : Logs.LOG)

open Cohttp
open Github_t
open Datakit_github
open Lwt.Infix

let some x = Some x

module Time: sig
  type t
  val min : t
  val now : unit -> t
end = struct
  type t = float
  let min = 0.
  let now () = Unix.gettimeofday ()
end

(** HTTP server *)
module HTTP = struct

  type body = Cohttp_lwt_body.t

  type response = Response.t * body

  type 'a handler = Cohttp_lwt_unix.Server.conn -> Request.t -> body -> 'a Lwt.t

  type service = {
    name   : string;
    routes : Re.t;
    handler: response option handler;
  }
  type service_search = { service : service; continue : unit -> service_search }

  type t = {
    port    : int;
    mutable services: service list;
    mutable dispatch: service_search handler;
  }

  let not_found_service = {
    name    = "Default404";
    routes  = Re.any;
    handler = Lwt.(fun _id req _body ->
        let body =
          Fmt.strf "404: Resource '%s' not found\n" (Uri.path (Request.uri req))
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body ()
        >|= some
      );
  }

  let make_dispatch services =
    let routes = List.map (fun s -> Re.compile s.routes, s) services in
    fun _id req _body ->
      let rec cascade = function
        | [] -> let rec fix =
                  {service=not_found_service; continue=fun () -> fix} in fix
        | (rt,service)::rest ->
          if Re.execp rt (Uri.path (Request.uri req))
          then {service; continue=fun () -> cascade rest}
          else cascade rest
      in
      Lwt.return (cascade routes)

  let create port = { port; services=[]; dispatch = make_dispatch [] }

  let with_service server service =
    let services = service::server.services in
    let dispatch = make_dispatch services in
    { server with services; dispatch }

  let listen server =
    let port = server.port in
    let rec callback kfn conn_id req body =
      kfn conn_id req body >>= fun {service; continue} ->
      let pathquery = Uri.path_and_query (Request.uri req) in
      Log.debug (fun l ->
          l "%s for %s dispatched to %s"
            (Code.string_of_method (Request.meth req)) pathquery service.name);
      service.handler conn_id req body >>= function
      | None ->
        Log.debug (fun l ->
            l "%s refused to service %s: continuing" service.name pathquery);
        callback (fun _ _ _ -> Lwt.return (continue ())) conn_id req body
      | Some resp -> Lwt.return resp
    in
    let conn_closed (_, conn_id) =
      Log.debug (fun l -> l "conn %s closed" (Connection.to_string conn_id))
    in
    let config =
      Cohttp_lwt_unix.Server.make ~callback:(callback server.dispatch)
        ~conn_closed ()
    in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port)) config

  let service name ~routes ~handler = { name; routes; handler }

end

(** Webhooks *)
module Webhook = struct

  type status = Indicated | Unauthorized | Connected

  type endpoint = {
    id          : int64;
    url         : Uri.t;
    secret      : Cstruct.t;
    repo        : Repo.t;
    status      : status;
    update_event: endpoint Lwt_condition.t;
    last_event  : Time.t;
    token       : Github.Token.t;
    handler     : HTTP.response option HTTP.handler;
  }

  let hmac ~secret message =
    Nocrypto.Hash.SHA1.hmac ~key:secret (Cstruct.of_string message)
    |> fun x -> Hex.of_cstruct x

  let verification_failure =
    let body = "403: Forbidden (Request verification failure)" in
    Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden ~body ()
    >|= some

  let verify_event ~secret req body =
    match Header.get (Request.headers req) "x-hub-signature" with
    | Some sign ->
      let hmac_label = Re_str.string_before sign 5 in
      let hmac_hex = `Hex (Re_str.string_after sign 5) in
      if hmac_label <> "sha1=" then Lwt.return_false
      else (
        Cohttp_lwt_body.to_string body >|= fun body ->
        hmac_hex = (hmac ~secret body)
      )
    | None -> Lwt.return_false

  let default_events = [
    `Create; `Delete; `Push; (* ref updates *)
    `Status;                 (* status updates *)
    `PullRequest;            (* PR updates *)
  ]

  let new_hook ?(events=default_events) url secret =
    let new_hook_config = `Web {
        web_hook_config_url          =  Uri.to_string url;
        web_hook_config_content_type = "json";
        web_hook_config_insecure_ssl = false; (* FIXME: review *)
        web_hook_config_secret       = secret;
      }
    in
    { new_hook_name   = "web";
      new_hook_active = true;
      new_hook_events = events;
      new_hook_config }

  let web_hook_config h = match h.hook_config with
    | `Web w -> Some w
    | `Raw _ ->
      Log.debug (fun l -> l "ignoring hook config for %s" h.hook_name);
      None

  let endpoint_of_hook ~token ~hook (repo, handler) =
    match web_hook_config hook  with
    | None   -> assert false
    | Some w ->
      let secret = match w.web_hook_config_secret with
        | None        -> Cstruct.of_string ""
        | Some secret -> Cstruct.of_string secret
      in {
        id           = hook.hook_id;
        url          = Uri.of_string w.web_hook_config_url;
        status       = Indicated;
        update_event = Lwt_condition.create ();
        last_event   = Time.min;
        token; secret; repo;
        handler      = (fun conn_id req body ->
            verify_event ~secret req body >>= function
            | true  -> handler conn_id req body
            | false -> verification_failure
          );
      }

  let register registry endpoint =
    let rec handler _id req body =
      verify_event ~secret:endpoint.secret req body >>= fun verified ->
      if verified then begin
        let last_event = Time.now () in
        let endpoint = { endpoint with last_event; status=Connected } in
        Hashtbl.replace registry (Uri.path endpoint.url) endpoint;
        Lwt_condition.broadcast endpoint.update_event endpoint;
        Log.info (fun l ->
            l "SUCCESS: Hook registration of %s for %a"
              (Uri.to_string endpoint.url) Repo.pp endpoint.repo);
        Cohttp_lwt_unix.Server.respond_string ~status:`No_content ~body:"" ()
        >|= some
      end
      else begin
        let endpoint = { endpoint with handler; status=Unauthorized } in
        Log.err (fun l ->
            l "FAILURE: Hook registration of %s for %a"
              (Uri.to_string endpoint.url) Repo.pp endpoint.repo);
        Hashtbl.replace registry (Uri.path endpoint.url) endpoint;
        Lwt_condition.broadcast endpoint.update_event endpoint;
        verification_failure
      end
    in
    Hashtbl.replace registry (Uri.path endpoint.url) {endpoint with handler}

  let connect ~token registry url x secret =
    let create =
      let open Github.Monad in
      let hook = new_hook url secret in
      let hook = {
        Github_t.hook_url = Uri.to_string url;
        hook_updated_at = "";
        hook_created_at = "";
        hook_events = [];
        hook_active = true;
        hook_name = hook.Github_t.new_hook_name;
        hook_config = hook.Github_t.new_hook_config;
        hook_id = 0L;
      } in
      endpoint_of_hook ~token ~hook x
      |> return
    in
    Github.Monad.run create >>= fun endpoint ->
    register registry {endpoint with status=Connected};
    Hashtbl.find registry (Uri.path url)
    |> Lwt.return

end

type s = {
  uri     : Uri.t;
  registry: (string, Webhook.endpoint) Hashtbl.t;
  token   : Github.Token.t;
  mutable repos: Repo.Set.t;
}

type t = {
  s: s;
  mutable events: Github_t.event list;
  http: HTTP.t;
  cond: unit Lwt_condition.t;
  secret  : string option;
}

let empty token uri =
  let registry = Hashtbl.create 10 in
  let repos = Repo.Set.empty in
  { uri; registry; token; repos }

let notify_query = "notify"
let path_seg = Re.(rep1 (compl [char '/']))
let github_error_str = Fmt.strf "GitHub connection for %a failed:" Repo.pp
let notify_re =
  Re.(seq [str "github/"; group path_seg; char '/'; group path_seg])

let notification_handler t repo _id _req body =
  Cohttp_lwt_body.to_string body >>= fun body ->
  t.events <- Github_j.event_of_string body :: t.events;
  Lwt_condition.signal t.cond ();
  let body = Fmt.strf "Got event for %a\n" Repo.pp repo in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
  >|= some

let watch t repo =
  if Repo.Set.mem repo t.s.repos then (
    Log.debug (fun l -> l "Alreday watching %a" Repo.Set.pp t.s.repos);
    Lwt.return_unit
  ) else (
    let url = Uri.resolve "" t.s.uri (Uri.of_string ("?"^notify_query)) in
    Log.info (fun l ->
        l "Connecting GitHub to callback %s\n%!" (Uri.to_string url));
    let err = github_error_str repo in
    Webhook.connect ~token:t.s.token t.s.registry url
      (repo, notification_handler t repo) t.secret
    >|= fun endpoint -> match endpoint.Webhook.status with
    | Webhook.Indicated    -> Log.err (fun l -> l "%s wedged prerequest" err)
    | Webhook.Unauthorized -> Log.err (fun l -> l "%s authorization failed" err)
    | Webhook.Connected    ->
      Log.info (fun l -> l "%a connected" Repo.pp repo);
      t.s.repos <- Repo.Set.add repo t.s.repos;
  )

let service { uri; registry; _ } service_fn =
  let root = Uri.path uri in
  let routes = Re.(seq [str root; notify_re]) in
  let handler conn_id req body =
    let uri = Request.uri req in
    if Uri.query uri <> [notify_query,[]] then Lwt.return_none
    else
      try
        let endpoint = Hashtbl.find registry (Uri.path uri) in
        endpoint.Webhook.handler conn_id req body
      with Not_found ->
        Lwt.return_none
  in
  service_fn  ~routes ~handler

let create token uri secret =
  let port = match Uri.port uri with None -> 80 | Some p -> p in
  let http = HTTP.create port in
  let s  = empty token uri in
  let service = service s (HTTP.service "GitHub listener") in
  let http = HTTP.with_service http service in
  let cond = Lwt_condition.create () in
  { s; http; events = []; cond; secret }

let repos t = t.s.repos
let run t = HTTP.listen t.http
let events t = List.rev t.events
let clear t = t.events <- []
let wait t = Lwt_condition.wait t.cond
