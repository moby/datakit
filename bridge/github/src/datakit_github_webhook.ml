(*  Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>        *)
(*  Copyright (c) 2016 Thomas Gazagnaire <thomas@gazagnaire.org> *)

let src =
  Logs.Src.create "dkt-github-webhook" ~doc:"Github to Git bridge webhooks"
module Log = (val Logs.src_log src : Logs.LOG)

open Cohttp
open Github_t
open Datakit_github
open Lwt.Infix
open Astring

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

  type t = {
    port    : int;
    mutable services: service list;
    mutable dispatch: service handler;
  }

  let service_not_found s = {
    name    = "Default404";
    routes  = Re.any;
    handler = Lwt.(fun _id req _body ->
        let routes = List.map (fun s -> s.name) s in
        let body =
          Fmt.strf "404: Resource '%s' not found\nExisting services:\n%a"
            (Uri.path (Request.uri req))
            Fmt.(list ~sep:(unit "\n") string) routes
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body ()
        >|= some
      );
  }

  let make_dispatch services =
    let routes = List.map (fun s -> Re.compile s.routes, s) services in
    fun _id req _body ->
      Log.debug (fun l ->
          l "dispatch %a" Fmt.(Dump.list string)
            (List.map (fun (_, x) -> x.name) routes));
      let rec dispatch = function
        | []              -> service_not_found services
        | (rt, s) :: rest ->
          let path = Uri.path (Request.uri req) in
          let m = Re.execp rt path in
          if m then s else dispatch rest
      in
      Lwt.return (dispatch routes)

  let create port = { port; services=[]; dispatch = make_dispatch [] }

  let add_service t service =
    t.services <- service :: t.services;
    t.dispatch <- make_dispatch t.services

  let listen server =
    let port = server.port in
    let callback t conn_id req body =
      t.dispatch conn_id req body >>= fun service ->
      let pathquery = Uri.path_and_query (Request.uri req) in
      Log.debug (fun l ->
          l "%s for %s dispatched to %s"
            (Code.string_of_method (Request.meth req)) pathquery service.name);
      service.handler conn_id req body >>= function
      | None ->
        Log.err (fun l -> l "%s refused to service %s" service.name pathquery);
        (* FIXME: should be a better error *)
        Lwt.fail_with "listen"
      | Some resp -> Lwt.return resp
    in
    let conn_closed (_, conn_id) =
      Log.debug (fun l -> l "conn %s closed" (Connection.to_string conn_id))
    in
    let config =
      Cohttp_lwt_unix.Server.make ~callback:(callback server) ~conn_closed ()
    in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port)) config

  let service name ~routes ~handler = { name; routes; handler }

end

(** Webhooks *)
module Webhook = struct

  type status = Indicated | Pending | Timeout | Unauthorized | Connected

  type t = {
    id          : int64;
    url         : Uri.t;
    secret      : Cstruct.t;
    repo        : Repo.t;
    update_event: unit Lwt_condition.t;
    token       : Github.Token.t;
    handler     : HTTP.response option HTTP.handler;
    mutable status    : status;
    mutable last_event: Time.t;
  }

  let secret_prefix = "datakit"

  let () = Random.self_init ()

  let hmac ~secret message =
    Nocrypto.Hash.SHA1.hmac ~key:secret (Cstruct.of_string message)
    |> fun x -> Hex.of_cstruct x

  let verification_failure =
    let body = "403: Forbidden (Request verification failure)" in
    Cohttp_lwt_unix.Server.respond_string ~status:`Forbidden ~body ()
    >|= some

  let verify_event ~secret req body =
    match Header.get (Request.headers req) "x-hub-signature" with
    | None      -> Lwt.return_none
    | Some sign ->
      match String.cut ~sep:"=" sign with
      | Some ("sha1", hex) ->
        Cohttp_lwt_body.to_string body >|= fun body ->
        let `Hex hmac = hmac ~secret body in
        if String.equal hex hmac then Some body else None
      | _ -> Lwt.return_none

  let new_secret prefix =
    let `Hex s = Hex.of_cstruct (Nocrypto.Rng.generate 20) in
    Cstruct.of_string (prefix ^ ":" ^ s)

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
        web_hook_config_secret       = Some (Cstruct.to_string secret);
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

  let handler t id req body =
    verify_event ~secret:t.secret req body >>= function
    | None ->
      t.status <- Unauthorized;
      Log.err (fun l ->
          l "FAILURE: Hook registration of %s for %a"
            (Uri.to_string t.url) Repo.pp t.repo);
      Lwt_condition.broadcast t.update_event ();
      verification_failure
    | Some body ->
      t.last_event <- Time.now ();
      t.status <- Connected;
      Lwt_condition.broadcast t.update_event ();
      Lwt.async (fun () ->
          t.handler id req (`String body) >|= function
          | None   -> ()
          | Some _ -> ());
      Log.info (fun l ->
          l "SUCCESS: Hook registration of %s for %a"
            (Uri.to_string t.url) Repo.pp t.repo);
      Cohttp_lwt_unix.Server.respond_string ~status:`No_content ~body:"" ()
      >|= some

  let of_hook ~token (repo, handler) hook secret =
    match web_hook_config hook  with
    | None   -> assert false
    | Some w -> {
        id           = hook.hook_id;
        url          = Uri.of_string w.web_hook_config_url;
        status       = Indicated;
        update_event = Lwt_condition.create ();
        last_event   = Time.min;
        handler; token; secret; repo;
      }

  let test ~token t =
    let open Github.Monad in
    let f =
      Github.Hook.test ~token ~user:t.repo.Repo.user ~repo:t.repo.Repo.repo
        ~id:t.id ()
      |> map ignore
    in
    run f

  let register registry t = Hashtbl.replace registry (Uri.path t.url) t

  let check_connectivity t timeout_s =
    let timeout () =
      Lwt_unix.sleep timeout_s >>= fun () ->
      t.status <- Timeout;
      Lwt_condition.broadcast t.update_event ();
      Lwt.return ()
    in
    let rec wait () =
      Log.debug (fun l -> l "wait %Ld" t.id);
      Lwt_condition.wait t.update_event >>= fun () ->
      Log.debug (fun l -> l "after-wait %Ld" t.id);
      if t.status = Connected then Lwt.return_unit
      else wait ()
    in
    Lwt.pick [ timeout (); wait () ]

  let connect ~token registry url ({Repo.user; repo}, _ as r) =
    let points_to_us h =
      match web_hook_config h with
      | None   -> false
      | Some w -> w.web_hook_config_url = Uri.to_string url
    in
    let create =
      let open Github.Monad in
      Github.Hook.for_repo ~token ~user ~repo ()
      |> Github.Stream.to_list
      >>= fun hooks ->
      List.fold_left (fun m h ->
          m >>= fun () ->
          if not (points_to_us h) then return () else
            let id = h.hook_id in
            Log.info (fun l -> l "Github.Hook.delete %s/%s/%Ld" user repo id);
            Github.Hook.delete ~token ~user ~repo ~id ()
            |> map Github.Response.value
        ) (return ()) hooks
      >>= fun () ->
      let secret = new_secret secret_prefix in
      let hook = new_hook url secret in
      Log.info (fun l ->
          l "Github.Hook.create %s/%s (%s)" user repo @@ Uri.to_string url);
      Github.Hook.create ~token ~user ~repo ~hook () >>~ fun hook ->
      of_hook ~token r hook secret
      |> return
    in
    Github.Monad.run create >>= fun t ->
    t.status <- Pending;
    register registry t;
    Lwt.join [
      check_connectivity t 10.;
      test ~token t;
    ] >|= fun () ->
    t

end

type s = {
  uri     : Uri.t;
  registry: (string, Webhook.t) Hashtbl.t;
  token   : Github.Token.t;
  mutable repos: Repo.Set.t
}

type t = {
  s: s;
  mutable events: (Repo.t * Github_t.event_constr) list;
  http: HTTP.t;
  cond: unit Lwt_condition.t;
}

let empty token uri =
  let registry = Hashtbl.create 10 in
  let repos = Repo.Set.empty in
  { uri; registry; token; repos }

let github_error_str = Fmt.strf "GitHub connection for %a failed:" Repo.pp

let safe_parse f x =
  try Some (f x)
  with Ag_oj_run.Error e ->
    Log.err (fun l -> l "parsing error: %s\n%s" e x);
    None

let event_type req =
  let parse s = safe_parse Github_j.event_type_of_string ("\"" ^ s ^ "\"") in
  match Header.get (Request.headers req) "x-github-event" with
  | Some s -> parse s
  | None   -> None

let parse_event t b: Github_t.event_constr option =
  let ( >|= ) x f = match safe_parse x b with
    | None   -> None
    | Some x -> Some (f x)
  in
  match t with
  | `Push        -> Github_j.push_event_of_string   >|= fun x -> `Push x
  | `Status      -> Github_j.status_event_of_string >|= fun x -> `Status x
  | `Delete      -> Github_j.delete_event_of_string >|= fun x -> `Delete x
  | `Create      -> Github_j.create_event_of_string >|= fun x -> `Create x
  | `PullRequest ->
    Github_j.pull_request_event_of_string >|= fun x -> `PullRequest x
  | _            -> None

let pp_event ppf = function
  | `Push _       -> Fmt.string ppf "push"
  | `Status _      -> Fmt.string ppf "status"
  | `Delete _      -> Fmt.string ppf "delete"
  | `Create _      -> Fmt.string ppf "create"
  | `PullRequest _ -> Fmt.string ppf "pull-request"
  | _ -> Fmt.string ppf "unknown"

let notification_handler t repo _id req body =
  Cohttp_lwt_body.to_string body >>= fun body ->
  let e = match event_type req with
    | None     -> None
    | Some typ -> parse_event typ body
  in
  match e with
  | Some e ->
    Log.info (fun l ->
        l "received webhook event for %a: %a" Repo.pp repo pp_event e);
    t.events <- (repo, e) :: t.events;
    Lwt_condition.signal t.cond ();
    let body = Fmt.strf "Got event for %a\n" Repo.pp repo in
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
    >|= some
  | None ->
    Log.info (fun l -> l "received unknown webhook event");
    Lwt.return_none

let service registry uri service_fn =
  let root = Uri.path uri in
  let routes = Re.str root in
  let handler conn_id req body =
    let uri = Request.uri req in
    try
      let endpoint = Hashtbl.find registry (Uri.path uri) in
      Webhook.handler endpoint conn_id req body
    with Not_found ->
      Lwt.return_none
  in
  service_fn ~routes ~handler

let (++) x y = Uri.resolve "" x (Uri.of_string y)

let watch t repo =
  if Repo.Set.mem repo t.s.repos then (
    Log.debug (fun l -> l "Alreday watching %a" Repo.Set.pp t.s.repos);
    Lwt.return_unit
  ) else
    let uri = t.s.uri ++ Fmt.to_to_string Repo.pp repo in
    Log.info (fun l ->
        l "Connecting GitHub to callback %s\n%!" (Uri.to_string uri));
    let service =
      let msg = Fmt.strf "GitHub listener for %a" Repo.pp repo in
      service t.s.registry uri (HTTP.service msg)
    in
    let err = github_error_str repo in
    HTTP.add_service t.http service;
    Webhook.connect ~token:t.s.token t.s.registry uri
      (repo, notification_handler t repo)
    >|= fun endpoint -> match endpoint.Webhook.status with
    | Webhook.Indicated    -> Log.err (fun l -> l "%s wedged prerequest" err)
    | Webhook.Pending      -> Log.err (fun l -> l "%s wedged pending" err)
    | Webhook.Timeout      -> Log.err (fun l -> l "%s handshake timeout" err)
    | Webhook.Unauthorized -> Log.err (fun l -> l "%s authorization failed" err)
    | Webhook.Connected    ->
      Log.info (fun l -> l "%a connected" Repo.pp repo);
      t.s.repos <- Repo.Set.add repo t.s.repos

let create token uri =
  let port = match Uri.port uri with None -> 80 | Some p -> p in
  let http = HTTP.create port in
  let s = empty token uri in
  let cond = Lwt_condition.create () in
  { s; http; events = []; cond }

let repos t = t.s.repos
let run t = HTTP.listen t.http
let events t = List.rev t.events
let clear t = t.events <- []
let wait t = Lwt_condition.wait t.cond
