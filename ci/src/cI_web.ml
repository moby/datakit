open! Astring
open Lwt.Infix
open CI_utils

module Wm = CI_web_utils.Wm
module Rd = Webmachine.Rd

type t = {
  ci : CI_engine.t;
  logs : CI_live_log.manager;
  server : CI_web_utils.server;
  dashboards : CI_target.ID_Set.t CI_projectID.Map.t;
}

class virtual http_page t = object(self)
  inherit CI_web_utils.protected_page t.server

  method virtual private render : (user:string -> [ `Html ] Tyxml.Html.elt, Cohttp_lwt_body.t) Wm.op

  method content_types_provided rd =
    Wm.continue [
      "text/html" , self#to_html;
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private to_html rd =
    self#render rd >>= fun (resp, rd) ->
    match resp with
    | Wm.Error _ as e -> Lwt.return (e, rd)
    | Wm.Ok html ->
      match self#authenticated_user with
      | None -> assert false
      | Some user ->
      let body = Fmt.to_to_string (Tyxml.Html.pp ()) (html ~user) in
      Wm.continue (`String body) rd
end

class user_page t = object(self)
  inherit http_page t
  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.user_page ~csrf_token) rd
end

class main t = object(self)
  inherit http_page t
  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.main_page ~csrf_token ~ci:t.ci ~dashboards:t.dashboards) rd
end

class pr_list t = object
  inherit http_page t
  method private render rd = Wm.continue (CI_web_templates.prs_page ~ci:t.ci) rd
end

class branch_list t = object
  inherit http_page t
  method private render rd = Wm.continue (CI_web_templates.branches_page ~ci:t.ci) rd
end

class tag_list t = object
  inherit http_page t
  method private render rd = Wm.continue (CI_web_templates.tags_page ~ci:t.ci) rd
end

let read_log ci state =
  let open CI_result.Step_log in
  let seen = ref String.Set.empty in
  CI_engine.dk ci >>= fun dk ->
  let rec aux = function
    | Empty -> Lwt.return `No_log
    | Live log when String.Set.mem (CI_live_log.branch log) !seen -> Lwt.return `No_log
    | Saved { branch; _ } when String.Set.mem branch !seen -> Lwt.return `No_log
    | Live log ->
      let branch = CI_live_log.branch log in
      seen := String.Set.add branch !seen;
      (* Check if the branch already exists - if so, provide a link to the previous build logs *)
      DK.branch dk branch >>*= fun b ->
      begin DK.Branch.head b >>*= function
        | None -> Lwt.return None
        | Some _ -> Lwt.return (Some branch)
      end >|= fun branch ->
      `Live_log (CI_live_log.contents log ^ "\n(still running...)", branch)
    | Pair (a, Empty) -> aux a
    | Pair (Empty, b) -> aux b
    | Pair (a, b) ->
      aux a >>= fun a ->
      aux b >>= fun b ->
      Lwt.return (`Pair (a, b))
    | Saved ({ commit; branch } as saved) ->
      seen := String.Set.add branch !seen;
      CI_engine.dk ci >>= fun dk ->
      CI_cache.read_log dk saved >|= function
      | Ok log -> `Saved_log (log, commit, branch)
      | Error (`Msg e) -> `Saved_log (e, commit, branch)
  in
  aux state.CI_state.logs

class pr_page t = object(self)
  inherit http_page t
  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let project = Rd.lookup_path_info_exn "project" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = int_of_string id in
    let project = CI_projectID.v ~user ~project in
    let projects = CI_engine.targets t.ci in
    match CI_projectID.Map.find project projects with
    | None -> Wm.respond 404 rd
    | Some (prs, _) ->
      match IntMap.find id prs with
      | None -> Wm.respond 404 rd
      | Some target ->
        CI_engine.jobs target |> Lwt_list.map_s (fun job ->
            let state = CI_engine.state job in
            read_log t.ci state >|= fun log_data ->
            (job, log_data)
          )
        >>= fun jobs ->
        self#session rd >>= fun session_data ->
        let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
        Wm.continue (CI_web_templates.pr_page ~csrf_token ~target jobs) rd
end

class ref_page t = object(self)
  inherit http_page t
  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let project = Rd.lookup_path_info_exn "project" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = CI_web_templates.unescape_ref id in
    let project = CI_projectID.v ~user ~project in
    let projects = CI_engine.targets t.ci in
    match CI_projectID.Map.find project projects with
    | None -> Wm.respond 404 rd
    | Some (_, refs) ->
      match Datakit_path.Map.find id refs with
      | exception Not_found -> Wm.respond 404 rd
      | target ->
        CI_engine.jobs target |> Lwt_list.map_s (fun job ->
            let state = CI_engine.state job in
            read_log t.ci state >|= fun log_data ->
            (job, log_data)
          )
        >>= fun jobs ->
        self#session rd >>= fun session_data ->
        let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
        Wm.continue (CI_web_templates.ref_page ~csrf_token ~target jobs) rd
end

let rebuild ~ci ~uri ~project ~target ~job ~redirect rd =
  let job = Uri.pct_decode job in
  match Uri.get_query_param uri "action" with
  | Some action ->
    CI_engine.rebuild ci project ~target ~job action >>= fun () ->
    Wm.continue true (Rd.redirect redirect rd)
  | None ->
    let body = `String (Fmt.strf "Missing 'action' parameter in POST") in
    Wm.respond ~body 400 rd

class pr_rebuild t = object
  inherit CI_web_utils.post_page t.server
  method! private process_post rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let project = Rd.lookup_path_info_exn "project" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = int_of_string id in
    let job = Rd.lookup_path_info_exn "job" rd in
    let project = CI_projectID.v ~user ~project in
    let target = `PR id in
    rebuild ~ci:t.ci ~uri:rd.Rd.uri ~project ~target ~job ~redirect:(CI_web_templates.pr_url project id) rd
end

class ref_rebuild t = object
  inherit CI_web_utils.post_page t.server
  method! private process_post rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let project = Rd.lookup_path_info_exn "project" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = CI_web_templates.unescape_ref id in
    let job = Rd.lookup_path_info_exn "job" rd in
    let project = CI_projectID.v ~user ~project in
    let target = `Ref id in
    rebuild ~ci:t.ci ~uri:rd.Rd.uri ~project ~target ~job ~redirect:(CI_web_templates.ref_url project id) rd
end

class cancel t = object
  inherit CI_web_utils.post_page t.server
  method! private process_post rd =
    let branch = Rd.lookup_path_info_exn "branch" rd in
    match CI_live_log.lookup ~branch t.logs with
    | None ->
      let body = Fmt.strf "Branch %S is not currently building" branch in
      Wm.respond ~body:(`String body) 404 rd
    | Some log ->
      CI_live_log.cancel log >>= function
      | Ok () -> Wm.continue true (Rd.redirect "/" rd)
      | Error body -> Wm.respond ~body:(`String body) 404 rd
end

let mime_type uri =
  match String.take ~sat:((<>) '.') ~rev:true (Uri.path uri) with
  | "css"   -> Some "text/css"
  | "js"    -> Some "text/javascript"
  | "eot"   -> Some "application/vnd.ms-fontobject"
  | "svg"   -> Some "image/svg+xml"
  | "ttf"   -> Some "application/x-font-ttf"
  | "woff"  -> Some "application/font-woff"
  | "woff2" -> Some "font/woff2"
  | "png"   -> Some "image/png"
  | _       -> None

let serve ~logs ~mode ~ci ~auth ~dashboards =
  let server = CI_web_utils.server ~auth in
  let t = { logs; ci; server; dashboards } in
  let routes = [
    (* Auth *)
    ("/auth/login",     fun () -> new CI_web_utils.login_page t.server);
    ("/auth/logout",    fun () -> new CI_web_utils.logout_page t.server);
    ("/user/profile",   fun () -> new user_page t);
    (* Overview pages *)
    ("/",               fun () -> new main t);
    ("pr",              fun () -> new pr_list t);
    ("branch",          fun () -> new branch_list t);
    ("tag",             fun () -> new tag_list t);
    (* Individual targets *)
    ("pr/:user/:project/:id",                   fun () -> new pr_page t);
    ("pr/:user/:project/:id/:job/rebuild",      fun () -> new pr_rebuild t);
    ("ref/:user/:project/:id",                  fun () -> new ref_page t);
    ("ref/:user/:project/:id/:job/rebuild",     fun () -> new ref_rebuild t);
    (* Logs *)
    ("cancel/:branch",                          fun () -> new cancel t);
    (* Static resources *)
    (":dir/:name",      fun () -> new CI_web_utils.static_crunch ~mime_type CI_static.read);
  ] in
  CI_web_utils.serve ~mode ~routes
