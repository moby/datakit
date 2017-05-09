open Datakit_github
open! Astring
open Lwt.Infix
open CI_utils
open CI_utils.Infix

module Wm = CI_web_utils.Wm
module Rd = Webmachine.Rd

type t = {
  ci : CI_engine.t;
  logs : CI_live_log.manager;
  server : CI_web_utils.server;
  dashboards : CI_target.Set.t Repo.Map.t;
}

let opt_query_decode name rd =
  match Uri.get_query_param rd.Rd.uri name with
  | None -> None
  | Some x -> Some (Uri.pct_decode x)

class user_page t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`LoggedIn]

  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.user_page ~csrf_token) rd
end

class main t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd =
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.main_page ~csrf_token ~ci:t.ci ~dashboards:t.dashboards) rd
end

class error t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = []

  method private render rd =
    let id = Rd.lookup_path_info_exn "id" rd in
    Wm.continue (CI_web_templates.error_page id) rd
end

let check_metrics_token server provided =
  match String.cut ~sep:" " provided with
  | Some (typ, provided) when String.Ascii.lowercase typ = "bearer" ->
    begin match (CI_web_utils.web_config server).CI_web_templates.metrics_token with
      | None -> false
      | Some (`SHA256 expected_hash) ->
        let user_hash = (Nocrypto.Hash.SHA256.digest (Cstruct.of_string provided)) in
        if Cstruct.equal expected_hash user_hash then true
        else (
          Log.info (fun f ->
              f "Bad /metrics token. Expected:@\n%aGot:@\n%a"
                Cstruct.hexdump_pp expected_hash
                Cstruct.hexdump_pp user_hash
            );
          false
        )
    end
  | _ ->
    Log.info (fun f -> f "Bad token %S" provided);
    false

class metrics t = object(self)
  inherit [Cohttp_lwt_body.t] Wm.resource

  method content_types_provided rd =
    Wm.continue [
      "text/plain; version=0.0.4" , self#to_plain;
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method! is_authorized rd =
    match Cohttp.Header.get_authorization rd.Rd.req_headers with
    | Some (`Other token) when check_metrics_token t.server token -> Wm.continue `Authorized rd
    | _ ->
      Wm.respond ~body:(`String "Bad token") 403 rd

  method private to_plain rd =
    let data = Prometheus.(CollectorRegistry.collect CollectorRegistry.default) in
    let body = Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data in
    Wm.continue (`String body) rd
end

class pr_list t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.prs_page ~ci:t.ci) rd
end

class branch_list t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.branches_page ~ci:t.ci) rd
end

class tag_list t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd = Wm.continue (CI_web_templates.tags_page ~ci:t.ci) rd
end

let load_jobs t target rd =
  match Uri.get_query_param rd.Rd.uri "history" with
  | None ->
    CI_engine.latest_state t.ci target >|= default CI_history.State.empty
  | Some commit ->
    CI_engine.dk t.ci >>= fun dk ->
    DK.commit dk commit >>*= fun commit ->
    CI_history.load commit

class pr_page t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let repo = Rd.lookup_path_info_exn "repo" rd in
    let id = Rd.lookup_path_info_exn "id" rd in
    let id = int_of_string id in
    let repo = Repo.v ~user ~repo in
    let prs = CI_engine.prs t.ci in
    let target = `PR (repo, id) in
    let test = opt_query_decode "test" rd in
    match Repo.Map.find repo prs with
    | None  -> Wm.respond 404 rd ~body:(`String "No such project")
    | Some prs ->
      load_jobs t target rd >>= fun commit ->
      self#session rd >>= fun session_data ->
      let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
      let title =
        match PR.Index.find (repo, id) prs with
        | None -> None
        | Some engine_target ->
          match CI_engine.target engine_target with
          | `PR pr -> Some (PR.title pr)
          | _ -> assert false
      in
      Wm.continue (CI_web_templates.target_page ?test ~csrf_token ?title ~target commit) rd
end

class ref_page t = object(self)
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let repo = Rd.lookup_path_info_exn "repo" rd in
    let id = CI_target.unescape_ref rd.Rd.dispatch_path in
    let repo = Repo.v ~user ~repo in
    let target = `Ref (repo, id) in
    let test = opt_query_decode "test" rd in
    load_jobs t target rd >>= fun state ->
    self#session rd >>= fun session_data ->
    let csrf_token = CI_web_utils.Session_data.csrf_token session_data in
    Wm.continue (CI_web_templates.target_page ?test ~csrf_token ~target state) rd
end

class commit_page t = object
  inherit CI_web_utils.html_page t.server

  method private required_roles = [`Reader]

  method private render rd =
    let user = Rd.lookup_path_info_exn "user" rd in
    let repo = Rd.lookup_path_info_exn "repo" rd in
    let commit = Rd.lookup_path_info_exn "id" rd in
    let test = opt_query_decode "test" rd in
    let repo = Datakit_github.Repo.v ~user ~repo in
    let live_targets = CI_engine.targets_of_commit t.ci repo commit in
    CI_engine.dk t.ci >>= fun dk ->
    let src_commit = Datakit_github.Commit.v repo commit in
    CI_history.builds_of_commit dk src_commit >>= fun archived_targets ->
    let archived_targets =
      live_targets |> List.fold_left (fun acc target ->
          CI_target.Map.remove target acc
        ) archived_targets
      |> CI_target.Map.bindings
    in
    match live_targets, archived_targets with
    | [t], [] -> Wm.respond 307 (Rd.redirect (Uri.to_string (CI_target.path ?test t)) rd)
    | _ -> Wm.continue (CI_web_templates.commit_page ~commit ~archived_targets ?test live_targets) rd
end

let max_escape_length = 20

type graphics_state = {
  bold : bool;
  fg : string option;
  bg : string option;
}

let default_gfx_state = {
  bold = false;
  fg = None;
  bg = None;
}

let format_colour = function
  | `Default -> None
  | `Black -> Some "black"
  | `Blue -> Some "blue"
  | `Cyan -> Some "cyan"
  | `Green -> Some "green"
  | `Magenta -> Some "magenta"
  | `Red -> Some "red"
  | `White -> Some "white"
  | `Yellow -> Some "yellow"

let apply_ctrl state = function
  | `Bold -> { state with bold = true }
  | `NoBold -> { state with bold = false }
  | `FgCol c -> { state with fg = format_colour c }
  | `BgCol c -> { state with bg = format_colour c }
  | `Italic | `NoItalic | `NoReverse | `NoUnderline | `Reverse  | `Underline -> state
  | `Reset -> default_gfx_state

let pp_style = Fmt.(list ~sep:(const string " ")) Fmt.string

let with_style s txt =
  match s with
  | {bold = false; fg = None; bg = None} -> txt
  | {bold; fg; bg} ->
    let cl ty = function
      | None   when bold && ty = "fg" -> ["fg-bright-white"]
      | Some c when bold && ty = "fg" -> [Printf.sprintf "fg-bright-%s" c]
      | Some c -> [Printf.sprintf "%s-%s" ty c]
      | None -> []
    in
    let style = if bold then ["bold"] else [] in
    let style = cl "fg" fg @ style in
    let style = cl "bg" bg @ style in
    Fmt.strf "<span class='%a'>%s</span>" pp_style style txt

(* [buf] is any partial escape sequence we weren't able to fully parse last time. *)
let process_escapes ~gfx_state ~buf data =
  let output = Buffer.create (String.length data * 2) in
  let add = Buffer.add_string output in
  let module Stream = CI_char_stream in
  let write (s, first, stop) =
    let data = String.with_range s ~first ~len:(stop - first) in
    add (Xml_print.encode_unsafe_char data |> with_style !gfx_state)
  in
  let rec aux s =
    match CI_escape_parser.parse s with
    | `Literal i when Stream.equal i s -> `Done ""
    | `Literal i -> write Stream.(s -- i); aux i
    | `Incomplete when Stream.avail s >= max_escape_length -> add "<b>ESCAPE-TOO-LONG</b>"; aux (Stream.skip s)
    | `Incomplete -> `Done (Stream.to_string s)
    | `Invalid i -> aux i
    | `Escape (`Reset, i) -> gfx_state := default_gfx_state; aux i
    | `Escape (`Ctrl (`SelectGraphicRendition c), i) -> gfx_state := List.fold_left apply_ctrl !gfx_state c; aux i
  in
  let `Done unprocessed = aux (Stream.of_string (!buf ^ data)) in
  buf := unprocessed;
  Buffer.contents output

class live_log_page t = object(self)
  inherit CI_web_utils.protected_page t.server

  method content_types_provided rd =
    Wm.continue [
      "text/html" , self#to_html;
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private required_roles = [`Reader]

  method private to_html rd =
    let branch = Uri.pct_decode (Rd.lookup_path_info_exn "branch" rd) in
    let user = self#authenticated_user in
    let web_config = CI_web_utils.web_config t.server in
    CI_engine.dk t.ci >>= fun dk ->
    DK.branch dk branch >>*= fun b ->
    DK.Branch.head b >>*= fun head ->
    match CI_live_log.lookup t.logs ~branch with
    | None ->
      (* todo: find out what commit it turned into and serve that instead *)
      begin match head with
        | Some head ->
          let path = CI_web_templates.saved_log_frame_link ~branch ~commit:(DK.Commit.id head) in
          Wm.respond 307 (Rd.redirect path rd)
        | None ->
          Log.warn (fun f -> f "Live log %S not found, and no saved branch either" branch);
          Wm.respond 500 rd ~body:(`String "Log finished, but wasn't saved!")
      end
    | Some live_log ->
      let have_history = head <> None in
      let html = CI_web_templates.live_log_frame ~branch ~have_history in
      let template = Fmt.to_to_string (Tyxml.Html.pp ()) (html ~user web_config) in
      match String.cut ~sep:"@STREAM-GOES-HERE@" template with
      | None -> CI_utils.failf "Bad template output %S" template
      | Some (head, tail) ->
        let stream, out = Lwt_stream.create_bounded 100 in
        CI_live_log.stream live_log >>= fun src ->
        out#push head >>= fun () ->
        let buf = ref "" in
        let gfx_state = ref default_gfx_state in
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                 let rec aux = function
                   | None ->
                     out#push tail >>= fun () ->
                     out#close;
                     Lwt.return ()
                   | Some {CI_live_log.data; next} ->
                     let data = process_escapes ~gfx_state ~buf data in
                     begin if data = "" then Lwt.return () else out#push data end >>= fun () ->
                     Lazy.force next >>= aux
                 in
                 aux src
              )
              (fun ex ->
                 Log.warn (fun f -> f "Error streaming log: %a" CI_utils.pp_exn ex);
                 Lwt.return ()
              )
          );
        Wm.continue
          (`Stream stream)
          { rd with
            (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
            Rd.resp_headers = Cohttp.Header.add rd.Rd.resp_headers "X-Accel-Buffering" "no";
          }
end

class saved_log_page t = object(self)
  inherit CI_web_utils.protected_page t.server

  method content_types_provided rd =
    Wm.continue [
      "text/html" , self#to_html;
    ] rd

  method content_types_accepted rd =
    Wm.continue [] rd

  method private required_roles = [`Reader]

  method private to_html rd =
    let branch = Uri.pct_decode (Rd.lookup_path_info_exn "branch" rd) in
    let commit = Rd.lookup_path_info_exn "commit" rd in
    let user = self#authenticated_user in
    let web_config = CI_web_utils.web_config t.server in
    CI_engine.dk t.ci >>= fun dk ->
    DK.commit dk commit >>*= DK.Commit.tree >>*= fun tree ->
    DK.Tree.read_file tree CI_cache.Path.log >>= function
    | Error e ->
      let html = CI_web_templates.plain_error (Fmt.to_to_string DK.pp_error e) in
      let body = Fmt.to_to_string (Tyxml.Html.pp ()) (html ~user web_config) in
      Wm.continue (`String body) rd
    | Ok log_data ->
      let html = CI_web_templates.saved_log_frame ~commit ~branch in
      let template = Fmt.to_to_string (Tyxml.Html.pp ()) (html ~user web_config) in
      match String.cut ~sep:"@STREAM-GOES-HERE@" template with
      | None -> CI_utils.failf "Bad template output %S" template
      | Some (head, tail) ->
        let stream, out = Lwt_stream.create_bounded 100 in
        out#push head >>= fun () ->
        let buf = ref "" in
        let gfx_state = ref default_gfx_state in
        Lwt.async (fun () ->
            Lwt.catch
              (fun () ->
                 let rec aux i =
                   if i = Cstruct.len log_data then (
                     out#push tail >>= fun () ->
                     out#close;
                     Lwt.return ()
                   ) else (
                     let len = min 40960 (Cstruct.len log_data - i) in
                     let data = Cstruct.sub log_data i len |> Cstruct.to_string in
                     let data = process_escapes ~gfx_state ~buf data in
                     begin if data = "" then Lwt.return () else out#push data end >>= fun () ->
                     aux (i + len)
                   )
                 in
                 aux 0
              )
              (fun ex ->
                 Log.warn (fun f -> f "Error streaming log: %a" CI_utils.pp_exn ex);
                 Lwt.return ()
              )
          );
        Wm.continue
          (`Stream stream)
          { rd with
            (* Otherwise, an nginx reverse proxy will wait for the whole log before sending anything. *)
            Rd.resp_headers = Cohttp.Header.add rd.Rd.resp_headers "X-Accel-Buffering" "no";
          }
end

class rebuild t = object
  inherit CI_web_utils.post_page t.server

  method private required_roles = [`Builder]

  method! private process_post rd =
    let branch_name = Uri.pct_decode (Rd.lookup_path_info_exn "branch" rd) in
    match Uri.get_query_param rd.Rd.uri "redirect" with
    | None -> Wm.respond ~body:(`String "Missing redirect") 400 rd
    | Some redirect ->
      CI_engine.rebuild t.ci ~branch_name >>= fun () ->
      Wm.continue true (Rd.redirect redirect rd)
end

class cancel t = object
  inherit CI_web_utils.post_page t.server

  method private required_roles = [`Builder]

  method! private process_post rd =
    let branch = Uri.pct_decode (Rd.lookup_path_info_exn "branch" rd) in
    match CI_live_log.lookup ~branch t.logs with
    | None ->
      let body = Fmt.strf "Branch %S is not currently building" branch in
      Wm.respond ~body:(`String body) 404 rd
    | Some log ->
      CI_live_log.cancel log >>= function
      | Ok () -> Wm.continue true (Rd.redirect "/" rd)
      | Error body -> Wm.respond ~body:(`String body) 404 rd
end

module Settings = struct
  class index t = object
    inherit CI_web_utils.html_page t.server

    method private required_roles = [`Admin]

    method private render rd =
      Wm.continue (CI_web_templates.Settings.index) rd
  end
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

let routes ~logs ~ci ~server ~dashboards =
  let t = { logs; ci; server; dashboards } in
  [
    (* Auth *)
    ("auth/intro/:token", fun () -> new CI_web_utils.auth_intro t.server);
    ("auth/setup",     fun () -> new CI_web_utils.auth_setup t.server);
    ("auth/login",     fun () -> new CI_web_utils.login_page t.server);
    ("auth/logout",    fun () -> new CI_web_utils.logout_page t.server);
    ("auth/github-callback",    fun () -> new CI_web_utils.github_callback t.server);
    ("user/profile",   fun () -> new user_page t);
    (* Overview pages *)
    ("/",               fun () -> new main t);
    ("pr",              fun () -> new pr_list t);
    ("branch",          fun () -> new branch_list t);
    ("tag",             fun () -> new tag_list t);
    (* Individual targets *)
    (":user/:repo/pr/:id",     fun () -> new pr_page t);
    (":user/:repo/ref/*",      fun () -> new ref_page t);
    (":user/:repo/commit/:id", fun () -> new commit_page t);
    (* Logs *)
    ("log/live/:branch",                        fun () -> new live_log_page t);
    ("log/saved/:branch/:commit",               fun () -> new saved_log_page t);
    ("log/rebuild/:branch",                     fun () -> new rebuild t);
    ("cancel/:branch",                          fun () -> new cancel t);
    (* Settings *)
    ("settings",        fun () -> new Settings.index t);
    ("settings/github-auth", fun () -> new CI_web_utils.github_auth_settings t.server);
    (* Errors *)
    ("error/:id",       fun () -> new error t);
    (* Reporting *)
    ("metrics",         fun () -> new metrics t);
    (* Static resources *)
    (":dir/:name",      fun () -> new CI_web_utils.static_crunch ~mime_type CI_static.read);
  ]
