open Datakit_github
open! Astring
open! Tyxml.Html

type t = {
  name : string;
  state_repo : Uri.t option;
  metrics_token : [`SHA256 of Cstruct.t] option;
  listen_addr: [`HTTP of int | `HTTPS of int];
  github_scopes_needed : Github_t.scope list;
  can_read : CI_ACL.t;
  can_build : CI_ACL.t;
}

type page = user:string option -> [`Html] Tyxml.Html.elt

(* Requests access to all public and private repos. Needed to check if users can read private repos.
   Once https://github.com/mirage/ocaml-github/issues/185 is implemented, this mess can go away. *)
let default_github_scopes = [`Read_org; `Repo]

module Error = struct
  type t = string
  let no_state_repo = "no-state-repo"
  let permission_denied = "permission-denied"
  let logout_needed = "logout-needed"

  let uri_path id = "/error/" ^ id
  let uri id = Uri.of_string (uri_path id)
end

let job_state job =
  match CI_engine.state job with
  | None -> (Error (`Pending "(new)"), CI_output.Empty)
  | Some o -> o

let config
    ?(name="datakit-ci") ?state_repo ?metrics_token ?(listen_addr=`HTTPS 8443)
    ?(github_scopes_needed=default_github_scopes) ~can_read ~can_build () =
  let metrics_token =
    match metrics_token with
    | None -> None
    | Some (`SHA256 str) -> Some (`SHA256 (Cstruct.of_string str))
  in
  { name; state_repo; metrics_token; listen_addr; github_scopes_needed; can_read; can_build }

let state_repo_url t fmt =
  fmt |> Fmt.kstrf @@ fun path ->
  match t.state_repo with
  | Some base -> Fmt.strf "%a/%s" Uri.pp_hum base path
  | None -> Error.(uri_path no_state_repo)

let status_history_url t target =
  state_repo_url t "commits/%s" (CI_target.status_branch target)

let log_commit_url t commit =
  state_repo_url t "commit/%s" commit

let log_branch_history_url t branch =
  state_repo_url t "commits/%s/log" branch

let log_branch_results_url t branch =
  state_repo_url t "tree/%s" branch

let gh_target_url = function
  | `PR (repo, id) ->
    let { Repo.user; repo } = repo in
    let user = User.name user in
    Fmt.strf "https://github.com/%s/%s/pull/%d" user repo id
  | `Ref (repo, id) ->
    let { Repo.user; repo } = repo in
    let user = User.name user in
    match id with
    | "tags" :: id -> Fmt.strf "https://github.com/%s/%s/releases/tag/%s" user repo (String.concat ~sep:"/" id)
    | _            -> Fmt.strf "https://github.com/%s/%s/tree/%a" user repo Ref.pp_name id

let metadata_url t = function
  | `PR (repo, id) ->
    let { Repo.user; repo } = repo in
    let user = User.name user in
    state_repo_url t "commits/github-metadata/%s/%s/pr/%d" user repo id
  | `Ref (repo, id) ->
    let { Repo.user; repo } = repo in
    let user = User.name user in
    state_repo_url t "commits/github-metadata/%s/%s/ref/%a" user repo
      Ref.pp_name id

let commit_history_url t target ~metadata_commit ~src_commit =
  let { Repo.user; repo } = CI_target.repo target in
  let user = User.name user in
  state_repo_url t "commits/%s/%s/%s/commit/%s" metadata_commit user repo src_commit

let commit_url ~repo commit =
  let { Repo.user; repo } = repo in
  let user = User.name user in
  Printf.sprintf "https://github.com/%s/%s/commit/%s" user repo commit

let short_commit x =
  String.with_range ~len:8 x

let encode = Uri.pct_encode ~scheme:"http"

let saved_log_frame_link ~branch ~commit = Printf.sprintf "/log/saved/%s/%s" (encode branch) (encode commit)

let logs_link = function
  | `Live live_log -> Printf.sprintf "/log/live/%s" (encode (CI_live_log.branch live_log))
  | `Saved {CI_output.branch; commit; _} -> saved_log_frame_link ~branch ~commit

let tag_map f map =
  Ref.Index.fold (fun key value acc ->
      match snd key with
      | "tags" :: _ -> f key value @ acc
      | _ -> acc
    ) map []

let branch_map f map =
  Ref.Index.fold (fun key value acc ->
      match snd key with
      | "heads" :: _ -> f key value @ acc
      | _ -> acc
    ) map []

let pr_map f map =
  PR.Index.fold (fun key value acc ->
      f key value @ acc
    ) map []

let dash_map f map targets =
  Ref.Index.fold (fun key value acc ->
      match CI_target.Set.mem (`Ref key) targets with
      | true -> [f key value] @ acc
      | false -> acc
    ) map []

let status state =
  let colour, icon, status =
    match state with
    | `Success -> "label-success", "glyphicon-ok","Success"
    | `Pending -> "label-warning", "glyphicon-hourglass", "Pending"
    | `Failure -> "label-danger", "glyphicon-remove", "Failure"
  in
  span ~a:[a_class ["label"; colour;]] [span ~a:[a_class ["glyphicon"; icon]] []; txt status]

let status_flag ?label stat =
  let cl, icon, status =
    match stat with
    | `Success -> "label-success", "glyphicon-ok","success"
    | `Pending -> "label-warning", "glyphicon-hourglass", "pending"
    | `Failure -> "label-danger", "glyphicon-remove", "failure"
  in
  let tooltip =
    match label with
    | None -> status
    | Some label -> Printf.sprintf "%s : %s" label status
  in
  span ~a:[a_class ["status"; "glyphicon"; icon; cl]; a_title tooltip] []

let status_list jobs =
  table ~a:[a_class ["ci-status-list"]] [
    tr (
      jobs |> List.map (fun job ->
          let state = job_state job in
          let label = CI_engine.job_name job in
          td [status_flag ~label (CI_output.status state)];
        )
    )
  ]

let summarise jobs =
  let outputs = List.map (fun j -> CI_engine.job_name j, job_state j) jobs in
  let combine status outputs =
    let results = ref String.Map.empty in
    outputs |> List.iter (fun (name, output) ->
        let descr = CI_output.descr output in
        let old_names = String.Map.find descr !results |> CI_utils.default [] in
        results := String.Map.add descr (name :: old_names) !results
      );
    let results = String.Map.bindings !results in
    let pp_group f (descr, g) = Fmt.pf f "%s (%a)" descr (Fmt.(list ~sep:(const string ", ") Fmt.string)) (List.rev g) in
    let descr = Fmt.strf "%a" Fmt.(list ~sep:(const string "; ") pp_group) results in
    CI_result.v status descr
  in
  let pending, outputs = List.partition (fun (_, s) -> CI_output.status s = `Pending) outputs in
  if pending <> [] then combine `Pending pending
  else (
    let failed, outputs = List.partition (fun (_, s) -> CI_output.status s = `Failure) outputs in
    if failed <> [] then combine `Failure failed
    else combine `Success outputs
  )

let dashboard_widget (_repo, id) ref =
  let state = CI_engine.jobs ref |> summarise in
  let cls, icon, status, comment =
    match CI_result.status state with
    | `Success -> "dashboard-success", "glyphicon-ok", "Succeeding", "YAY! The build is fine... Nothing to see here..."
    | `Pending -> "dashboard-pending", "glyphicon-hourglass", "Pending", "... WAITING ..."
    | `Failure -> "dashboard-failure", "glyphicon-remove", "Failing", "SOUND THE ALARM!!! The build has been broken!"
  in
  let title = match id with
    | ("heads" | "tags") :: tl -> tl
    | x -> x
  in
  div ~a:[a_class ["col-md-4"; "text-center"; "dashboard"; cls]] [
    h2 ~a:[a_id "title"] [ txt (String.concat ~sep:"/" title) ];
    h2 ~a:[a_id "icon"] [ span ~a:[a_class["glyphicon"; icon]] [] ];
    h2 ~a:[a_id "status"] [ txt status ];
    small [ txt comment ];
  ]

let ref_job (_repo, id) ref =
  match CI_engine.jobs ref with
  | [] -> []
  | jobs ->
    let summary = summarise jobs in
    let ref_url = Uri.to_string (CI_target.path_v (CI_engine.target ref)) in
    [
      tr [
        td [a ~a:[a_href ref_url] [txt (Fmt.to_to_string Ref.pp_name id)]];
        td [status_list jobs];
        td [txt (CI_result.descr summary)];
      ]
    ]

let pr_job (_repo, id) open_pr =
  match CI_engine.jobs open_pr with
  | [] -> []
  | jobs ->
    let summary = summarise jobs in
    let pr_url = Uri.to_string (CI_target.path_v (CI_engine.target open_pr)) in
    [
      tr [
        td [a ~a:[a_href pr_url] [txt (string_of_int id)]];
        td [txt (CI_engine.title open_pr)];
        td [status_list jobs];
        td [txt (CI_result.descr summary)];
      ]
    ]

let heading x = th [txt x]

let url x =
  a ~a:[a_href x] [txt x]

module Nav = struct
  type t =
    | Home
    | PRs
    | Branches
    | Tags
    | Settings

  let to_string = function
    | Home -> "Home"
    | PRs -> "PRs"
    | Branches -> "Branches"
    | Tags -> "Tags"
    | Settings -> "Settings"
end

let build_navbar active =
  let item name href =
    let cl = if name = active then ["active"] else [] in
    li ~a:[a_class cl] [a ~a:[a_href href] [txt (Nav.to_string name)]];
  in
  Nav.[
    item Home "/";
    item PRs "/pr";
    item Branches "/branch";
    item Tags "/tag";
    item Settings "/settings";
  ]

let page page_title active children t ~user =
  let navbar = build_navbar active in
  let user = user |> CI_utils.default "not logged in" in
  let nav_header =
    nav ~a:[a_class["navbar";"navbar-inverse";"navbar-fixed-top"]] [
      div ~a:[a_class["container"]] [
        div ~a:[a_class["navbar-header"]] [
          button ~a:[a_user_data "toggle" "collapse"; a_user_data "target" "#navbar"; a_class ["navbar-toggle"]; a_button_type `Button;] [

            span ~a:[a_class["sr-only"]] [txt "Toggle Navigation"];
            span ~a:[a_class["icon-bar"]] [];
            span ~a:[a_class["icon-bar"]] [];
            span ~a:[a_class["icon-bar"]] [];

          ];
          a ~a:[a_class["navbar-brand"]; a_href "/"] [txt t.name];
        ];
        div ~a:[a_id "navbar"; a_class["collapse"; "navbar-collapse"]] [
          ul ~a:[a_class ["nav"; "navbar-nav"]] navbar;
          ul ~a:[a_class ["nav"; "navbar-nav"; "navbar-right"]] [
            li [a ~a:[a_href "/user/profile"] [txt user]]
          ];
        ];
      ];
    ]
  in
  let content =
    [
      div ~a:[a_class ["container"]] [
        div ~a:[a_class ["content"]] children;
      ];
      script ~a:[a_mime_type "text/javascript"; a_src "/js/ci.js"] (txt "");
      script ~a:[a_mime_type "text/javascript";
                 a_src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"] (txt "");
      script ~a:[a_mime_type "text/javascript"; a_src "/js/bootstrap.min.js"] (txt "");
    ]
  in
  let body = body (nav_header :: content) in
  html
    (head (title (txt page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        meta ~a:[a_http_equiv "X-UA-Compatible"; a_content "IE=edge"] ();
        meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1"] ();
        link ~rel:[`Icon] ~a:[a_mime_type "image/png"] ~href:"/images/favicon.png" ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
      ])
    body

let opt_warning ci =
  let dk = CI_engine.dk ci in
  match Lwt.state dk with
  | Lwt.Return _ -> []
  | Lwt.Sleep -> [div ~a:[a_class ["warning"]] [txt "Connecting to DataKit..."]]
  | Lwt.Fail ex ->
    let msg = Fmt.strf "DataKit connection is down: %s" (Printexc.to_string ex) in
    [div ~a:[a_class ["warning"]] [txt msg]]

let pr_table (id, prs) =
  div [
    h2 [txt (Fmt.strf "PR status for %a" Repo.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "PR"; heading "Title"; heading "State"; heading "Details"] ::
      (pr_map pr_job prs)
    );
  ]

let branch_table (id, refs) =
  div [
    h2 [txt (Fmt.strf "Branches for %a" Repo.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "Ref"; heading "State"; heading "Details"] ::
      (branch_map ref_job refs)
    );
  ]

let tag_table (id, refs) =
  div [
    h2 [txt (Fmt.strf "Tags for %a" Repo.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "Ref"; heading "State"; heading "Details"] ::
      (tag_map ref_job refs)
    );
  ]

let rec make_pairs pairs = function
  | [] -> List.rev pairs
  | [x] -> List.rev ((Some x, None)::pairs)
  | x::y::rest -> make_pairs ((Some x, Some y)::pairs) rest

let dashboard_row acc x =
  let (l,r) = x in
  let col1 = match l with
    | None -> div ~a:[a_class["col-md-4"]] [];
    | Some l -> l;
  in
  let col2 = match r with
    | None -> div ~a:[a_class["col-md-4"]] [];
    | Some r -> r;
  in
  div ~a:[a_class["row"]]
    [
      div ~a:[a_class["col-md-1"]] [];
      col1;
      div ~a:[a_class["col-md-2"]] [];
      col2;
      div ~a:[a_class["col-md-1"]] [];
    ]
  :: acc

let dashboard_table _id (refs, targets) acc =
  let widgets = dash_map dashboard_widget refs targets in
  let widget_pairs = make_pairs [] widgets in
  List.fold_left (fun acc x -> dashboard_row acc x) [] widget_pairs @ acc

let pp_opt_label f = function
  | None -> ()
  | Some label -> Fmt.pf f ": %s" label

let html_of_user ~csrf_token ((job, label), log) =
  let cancel_attrs, branch =
    match log with
    | Some log when CI_live_log.can_cancel log -> [], CI_live_log.branch log
    | _ -> [a_disabled ()], ""
  in
  let log_link =
    match log with
    | Some log -> [txt "[ "; a ~a:[a_href (logs_link (`Live log))] [txt "log"]; txt " ] "]
    | None -> []
  in
  let branch = Uri.pct_encode ~scheme:"http" branch in
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/cancel/%s?%s" branch (Uri.encoded_of_query query) in
  let target, job_name = job in
  let link = Uri.to_string (CI_target.path target) in
  let reason = Fmt.strf "%a:%s%a" CI_target.pp target job_name pp_opt_label label in
  [
    br ();
    form ~a:[a_class ["cancel"]; a_action action; a_method `Post] [
      button ~a:(a_class ["btn"; "btn-default"] :: a_button_type `Submit :: cancel_attrs) [
        span ~a:[a_class ["glyphicon"; "glyphicon-remove"]] []; txt "Cancel"
      ];
    ];
  ] @ log_link @ [
    a ~a:[a_href link] [txt reason];
  ]

let resource_pools ~csrf_token =
  let items =
    String.Map.bindings (CI_monitored_pool.pools ()) |> List.map (fun (name, pool) ->
        let active = CI_monitored_pool.active pool in
        let capacity = CI_monitored_pool.capacity pool in
        let qlen = CI_monitored_pool.qlen pool in
        let used =
          if active < capacity then Fmt.strf "%d / %d" active capacity
          else Fmt.strf "%d / %d [%d queued]" active capacity qlen
        in
        let uses = CI_monitored_pool.users pool |> List.map (html_of_user ~csrf_token) |> List.concat in
        tr [th [txt name]; td (txt used :: uses)];
      )
  in
  table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
    tr [th [txt "Name"]; th [txt "Utilisation"]] ::
    items
  )

let login_page ?github ~csrf_token state ~is_configured t ~user =
  let field = CI_form.Html.field state in
  let action = "/auth/login" in
  let github_login =
    match github with
    | None ->
      p [txt "(configure webauth to allow GitHub logins)"]
    | Some github ->
      p [
        a ~a:[a_href (Uri.to_string github)] [txt "Log in with GitHub"];
      ]
  in
  let warnings =
    if is_configured then []
    else [
      div ~a:[a_class ["alert"; "alert-warning"]] [
        txt
          "The CI has not yet been configured with an administrator user. \
           In the CI's logs you should find a URL containing a token - open this \
           URL to create an 'admin' user."
      ]
    ]
  in
  page "Login" Nav.Home ~user ([
    h2 [txt "Login"];
    CI_form.Html.form ~csrf_token state ~form_class:["login-form"] ~action [
      field "Username" `Text "user";
      field "Password" `Password "password";
      div [button ~a:[a_class ["btn"; "btn-primary"]; a_button_type `Submit] [txt "Log in"]];
    ];
    github_login;
  ] @ warnings) t

let auth_setup ~csrf_token state =
  let field = CI_form.Html.field state in
  let action = "/auth/setup" in
  page "Auth Setup" Nav.Home [
    CI_form.Html.form state ~csrf_token ~form_class:["auth-setup-form"] ~action [
      div ~a:[a_class ["form-group"]] [
        label ~a:[a_label_for "user"] [txt "Username"];
        input ~a:[a_class ["form-control"]; a_id "user"; a_input_type `Text;
                  a_disabled (); a_name "name"; a_value "admin"] ()
      ];
      field "Password" `Password "password";
      field "Confirm" `Password "password2";
      button ~a:[a_class ["btn"; "btn-default"]; a_button_type `Submit] [
        txt "Submit"
      ]
    ]
  ]

let user_page ~csrf_token =
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/auth/logout?%s" (Uri.encoded_of_query query) in
  page "Profile" Nav.Home [
    form ~a:[a_class ["logout-form"]; a_action action; a_method `Post] [
      button ~a:[a_class ["btn"; "btn-default"]; a_button_type `Submit] [
        txt "Log out"
      ]
    ]
  ]

let main_page ~csrf_token ~ci ~dashboards =
  let prs = CI_engine.prs ci in
  let refs = CI_engine.refs ci in
  let title_of_project (id, _) = Fmt.to_to_string Repo.pp id in
  let projects =
    (Repo.Map.bindings prs |> List.map title_of_project)
    @ (Repo.Map.bindings refs |> List.map title_of_project)
  in
  let title = "CI: " ^ (projects |> String.concat ~sep:" / ") in
  let dashboard_widgets =
    let combined =
      Repo.Map.merge (fun _ project_state dash_config ->
          match project_state, dash_config with
          | Some refs, Some y -> Some (refs, y)
          | _ -> None
        ) refs dashboards
    in
    Repo.Map.fold dashboard_table combined []
  in
  page title Nav.Home @@ opt_warning ci @ dashboard_widgets @ [
      h2 [txt "Resource pools"];
      resource_pools ~csrf_token;
    ]

let prs_page ~ci =
  let prs = CI_engine.prs ci |> Repo.Map.bindings in
  let sections = List.map pr_table prs in
  let title_of_project (id, _) = Fmt.to_to_string Repo.pp id in
  let title = "CI: " ^ (List.map title_of_project prs |> String.concat ~sep:" / ") in
  page title Nav.PRs @@ opt_warning ci @ sections

let branches_page ~ci =
  let refs = CI_engine.refs ci |> Repo.Map.bindings in
  let sections = List.map branch_table refs in
  let title_of_project (id, _) = Fmt.to_to_string Repo.pp id in
  let title = "CI: " ^ (List.map title_of_project refs |> String.concat ~sep:" / ") in
  page title Nav.Branches @@ opt_warning ci @ sections

let tags_page ~ci =
  let refs = CI_engine.refs ci |> Repo.Map.bindings in
  let sections = List.map tag_table refs in
  let title_of_project (id, _) = Fmt.to_to_string Repo.pp id in
  let title = "CI: " ^ (List.map title_of_project refs |> String.concat ~sep:" / ") in
  page title Nav.Tags @@ opt_warning ci @ sections

let history_button url =
  a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href url;] [span ~a:[a_class["glyphicon"; "glyphicon-time"]] []; txt "History"]

let log_button_group history log_url =
  div ~a:[a_class["btn-group";"pull-right"]] [
    history;
    a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href log_url;] [span ~a:[a_class["glyphicon"; "glyphicon-book"]] []; txt "Artefacts"];
  ]

let logs ~csrf_token ~page_url state =
  let open CI_output in
  let logs = CI_output.logs state in
  let last_title = ref None in
  let seen = ref String.Set.empty in
  let log_link ~title log =
    let cl = ["log-link"] in
    let href = logs_link log in
    span [
      txt "[ ";
      a ~a:[a_href href; a_target "iframe_log"; a_class cl] [txt "logs"];
      txt " ] ";
      txt title;
    ]
  in
  let rec aux = function
    | Empty -> []
    | Live log when String.Set.mem (CI_live_log.branch log) !seen -> []
    | Saved { branch; _ } when String.Set.mem branch !seen -> []
    | Live live_log ->
      let branch = CI_live_log.branch live_log in
      seen := String.Set.add branch !seen;
      let title = CI_live_log.title live_log in
      last_title := Some title;
      [
        form [
          status_flag `Pending;
          button ~a:[a_class ["btn"; "btn-default"; "btn-xs"; "rebuild"]; a_button_type `Submit; a_disabled ()] [
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; txt "Rebuild"];
          log_link ~title (`Live live_log);
        ];
      ]
    | Saved ({commit = _; branch; title; rebuild; failed} as saved) ->
      seen := String.Set.add branch !seen;
      let query = [
        "CSRFToken", [csrf_token];
        "redirect", [Uri.to_string page_url];
      ] in
      let action =
        let path = "/log/rebuild/" ^ Uri.pct_encode ~scheme:"http" branch in
        Uri.make ~path ~query () |> Uri.to_string
      in
      let status = if failed then `Failure else `Success in
      let rebuild_button =
        match rebuild with
        | `Rebuildable _ ->
          button ~a:[a_class ["btn"; "btn-default"; "btn-xs"; "rebuild"]; a_button_type `Submit] [
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; txt "Rebuild"];
        | `Rebuilding -> txt "(rebuild queued) "
        | `Archived -> txt "(archived) "
      in
      last_title := Some title;
      [
        form ~a:[a_action action; a_method `Post] [
          status_flag status;
          rebuild_button;
          log_link ~title (`Saved saved);
        ]
      ]
    | Pair (a, b) ->
      let a = aux a in  (* Order matters, for [last_title] *)
      let b = aux b in
      a @ b
  in
  let items = aux logs in
  (* Don't show the overall status if it's the same as the last log title. *)
  match !last_title with
  | Some shown when shown = CI_output.descr state -> items
  | _ ->
    let status = CI_output.status state in
    let descr = CI_output.descr state in
    items @ [p [status_flag status; txt descr]]

let job_row ?selected ~csrf_token ~page_url (job_name, state) =
  let output =
    match state with
    | None -> (Error (`Pending "(new)"), CI_output.Empty)
    | Some state -> state
  in
  let attrs =
    if selected = Some job_name then [a_class ["selected-job"]]
    else []
  in
  tr ~a:attrs [
    th [txt job_name];
    td [status (CI_output.status output)];
    td (
      logs ~csrf_token ~page_url output
    );
  ]

let target_title ~title = function
  | `PR (_, pr) -> Printf.sprintf "PR %d (%s)" pr title
  | `Ref (_, r) -> Fmt.strf "Ref %a" Ref.pp_name r

let map_or_none f = function
  | [] -> [li [txt "(none)"]]
  | xs -> List.map f xs

let commit_page ?test ~commit ~archived_targets targets t =
  let title = Fmt.strf "Commit %s" commit in
  let target_link target =
    let uri = Uri.to_string (CI_target.path ?test target) in
    li [
      a ~a:[a_href uri] [
        txt (Fmt.to_to_string CI_target.pp target)
      ]
    ]
  in
  let archive_target_link (target, commit) =
    let commit = CI_utils.DK.Commit.id commit in
    let uri = Uri.to_string (Uri.add_query_param' (CI_target.path ?test target) ("history", commit)) in
    li [
      a ~a:[a_href uri] [
        txt (Fmt.to_to_string CI_target.pp target)
      ]
    ]
  in
  page title Nav.Home [
    h2 [txt (Fmt.strf "Commit %s" commit)];
    p [txt "Current builds:"];
    ul (map_or_none target_link targets);
    p [txt "Archived builds:"];
    ul (map_or_none archive_target_link archived_targets);
  ] t

let target_page_url = CI_target.path

let state_link commit =
  let url = Fmt.strf "?history=%s" commit in
  a ~a:[a_href url] [txt (short_commit commit)]

let rec intersperse sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse sep xs

let history_nav t target state =
  let history_link = status_history_url t target in
  let links = [
      txt " [ "; a ~a:[a_href "?"] [txt "latest"]; txt " | ";
      a ~a:[a_href history_link] [txt "history"]; txt " ]";
    ]
  in
  match CI_history.State.parents state with
  | [] -> p (txt "No previous states" :: links)
  | [x] -> p (txt "Previous state: " :: state_link x :: links)
  | xs -> p (txt "Previous states: " :: (intersperse (txt ", ") (List.map state_link xs)) @ links)

let target_page ?test ~csrf_token ?(title="(no title)") ~(target:CI_target.t) state t =
  let jobs = CI_history.State.jobs state |> String.Map.bindings |> List.map (fun (name, s) -> name, Some s) in
  let title = target_title ~title target in
  let repo = CI_target.repo target in
  let src_commit = CI_history.State.source_commit state |> CI_utils.default "MISSING-COMMIT" in
  let metadata_commit = CI_history.State.metadata_commit state |> CI_utils.default "MISSING-COMMIT" in
  let page_url = target_page_url target in
  let state_summary = [
    table ~a:[a_class ["table"; "table-bordered"; "results"]] (List.map (job_row ?selected:test ~csrf_token ~page_url) jobs)
  ] in
  let nav =
    match target with
    | `PR _ -> Nav.PRs
    | `Ref (_, r) ->
      match r with
      | "heads" :: _ -> Nav.Branches
      | "tags" :: _ -> Nav.Tags
      | _ -> assert false
  in
  page title nav (
    history_nav t target state
    :: p [
      a ~a:[a_href (gh_target_url target)] [txt title];
      txt " has head commit ";
      a ~a:[a_href (commit_url ~repo src_commit)] [txt (short_commit src_commit)];
      txt " [ ";
      a ~a:[a_href (metadata_url t target)] [txt "head history"];
      txt " ]";
      txt " [ ";
      a ~a:[a_href (commit_history_url t target ~metadata_commit ~src_commit)] [txt "status history for this head"];
      txt " ]";
    ]
    :: state_summary
  ) t

let plain_page ~page_title =
  html
    (head (title (txt page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
      ])

let iframe_page ~page_title =
  html
    (head (title (txt page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
        base ~a:[a_target "_parent"] ();
        script (txt "window.top.highlight_log()");
      ])

let plain_error msg _t ~user:_ =
  plain_page ~page_title:"Error" (body [txt msg])

let live_log_frame ~branch ~have_history t ~user:_ =
  let buttons =
    if have_history then [history_button (log_branch_history_url t branch)]
    else []
  in
  let page_title = Fmt.strf "Logging to branch %s" branch in
  iframe_page ~page_title
    (body ~a:[a_class ["log"]] [
        div ~a:[a_class ["row"]] [
          div ~a:[a_class ["col-md-9"]] [
            p [txt "Still running..."];
          ];
          div ~a:[a_class ["col-md-3"]] [
            div ~a:[a_class["btn-group";"pull-right"]] buttons
          ]
        ];
        pre [txt "@STREAM-GOES-HERE@"];
        p [txt "This log is now complete."];
      ]
    )

let saved_log_frame ~commit ~branch t ~user:_ =
  let page_title = Fmt.strf "Log from commit %s" commit in
  let history = history_button (log_branch_history_url t branch) in
  let log_url = log_branch_results_url t branch in
  let commit_url = log_commit_url t commit in
  iframe_page ~page_title
    (body ~a:[a_class ["log"]] [
        div ~a:[a_class ["row"]] [
          div ~a:[a_class ["col-md-9"]] [
            p [txt "Loaded from commit "; a ~a:[a_href commit_url] [txt commit]]
          ];
          div ~a:[a_class ["col-md-3"]] [
            log_button_group history log_url;
          ];
        ];
        pre [txt "@STREAM-GOES-HERE@"];
      ]
    )

let error_page id =
  page "Error" Nav.Home (
    if id = Error.no_state_repo then
      [
        p [txt "No web mirror of the state repository has been configured, so can't link to it."];
        p [txt "Configure DataKit to push to a GitHub repository and then pass the repository's URL using ";
           code [txt "Web.config ~state_repo"];
           txt "."
          ];
      ]
    else if id = Error.permission_denied then
      [
        p [txt "Permission denied"];
      ]
    else if id = Error.logout_needed then
      [
        p [txt
             "Access policy has changed - please log out and log back in so we can \
              check your credentials against the new policy."];
      ]
    else
      [
        p [txt (Printf.sprintf "Unknown error code %S" id)]
      ]
  )

module Settings = struct
  let index =
    page "Settings" Nav.Settings [
      ul [
        li [
          a ~a:[a_href "/settings/github-auth"] [txt "Configure GitHub authentication"]
        ]
      ]
    ]

  let github_auth ~csrf_token state =
    let action = "/settings/github-auth" in
    let field = CI_form.Html.field state in
    page "GitHub authentication" Nav.Settings [
      div ~a:[a_class ["github-auth"]] [
        h2 [txt "GitHub authentication"];
        p [txt "You can configure the CI to allow users to authenticate using their GitHub accounts. To do this:"];
        ol [
          li [
            p [txt "Go to "; url "https://github.com/settings/applications/new"; txt " and register your CI. "];
            p [txt "Give the callback URL as <https://HOST:PORT/auth/github-callback> \
                       (you can use 'localhost' for testing)."];
          ];
          li [
            p [txt "Enter the Client ID and Client Secret below."];
            p [txt
                 "You can also specify a callback here, if you want to override the one you registered with GitHub above. \
                  This might be useful if you have several CI instances using a single Client ID.";
              ]
          ]
        ];
        CI_form.Html.form ~csrf_token state ~form_class:["github-auth"] ~action [
          field "Client ID" `Text "client-id";
          field "Client Secret" `Password "client-secret";
          field "Callback (optional)" `Url "callback";
          div [button ~a:[a_class ["btn"; "btn-primary"]; a_button_type `Submit] [txt "Submit"]];
        ];
      ]
    ]
end
