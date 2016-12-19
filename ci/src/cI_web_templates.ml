open Datakit_github
open! Astring
open! Tyxml.Html
open CI_s

type t = {
  name : string;
  state_repo : Uri.t option;
  metrics_token : [`SHA256 of Cstruct.t] option;
  can_read : CI_ACL.t;
  can_build : CI_ACL.t;
}

type page = user:string option -> [`Html] Tyxml.Html.elt

module Error = struct
  type t = string
  let no_state_repo = "no-state-repo"
  let permission_denied = "permission-denied"
  let logout_needed = "logout-needed"

  let uri_path id = "/error/" ^ id
  let uri id = Uri.of_string (uri_path id)
end

let config ?(name="datakit-ci") ?state_repo ?metrics_token ~can_read ~can_build () =
  let metrics_token =
    match metrics_token with
    | None -> None
    | Some (`SHA256 str) -> Some (`SHA256 (Cstruct.of_string str))
  in
  { name; state_repo; metrics_token; can_read; can_build }

let state_repo_url t fmt =
  fmt |> Fmt.kstrf @@ fun path ->
  match t.state_repo with
  | Some base -> Fmt.strf "%a/%s" Uri.pp_hum base path
  | None -> Error.(uri_path no_state_repo)

let log_commit_url t commit =
  state_repo_url t "commit/%s" commit

let log_branch_history_url t branch =
  state_repo_url t "commits/%s/log" branch

let log_branch_results_url t branch =
  state_repo_url t "tree/%s" branch

let gh_target_url = function
  | `PR pr ->
    let { Repo.user; repo } = PR.repo pr in
    let id = PR.number pr in
    Printf.sprintf "https://github.com/%s/%s/pull/%d" user repo id
  | `Ref r ->
    let { Repo.user; repo } = Ref.repo r in
    let id = Ref.name r in
    match id with
    | "tags" :: id -> Fmt.strf "https://github.com/%s/%s/releases/tag/%s" user repo (String.concat ~sep:"/" id)
    | _            -> Fmt.strf "https://github.com/%s/%s/tree/%a" user repo Ref.pp_name id

let metadata_url t = function
  | `PR pr ->
    let { Repo.user; repo } = PR.repo pr in
    let id = PR.number pr in
    state_repo_url t "commits/github-metadata/%s/%s/pr/%d" user repo id
  | `Ref r ->
    let { Repo.user; repo } = Ref.repo r in
    let id = Ref.name r in
    state_repo_url t "commits/github-metadata/%s/%s/ref/%a" user repo
      Ref.pp_name id

let commit_history_url t target =
  let { Repo.user; repo }, commit =
    match target with
    | `PR pr -> PR.repo pr, PR.commit pr
    | `Ref r -> Ref.repo r, Ref.commit r
  in
  let hash = Commit.hash commit in
  state_repo_url t "commits/github-metadata/%s/%s/commit/%s" user repo hash

let commit_url ~repo commit =
  let { Repo.user; repo } = repo in
  Printf.sprintf "https://github.com/%s/%s/commit/%s" user repo commit

let tag_map f map =
  Ref.Index.fold (fun key value acc ->
      match snd key with
      | "tags" :: _ -> [f key value] @ acc
      | _ -> acc
    ) map []

let branch_map f map =
  Ref.Index.fold (fun key value acc ->
      match snd key with
      | "heads" :: _ -> [f key value] @ acc
      | _ -> acc
    ) map []

let pr_map f map =
  PR.Index.fold (fun key value acc ->
      [f key value] @ acc
    ) map []

let dash_map f map targets =
  Ref.Index.fold (fun key value acc ->
      match CI_target.Set.mem (`Ref key) targets with
      | true -> [f key value] @ acc
      | false -> acc
    ) map []

let status state =
  let colour, icon, status =
    match state.status with
    | `Pending -> "label-warning", "glyphicon-hourglass", "Pending"
    | `Success -> "label-success", "glyphicon-ok","Success"
    | `Error -> "label-danger", "glyphicon-warning-sign", "Error"
    | `Failure -> "label-danger", "glyphicon-remove", "Failure"
  in
  span ~a:[a_class ["label"; colour;]] [span ~a:[a_class ["glyphicon"; icon]] []; pcdata status]

let status_flag ?label status =
  let cl, icon, status =
    match status with
    | `Pending -> "label-warning", "glyphicon-hourglass", "pending"
    | `Success -> "label-success", "glyphicon-ok","success"
    | `Error -> "label-danger", "glyphicon-warning-sign", "error"
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
          let state = CI_engine.state job in
          let label = CI_engine.job_name job in
          td [status_flag ~label state.status];
        )
    )
  ]

let summarise jobs =
  let states = List.map (fun j -> CI_engine.job_name j, CI_engine.state j) jobs in
  let combine status states =
    let results = ref String.Map.empty in
    states |> List.iter (fun (name, state) ->
        let descr = state.descr in
        let old_names = String.Map.find descr !results |> CI_utils.default [] in
        results := String.Map.add descr (name :: old_names) !results
      );
    let results = String.Map.bindings !results in
    let pp_group f (descr, g) = Fmt.pf f "%s (%a)" descr (Fmt.(list ~sep:(const string ", ") Fmt.string)) (List.rev g) in
    let descr = Fmt.strf "%a" Fmt.(list ~sep:(const string "; ") pp_group) results in
    { status; descr; logs = CI_output.Empty }
  in
  let pending, states = List.partition (fun (_, x) -> x.status = `Pending) states in
  if pending <> [] then combine `Pending pending
  else (
    let failed, states = List.partition (fun (_, x) -> x.status = `Failure) states in
    if failed <> [] then combine `Failure failed
    else combine `Success states
  )

let dashboard_widget (_repo, id) ref =
  let state = CI_engine.jobs ref |> summarise in
  let cls, icon, status, comment =
    match state.status with
    | `Pending -> "dashboard-pending", "glyphicon-hourglass", "Pending", "... WAITING ..."
    | `Success -> "dashboard-success", "glyphicon-ok", "Succeeding", "YAY! The build is fine... Nothing to see here..."
    | `Error -> "dashboard-error", "glyphicon-warning-sign", "Erroring", "OH NO! Something has gone terribly wrong"
    | `Failure -> "dashboard-failure", "glyphicon-remove", "Failing", "SOUND THE ALARM!!! The build has been broken!"
  in
  let title = match id with
    | ("heads" | "tags") :: tl -> tl
    | x -> x
  in
  div ~a:[a_class ["col-md-4"; "text-center"; "dashboard"; cls]] [
    h2 ~a:[a_id "title"] [ pcdata (String.concat ~sep:"/" title) ];
    h2 ~a:[a_id "icon"] [ span ~a:[a_class["glyphicon"; icon]] [] ];
    h2 ~a:[a_id "status"] [ pcdata status ];
    small [ pcdata comment ];
  ]

let ref_job (_repo, id) ref =
  let jobs = CI_engine.jobs ref in
  let summary = summarise jobs in
  let ref_url = CI_target.path_v (CI_engine.target ref) in
  tr [
    td [a ~a:[a_href ref_url] [pcdata (Fmt.to_to_string Ref.pp_name id)]];
    td [status_list jobs];
    td [pcdata summary.descr];
  ]

let pr_job (_repo, id) open_pr =
  let jobs = CI_engine.jobs open_pr in
  let summary = summarise jobs in
  let pr_url = CI_target.path_v (CI_engine.target open_pr) in
  tr [
    td [a ~a:[a_href pr_url] [pcdata (string_of_int id)]];
    td [pcdata (CI_engine.title open_pr)];
    td [status_list jobs];
    td [pcdata summary.descr];
  ]

let heading x = th [pcdata x]


module Nav = struct
  type t =
    | Home
    | PRs
    | Branches
    | Tags

  let to_string = function
    | Home -> "Home"
    | PRs -> "PRs"
    | Branches -> "Branches"
    | Tags -> "Tags"
end

let build_navbar active =
  let item name href =
    let cl = if name = active then ["active"] else [] in
    li ~a:[a_class cl] [a ~a:[a_href href] [pcdata (Nav.to_string name)]];
  in
  Nav.[
    item Home "/";
    item PRs "/pr";
    item Branches "/branch";
    item Tags "/tag";
  ]

let page ?logs page_title active children t ~user =
  let navbar = build_navbar active in
  let user = user |> CI_utils.default "not logged in" in
  let nav_header =
    nav ~a:[a_class["navbar";"navbar-inverse";"navbar-fixed-top"]] [
      div ~a:[a_class["container"]] [
        div ~a:[a_class["navbar-header"]] [
          button ~a:[a_user_data "toggle" "collapse"; a_user_data "target" "#navbar"; a_class ["navbar-toggle"]; a_button_type `Button;] [

            span ~a:[a_class["sr-only"]] [pcdata "Toggle Navigation"];
            span ~a:[a_class["icon-bar"]] [];
            span ~a:[a_class["icon-bar"]] [];
            span ~a:[a_class["icon-bar"]] [];

          ];
          a ~a:[a_class["navbar-brand"]; a_href "/"] [pcdata t.name];
        ];
        div ~a:[a_id "navbar"; a_class["collapse"; "navbar-collapse"]] [
          ul ~a:[a_class ["nav"; "navbar-nav"]] navbar;
          ul ~a:[a_class ["nav"; "navbar-nav"; "navbar-right"]] [
            li [a ~a:[a_href "/user/profile"] [pcdata user]]
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
    ]
  in
  let body =
    match logs with
    | Some init ->
      let attrs =
        match init with
        | None -> []
        | Some init -> [a_src init]
      in
      body ~a:[a_class ["split-page"]] [
        nav_header;
        div ~a:[a_class ["upper"]] content;
        iframe ~a:(a_id "iframe_log" :: a_class ["log"] :: a_onload "highlight_log()" :: a_name "iframe_log" :: attrs) [];
      ]
    | None ->
      body (nav_header :: content)
  in
  html
    (head (title (pcdata page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        meta ~a:[a_http_equiv "X-UA-Compatible"; a_content "IE=edge"] ();
        meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1"] ();
        link ~rel:[`Icon] ~a:[a_mime_type "image/png"] ~href:"/images/favicon.png" ();
        script ~a:[a_mime_type "text/javascript"; a_src "/js/ci.js"] (pcdata "");
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
      ])
    body

let opt_warning ci =
  let dk = CI_engine.dk ci in
  match Lwt.state dk with
  | Lwt.Return _ -> []
  | Lwt.Sleep -> [div ~a:[a_class ["warning"]] [pcdata "Connecting to DataKit..."]]
  | Lwt.Fail ex ->
    let msg = Fmt.strf "DataKit connection is down: %s" (Printexc.to_string ex) in
    [div ~a:[a_class ["warning"]] [pcdata msg]]

let pr_table (id, prs) =
  div [
    h2 [pcdata (Fmt.strf "PR status for %a" Repo.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "PR"; heading "Title"; heading "State"; heading "Details"] ::
      (pr_map pr_job prs)
    );
  ]

let branch_table (id, refs) =
  div [
    h2 [pcdata (Fmt.strf "Branches for %a" Repo.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "Ref"; heading "State"; heading "Details"] ::
      (branch_map ref_job refs)
    );
  ]

let tag_table (id, refs) =
  div [
    h2 [pcdata (Fmt.strf "Tags for %a" Repo.pp id)];
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
  let branch = Uri.pct_encode ~scheme:"http" branch in
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/cancel/%s?%s" branch (Uri.encoded_of_query query) in
  let target, job_name = job in
  let link = CI_target.path target in
  let reason = Fmt.strf "%a:%s%a" CI_target.pp target job_name pp_opt_label label in
  [
    br ();
    form ~a:[a_class ["cancel"]; a_action action; a_method `Post] [
      button ~a:(a_class ["btn"; "btn-default"] :: a_button_type `Submit :: cancel_attrs) [
        span ~a:[a_class ["glyphicon"; "glyphicon-remove"]] []; pcdata "Cancel"
      ];
    ];
    a ~a:[a_href link] [pcdata reason];
  ]

let resource_pools ~csrf_token =
  let items =
    let open CI_monitored_pool in
    String.Map.bindings (pools ()) |> List.map (fun (name, pool) ->
        let used = Fmt.strf "%d / %d" (active pool) (capacity pool) in
        let uses = users pool |> List.map (html_of_user ~csrf_token) |> List.concat in
        tr [th [pcdata name]; td (pcdata used :: uses)];
      )
  in
  table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
    tr [th [pcdata "Name"]; th [pcdata "Utilisation"]] ::
    items
  )

let field descr ty name =
  let id = "field-" ^ name in
  div ~a:[a_class ["form-group"]] [
    label ~a:[a_label_for id] [pcdata descr];
    input ~a:[a_class ["form-control"]; a_id id; a_input_type ty; a_name name] ()
  ]

let login_page ?github ~csrf_token ~is_configured t ~user =
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/auth/login?%s" (Uri.encoded_of_query query) in
  let github_login =
    match github with
    | None ->
      p [pcdata "(configure webauth to allow GitHub logins)"]
    | Some github ->
      p [
        a ~a:[a_href (Uri.to_string github)] [pcdata "Log in with GitHub"];
      ]
  in
  let warnings =
    if is_configured then []
    else [
      div ~a:[a_class ["alert"; "alert-warning"]] [
        pcdata
          "The CI has not yet been configured with an administrator user. \
           In the CI's logs you should find a URL containing a token - open this \
           URL to create an 'admin' user."
      ]
    ]
  in
  page "Login" Nav.Home ~user ([
    h2 [pcdata "Login"];
    form ~a:[a_class ["login-form"]; a_action action; a_method `Post; a_enctype "multipart/form-data"] [
      field "Username" `Text "user";
      field "Password" `Password "password";
      div [button ~a:[a_class ["btn"; "btn-primary"]; a_button_type `Submit] [pcdata "Log in"]];
    ];
    github_login;
  ] @ warnings) t

let auth_setup ~csrf_token =
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/auth/setup?%s" (Uri.encoded_of_query query) in
  page "Auth Setup" Nav.Home [
    form ~a:[a_class ["auth-setup-form"]; a_action action; a_method `Post; a_enctype "multipart/form-data"] [
      div ~a:[a_class ["form-group"]] [
        label ~a:[a_label_for "user"] [pcdata "Username"];
        input ~a:[a_class ["form-control"]; a_id "user"; a_input_type `Text;
                  a_disabled (); a_name "name"; a_value "admin"] ()
      ];
      field "Password" `Password "password";
      field "Confirm" `Password "password2";
      button ~a:[a_class ["btn"; "btn-default"]; a_button_type `Submit] [
        pcdata "Submit"
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
        pcdata "Log out"
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
      h2 [pcdata "Resource pools"];
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
  a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href url;] [span ~a:[a_class["glyphicon"; "glyphicon-time"]] []; pcdata "History"]

let log_button_group history log_url =
  div ~a:[a_class["btn-group";"pull-right"]] [
    history;
    a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href log_url;] [span ~a:[a_class["glyphicon"; "glyphicon-book"]] []; pcdata "Artefacts"];
  ]

let encode = Uri.pct_encode ~scheme:"http"

module LogScore : sig
  type t
  type log = [`Live of CI_live_log.t | `Saved of CI_output.saved]

  val create : unit -> t
  val update : t -> log -> unit
  val best : t -> log option
end = struct
  type score = int
  type log = [`Live of CI_live_log.t | `Saved of CI_output.saved]
  type t = (score * log) option ref

  let ok = 1
  let pending = 2
  let failed = 3

  let create () = ref None

  let update (best:t) (x:log) =
    let new_score =
      match x with
      | `Live _ -> pending
      | `Saved {CI_output.failed = true; _} -> failed
      | `Saved _ -> ok
    in
    match !best with
    | None -> best := Some (new_score, x)
    | Some (score, _) when score < new_score -> best := Some (new_score, x)
    | _ -> ()

  let best t =
    match !t with
    | None -> None
    | Some (_, x) -> Some x
end

let logs_frame_link = function
  | `Live live_log -> Printf.sprintf "/log/live/%s" (encode (CI_live_log.branch live_log))
  | `Saved {CI_output.branch; commit; _} -> Printf.sprintf "/log/saved/%s/%s" (encode branch) (encode commit)

let score_logs ~best job =
  let open CI_output in
  let rec aux = function
    | Empty -> ()
    | Live live_log -> LogScore.update best (`Live live_log);
    | Saved saved -> LogScore.update best (`Saved saved);
    | Pair (a, b) -> aux a; aux b
  in
  aux (CI_engine.state job).logs

let logs ~csrf_token ~page_url ~selected state =
  let open CI_output in
  let logs = state.logs in
  let last_title = ref None in
  let seen = ref String.Set.empty in
  let selected_branch =
    match selected with
    | Some (`Live x) -> Some (CI_live_log.branch x)
    | Some (`Saved x) -> Some x.branch
    | None -> None
  in
  let log_link ~branch ~title log =
    let cl =
      if Some branch = selected_branch then ["log-link"; "selected-log"]
      else ["log-link"]
    in
    let href = logs_frame_link log in
    span [
      pcdata "[ ";
      a ~a:[a_href href; a_target "iframe_log"; a_class cl] [pcdata "logs"];
      pcdata " ] ";
      pcdata title;
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
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; pcdata "Rebuild"];
          log_link ~branch ~title (`Live live_log);
        ];
      ]
    | Saved ({commit = _; branch; title; rebuild = _; failed} as saved) ->
      seen := String.Set.add branch !seen;
      let query = [
        "CSRFToken", [csrf_token];
        "redirect", [page_url];
      ] in
      let action = Printf.sprintf "/log/rebuild/%s?%s" branch (Uri.encoded_of_query query) in
      let status = if failed then `Failure else `Success in
      last_title := Some title;
      [
        form ~a:[a_action action; a_method `Post] [
          status_flag status;
          button ~a:[a_class ["btn"; "btn-default"; "btn-xs"; "rebuild"]; a_button_type `Submit] [
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; pcdata "Rebuild"];
          log_link ~branch ~title (`Saved saved);
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
  | Some shown when shown = state.descr -> items
  | _ -> items @ [p [status_flag state.status; pcdata state.descr]]

let job_row ~csrf_token ~page_url ~best_log job =
  let state = CI_engine.state job in
  let job_name = CI_engine.job_name job in
  tr [
    th [pcdata job_name];
    td [status state];
    td (
      logs ~csrf_token ~page_url ~selected:best_log (CI_engine.state job)
    );
  ]

let target_title = function
  | `PR pr -> Printf.sprintf "PR %d" (PR.number pr)
  | `Ref r -> Fmt.strf "Ref %a" Ref.pp_name (Ref.name r)

let target_commit = function
  | `PR pr -> PR.commit_hash pr
  | `Ref r -> Ref.commit_hash r

let target_repo = function
  | `PR pr -> PR.repo pr
  | `Ref r -> Ref.repo r

let target_page_url = CI_target.path_v

let target_page ~csrf_token ~target jobs t =
  let target = CI_engine.target target in
  let title = target_title target in
  let repo = target_repo target in
  let commit = target_commit target in
  let page_url = target_page_url target in
  let best_log =
    let best = LogScore.create () in
    List.iter (score_logs ~best) jobs;
    LogScore.best best
  in
  let state_summary = [
    table ~a:[a_class ["table"; "table-bordered"; "results"]] (List.map (job_row ~csrf_token ~page_url ~best_log) jobs)
  ] in
  let nav =
    match target with
    | `PR _ -> Nav.PRs
    | `Ref r ->
      match Ref.name r with
      | "heads" :: _ -> Nav.Branches
      | "tags" :: _ -> Nav.Tags
      | _ -> assert false
  in
  let logs =
    match best_log with
    | None -> None
    | Some best -> Some (logs_frame_link best)
  in
  page ~logs title nav (
    p [
      a ~a:[a_href (gh_target_url target)] [pcdata title];
      pcdata " has head commit ";
      a ~a:[a_href (commit_url ~repo commit)] [pcdata commit];
      pcdata " [ ";
      a ~a:[a_href (metadata_url t target)] [pcdata "head history"];
      pcdata " ]";
      pcdata " [ ";
      a ~a:[a_href (commit_history_url t target)] [pcdata "status history for this head"];
      pcdata " ]";
    ]
    :: state_summary
  ) t

let plain_page ~page_title =
  html
    (head (title (pcdata page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
      ])

let iframe_page ~page_title =
  html
    (head (title (pcdata page_title)) [
        meta ~a:[a_charset "utf-8"] ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
        base ~a:[a_target "_parent"] ();
      ])

let plain_error msg _t ~user:_ =
  plain_page ~page_title:"Error" (body [pcdata msg])

let live_log_frame ~branch ~live_log ~have_history t ~user:_ =
  let buttons =
    if have_history then [history_button (log_branch_history_url t branch)]
    else []
  in
  let page_title = Fmt.strf "Logging to branch %s" branch in
  iframe_page ~page_title
    (body ~a:[a_class ["log"]] [
        div ~a:[a_class ["row"]] [
          div ~a:[a_class ["col-md-9"]] [
            p [pcdata "Still running..."];
          ];
          div ~a:[a_class ["col-md-3"]] [
            div ~a:[a_class["btn-group";"pull-right"]] buttons
          ]
        ];
        pre [pcdata (CI_live_log.contents live_log)];
      ]
    )

let saved_log_frame ~commit ~branch ~log_data t ~user:_ =
  let page_title = Fmt.strf "Log from commit %s" commit in
  let history = history_button (log_branch_history_url t branch) in
  let log_url = log_branch_results_url t branch in
  let commit_url = log_commit_url t commit in
  iframe_page ~page_title
    (body ~a:[a_class ["log"]] [
        div ~a:[a_class ["row"]] [
          div ~a:[a_class ["col-md-9"]] [
            p [pcdata "Loaded from commit "; a ~a:[a_href commit_url] [pcdata commit]]
          ];
          div ~a:[a_class ["col-md-3"]] [
            log_button_group history log_url;
          ];
        ];
        pre [pcdata (Cstruct.to_string log_data)];
      ]
    )

let error_page id =
  page "Error" Nav.Home (
    if id = Error.no_state_repo then
      [
        p [pcdata "No web mirror of the state repository has been configured, so can't link to it."];
        p [pcdata "Configure DataKit to push to a GitHub repository and then pass the repository's URL using ";
           code [pcdata "Web.config ~state_repo"];
           pcdata "."
          ];
      ]
    else if id = Error.permission_denied then
      [
        p [pcdata "Permission denied"];
      ]
    else if id = Error.logout_needed then
      [
        p [pcdata
             "Access policy has changed - please log out and log back in so we can \
              check your credentials against the new policy."];
      ]
    else
      [
        p [pcdata (Printf.sprintf "Unknown error code %S" id)]
      ]
  )
