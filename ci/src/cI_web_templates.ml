open Datakit_github
open! Astring
open! Tyxml.Html

type t = {
  name : string;
  state_repo : Uri.t option;
  metrics_token : [`SHA256 of Cstruct.t] option;
  listen_addr: [`HTTP of int | `HTTPS of int];
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

let job_state job =
  match CI_engine.state job with
  | None -> (Error (`Pending "(new)"), CI_output.Empty)
  | Some o -> o

let config ?(name="datakit-ci") ?state_repo ?metrics_token ?(listen_addr=`HTTPS 8443) ~can_read ~can_build () =
  let metrics_token =
    match metrics_token with
    | None -> None
    | Some (`SHA256 str) -> Some (`SHA256 (Cstruct.of_string str))
  in
  { name; state_repo; metrics_token; listen_addr; can_read; can_build }

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
    Printf.sprintf "https://github.com/%s/%s/pull/%d" user repo id
  | `Ref (repo, id) ->
    let { Repo.user; repo } = repo in
    match id with
    | "tags" :: id -> Fmt.strf "https://github.com/%s/%s/releases/tag/%s" user repo (String.concat ~sep:"/" id)
    | _            -> Fmt.strf "https://github.com/%s/%s/tree/%a" user repo Ref.pp_name id

let metadata_url t = function
  | `PR (repo, id) ->
    let { Repo.user; repo } = repo in
    state_repo_url t "commits/github-metadata/%s/%s/pr/%d" user repo id
  | `Ref (repo, id) ->
    let { Repo.user; repo } = repo in
    state_repo_url t "commits/github-metadata/%s/%s/ref/%a" user repo
      Ref.pp_name id

let commit_history_url t target ~hash =
  let { Repo.user; repo } = CI_target.repo target in
  state_repo_url t "commits/github-metadata/%s/%s/commit/%s" user repo hash

let commit_url ~repo commit =
  let { Repo.user; repo } = repo in
  Printf.sprintf "https://github.com/%s/%s/commit/%s" user repo commit

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
  span ~a:[a_class ["label"; colour;]] [span ~a:[a_class ["glyphicon"; icon]] []; pcdata status]

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
    h2 ~a:[a_id "title"] [ pcdata (String.concat ~sep:"/" title) ];
    h2 ~a:[a_id "icon"] [ span ~a:[a_class["glyphicon"; icon]] [] ];
    h2 ~a:[a_id "status"] [ pcdata status ];
    small [ pcdata comment ];
  ]

let ref_job (_repo, id) ref =
  match CI_engine.jobs ref with
  | [] -> []
  | jobs ->
    let summary = summarise jobs in
    let ref_url = CI_target.path_v (CI_engine.target ref) in
    [
      tr [
        td [a ~a:[a_href ref_url] [pcdata (Fmt.to_to_string Ref.pp_name id)]];
        td [status_list jobs];
        td [pcdata (CI_result.descr summary)];
      ]
    ]

let pr_job (_repo, id) open_pr =
  match CI_engine.jobs open_pr with
  | [] -> []
  | jobs ->
    let summary = summarise jobs in
    let pr_url = CI_target.path_v (CI_engine.target open_pr) in
    [
      tr [
        td [a ~a:[a_href pr_url] [pcdata (string_of_int id)]];
        td [pcdata (CI_engine.title open_pr)];
        td [status_list jobs];
        td [pcdata (CI_result.descr summary)];
      ]
    ]

let heading x = th [pcdata x]

let url x =
  a ~a:[a_href x] [pcdata x]

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
    li ~a:[a_class cl] [a ~a:[a_href href] [pcdata (Nav.to_string name)]];
  in
  Nav.[
    item Home "/";
    item PRs "/pr";
    item Branches "/branch";
    item Tags "/tag";
    item Settings "/settings";
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
      script ~a:[a_mime_type "text/javascript"; a_src "/js/ci.js"] (pcdata "");
      script ~a:[a_mime_type "text/javascript";
                 a_src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"] (pcdata "");
      script ~a:[a_mime_type "text/javascript"; a_src "/js/bootstrap.min.js"] (pcdata "");
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

let login_page ?github ~csrf_token state ~is_configured t ~user =
  let field = CI_form.Html.field state in
  let action = "/auth/login" in
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
    CI_form.Html.form ~csrf_token state ~form_class:["login-form"] ~action [
      field "Username" `Text "user";
      field "Password" `Password "password";
      div [button ~a:[a_class ["btn"; "btn-primary"]; a_button_type `Submit] [pcdata "Log in"]];
    ];
    github_login;
  ] @ warnings) t

let auth_setup ~csrf_token state =
  let field = CI_form.Html.field state in
  let action = "/auth/setup" in
  page "Auth Setup" Nav.Home [
    CI_form.Html.form state ~csrf_token ~form_class:["auth-setup-form"] ~action [
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

let saved_log_frame_link ~branch ~commit = Printf.sprintf "/log/saved/%s/%s" (encode branch) (encode commit)

let logs_frame_link = function
  | `Live live_log -> Printf.sprintf "/log/live/%s" (encode (CI_live_log.branch live_log))
  | `Saved {CI_output.branch; commit; _} -> saved_log_frame_link ~branch ~commit

let score_logs ~best = function
  | (_name, None) -> ()
  | (_name, Some state) ->
    let open CI_output in
    let rec aux = function
      | Empty -> ()
      | Live live_log -> LogScore.update best (`Live live_log);
      | Saved saved -> LogScore.update best (`Saved saved);
      | Pair (a, b) -> aux a; aux b
    in
    aux (CI_output.logs state)

let logs ~csrf_token ~page_url ~selected state =
  let open CI_output in
  let logs = CI_output.logs state in
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
    | Saved ({commit = _; branch; title; rebuild; failed} as saved) ->
      seen := String.Set.add branch !seen;
      let query = [
        "CSRFToken", [csrf_token];
        "redirect", [page_url];
      ] in
      let action = Printf.sprintf "/log/rebuild/%s?%s" branch (Uri.encoded_of_query query) in
      let status = if failed then `Failure else `Success in
      let rebuild_button =
        match rebuild with
        | `Rebuildable _ -> 
          button ~a:[a_class ["btn"; "btn-default"; "btn-xs"; "rebuild"]; a_button_type `Submit] [
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; pcdata "Rebuild"];
        | `Rebuilding -> pcdata "(rebuild queued) "
        | `Archived -> pcdata "(archived) "
      in
      last_title := Some title;
      [
        form ~a:[a_action action; a_method `Post] [
          status_flag status;
          rebuild_button;
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
  | Some shown when shown = CI_output.descr state -> items
  | _ ->
    let status = CI_output.status state in
    let descr = CI_output.descr state in
    items @ [p [status_flag status; pcdata descr]]

let job_row ~csrf_token ~page_url ~best_log (job_name, state) =
  let output =
    match state with
    | None -> (Error (`Pending "(new)"), CI_output.Empty)
    | Some state -> state
  in
  tr [
    th [pcdata job_name];
    td [status (CI_output.status output)];
    td (
      logs ~csrf_token ~page_url ~selected:best_log output
    );
  ]

let target_title ~title = function
  | `PR (_, pr) -> Printf.sprintf "PR %d (%s)" pr title
  | `Ref (_, r) -> Fmt.strf "Ref %a" Ref.pp_name r

let map_or_none f = function
  | [] -> [li [pcdata "(none)"]]
  | xs -> List.map f xs

let commit_page ~commit ~archived_targets targets t =
  let title = Fmt.strf "Commit %s" commit in
  let target_link target =
    li [
      a ~a:[a_href (CI_target.path target)] [
        pcdata (Fmt.to_to_string CI_target.pp target)
      ]
    ]
  in
  let archive_target_link (target, commit) =
    let commit = CI_utils.DK.Commit.id commit in
    li [
      a ~a:[a_href (Fmt.strf "%s?history=%s" (CI_target.path target) commit)] [
        pcdata (Fmt.to_to_string CI_target.pp target)
      ]
    ]
  in
  page title Nav.Home [
    h2 [pcdata (Fmt.strf "Commit %s" commit)];
    p [pcdata "Current builds:"];
    ul (map_or_none target_link targets);
    p [pcdata "Archived builds:"];
    ul (map_or_none archive_target_link archived_targets);
  ] t

let target_page_url = CI_target.path

let state_link commit =
  let url = Fmt.strf "?history=%s" commit in
  a ~a:[a_href url] [pcdata commit]

let rec intersperse sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse sep xs

let history_nav t target state =
  let history_link = status_history_url t target in
  let links = [
      pcdata " [ "; a ~a:[a_href "?"] [pcdata "latest"]; pcdata " | ";
      a ~a:[a_href history_link] [pcdata "history"]; pcdata " ]";
    ]
  in
  match CI_history.State.parents state with
  | [] -> p (pcdata "No previous states" :: links)
  | [x] -> p (pcdata "Previous state: " :: state_link x :: links)
  | xs -> p (pcdata "Previous states: " :: (intersperse (pcdata ", ") (List.map state_link xs)) @ links)

let target_page ~csrf_token ?(title="(no title)") ~(target:CI_target.t) state t =
  let jobs = CI_history.State.jobs state |> String.Map.bindings |> List.map (fun (name, s) -> name, Some s) in
  let title = target_title ~title target in
  let repo = CI_target.repo target in
  let commit = CI_history.State.source_commit state in
  let metadata_commit = CI_history.State.metadata_commit state |> CI_utils.default "MISSING-COMMIT" in
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
    | `Ref (_, r) ->
      match r with
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
    history_nav t target state
    :: p [
      a ~a:[a_href (gh_target_url target)] [pcdata title];
      pcdata " has head commit ";
      (match commit with
       | None -> pcdata "(missing commit info)"
       | Some commit -> a ~a:[a_href (commit_url ~repo commit)] [pcdata commit]
      );
      pcdata " [ ";
      a ~a:[a_href (metadata_url t target)] [pcdata "head history"];
      pcdata " ]";
      pcdata " [ ";
      a ~a:[a_href (commit_history_url t target ~hash:metadata_commit)] [pcdata "status history for this head"];
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
        script (pcdata "window.top.highlight_log()");
      ])

let plain_error msg _t ~user:_ =
  plain_page ~page_title:"Error" (body [pcdata msg])

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
            p [pcdata "Still running..."];
          ];
          div ~a:[a_class ["col-md-3"]] [
            div ~a:[a_class["btn-group";"pull-right"]] buttons
          ]
        ];
        pre [pcdata "@STREAM-GOES-HERE@"];
        p [pcdata "This log is now complete."];
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
            p [pcdata "Loaded from commit "; a ~a:[a_href commit_url] [pcdata commit]]
          ];
          div ~a:[a_class ["col-md-3"]] [
            log_button_group history log_url;
          ];
        ];
        pre [pcdata "@STREAM-GOES-HERE@"];
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

module Settings = struct
  let index =
    page "Settings" Nav.Settings [
      ul [
        li [
          a ~a:[a_href "/settings/github-auth"] [pcdata "Configure GitHub authentication"]
        ]
      ]
    ]

  let github_auth ~csrf_token state =
    let action = "/settings/github-auth" in
    let field = CI_form.Html.field state in
    page "GitHub authentication" Nav.Settings [
      div ~a:[a_class ["github-auth"]] [
        h2 [pcdata "GitHub authentication"];
        p [pcdata "You can configure the CI to allow users to authenticate using their GitHub accounts. To do this:"];
        ol [
          li [
            p [pcdata "Go to "; url "https://github.com/settings/applications/new"; pcdata " and register your CI. "];
            p [pcdata "Give the callback URL as <https://HOST:PORT/auth/github-callback> \
                       (you can use 'localhost' for testing)."];
          ];
          li [
            p [pcdata "Enter the Client ID and Client Secret below."];
            p [pcdata
                 "You can also specify a callback here, if you want to override the one you registered with GitHub above. \
                  This might be useful if you have several CI instances using a single Client ID.";
              ]
          ]
        ];
        CI_form.Html.form ~csrf_token state ~form_class:["github-auth"] ~action [
          field "Client ID" `Text "client-id";
          field "Client Secret" `Password "client-secret";
          field "Callback (optional)" `Url "callback";
          div [button ~a:[a_class ["btn"; "btn-primary"]; a_button_type `Submit] [pcdata "Submit"]];
        ];
      ]
    ]
end
