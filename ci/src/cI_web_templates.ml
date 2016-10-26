open! Astring
open! Tyxml.Html

type logs =
  [ `Live_log of string * string option
  | `No_log
  | `Pair of logs * logs
  | `Saved_log of string * string * string ]

let hooks_url = "https://github.com/docker/datakit-test-dump/tree/github-metadata"

let log_commit_url commit =
  Printf.sprintf "https://github.com/docker/datakit-test-dump/commit/%s" commit

let log_branch_history_url branch =
  Printf.sprintf "https://github.com/docker/datakit-test-dump/commits/%s/log" branch

let log_branch_results_url branch =
  Printf.sprintf "https://github.com/docker/datakit-test-dump/tree/%s" branch

let gh_ref_url r =
  let { CI_projectID.user; project } = CI_github_hooks.Ref.project r in
  let id = CI_github_hooks.Ref.name r in
  match Datakit_path.unwrap id with
  | "tags" :: id -> Fmt.strf "https://github.com/%s/%s/releases/tag/%s" user project (String.concat ~sep:"/" id)
  | _            -> Fmt.strf "https://github.com/%s/%s/tree/%a" user project Datakit_path.pp id

let ref_metadata_url r =
  let { CI_projectID.user; project } = CI_github_hooks.Ref.project r in
  let id = CI_github_hooks.Ref.name r in
  Fmt.strf "https://github.com/docker/datakit-test-dump/commits/github-metadata/%s/%s/ref/%a" user project
    Datakit_path.pp id

let gh_pr_url pr =
  let { CI_projectID.user; project } = CI_github_hooks.PR.project pr in
  let id = CI_github_hooks.PR.id pr in
  Printf.sprintf "https://github.com/%s/%s/pull/%d" user project id

let pr_metadata_url pr =
  let { CI_projectID.user; project } = CI_github_hooks.PR.project pr in
  let id = CI_github_hooks.PR.id pr in
  Printf.sprintf "https://github.com/docker/datakit-test-dump/commits/github-metadata/%s/%s/pr/%d" user project id

let commit_history_url target =
  let { CI_projectID.user; project }, commit =
    match target with
    | `PR pr -> CI_github_hooks.PR.project pr, CI_github_hooks.PR.head pr
    | `Ref r -> CI_github_hooks.Ref.project r, CI_github_hooks.Ref.head r
  in
  let hash = CI_github_hooks.Commit.hash commit in
  Printf.sprintf "https://github.com/docker/datakit-test-dump/commits/github-metadata/%s/%s/commit/%s" user project hash

let commit_url ~project commit =
  let { CI_projectID.user; project } = project in
  Printf.sprintf "https://github.com/%s/%s/commit/%s" user project commit

let pr_url { CI_projectID.user; project} pr =
  Printf.sprintf "/pr/%s/%s/%d" user project pr

let escape_ref path =
  Uri.pct_encode ~scheme:"http" (String.concat ~sep:"/" (Datakit_path.unwrap path))

let unescape_ref s =
  Uri.pct_decode s |> Datakit_path.of_string_exn

let ref_url { CI_projectID.user; project} r =
  Printf.sprintf "/ref/%s/%s/%s" user project (escape_ref r)

let tag_map f map =
  Datakit_path.Map.fold (fun key value acc ->
      match Datakit_path.unwrap key with
      | "tags" :: _ -> [f key value] @ acc
      | _ -> acc
    ) map []

let branch_map f map =
  Datakit_path.Map.fold (fun key value acc ->
      match Datakit_path.unwrap key with
      | "heads" :: _ -> [f key value] @ acc
      | _ -> acc
    ) map []

let int_map f map =
  CI_utils.IntMap.fold (fun key value acc ->
      [f key value] @ acc
    ) map []

let dash_map f map targets =
  Datakit_path.Map.fold (fun key value acc ->
      match CI_target.ID_Set.mem (`Ref key) targets with
      | true -> [f key value] @ acc
      | false -> acc
    ) map []

let status state =
  let colour, icon, status =
    match state.CI_state.status with
    | `Pending -> "label-warning", "glyphicon-hourglass", "Pending"
    | `Success -> "label-success", "glyphicon-ok","Success"
    | `Error -> "label-danger", "glyphicon-warning-sign", "Error"
    | `Failure -> "label-danger", "glyphicon-remove", "Failure"
  in
  span ~a:[a_class ["label"; colour;]] [span ~a:[a_class ["glyphicon"; icon]] []; pcdata status]

let summarise jobs =
  let states = List.map (fun j -> CI_engine.job_name j, CI_engine.state j) jobs in
  let combine status states =
    let results = ref String.Map.empty in
    states |> List.iter (fun (name, state) ->
        let descr = state.CI_state.descr in
        let old_names = String.Map.find descr !results |> CI_utils.default [] in
        results := String.Map.add descr (name :: old_names) !results
      );
    let results = String.Map.bindings !results in
    let pp_group f (descr, g) = Fmt.pf f "%s (%a)" descr (Fmt.(list ~sep:(const string ", ") Fmt.string)) (List.rev g) in
    let descr = Fmt.strf "%a" Fmt.(list ~sep:(const string "; ") pp_group) results in
    { CI_state.status; descr; logs = CI_result.Step_log.Empty }
  in
  let pending, states = List.partition (fun (_, x) -> x.CI_state.status = `Pending) states in
  if pending <> [] then combine `Pending pending
  else (
    let failed, states = List.partition (fun (_, x) -> x.CI_state.status = `Failure) states in
    if failed <> [] then combine `Failure failed
    else combine `Success states
  )

let dashboard_widget id ref =
  let state = CI_engine.jobs ref |> summarise in
  let cls, icon, status, comment =
    match state.CI_state.status with
    | `Pending -> "dashboard-pending", "glyphicon-hourglass", "Pending", "... WAITING ..."
    | `Success -> "dashboard-success", "glyphicon-ok", "Succeeding", "YAY! The build is fine... Nothing to see here..."
    | `Error -> "dashboard-error", "glyphicon-warning-sign", "Erroring", "OH NO! Something has gone terribly wrong"
    | `Failure -> "dashboard-failure", "glyphicon-remove", "Failing", "SOUND THE ALARM!!! The build has been broken!"
  in
  let title =
    match Datakit_path.unwrap id with
    | ("heads" | "tags") :: tl -> tl
    | x -> x
  in
  div ~a:[a_class ["col-md-4"; "text-center"; "dashboard"; cls]] [
    h2 ~a:[a_id "title"] [ pcdata (String.concat ~sep:"/" title) ];
    h2 ~a:[a_id "icon"] [ span ~a:[a_class["glyphicon"; icon]] [] ];
    h2 ~a:[a_id "status"] [ pcdata status ];
    small [ pcdata comment ];
  ]

let ref_job ~project id ref =
  let state = CI_engine.jobs ref |> summarise in
  tr [
    td [a ~a:[a_href (ref_url project id)] [pcdata (Fmt.to_to_string Datakit_path.pp id)]];
    td [status state];
    td [pcdata state.CI_state.descr];
  ]

let pr_job ~project id open_pr =
  let state = CI_engine.jobs open_pr |> summarise in
  tr [
    td [a ~a:[a_href (pr_url project id)] [pcdata (string_of_int id)]];
    td [pcdata (CI_engine.title open_pr)];
    td [status state];
    td [pcdata state.CI_state.descr];
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

let page t active children ~user =
  let navbar = build_navbar active in
  let user = user |> CI_utils.default "not logged in" in
  html
    (head (title (pcdata t)) [
        meta ~a:[a_charset "utf-8"] ();
        meta ~a:[a_http_equiv "X-UA-Compatible"; a_content "IE=edge"] ();
        meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1"] ();
        link ~rel:[`Icon] ~a:[a_mime_type "image/png"] ~href:"/images/favicon.png" ();
        link ~rel:[`Stylesheet] ~href:"/css/style.css" ();
        link ~rel:[`Stylesheet] ~href:"/css/bootstrap.min.css" ();
      ])
    (body [
        nav ~a:[a_class["navbar";"navbar-inverse";"navbar-fixed-top"]] [
          div ~a:[a_class["container"]] [
            div ~a:[a_class["navbar-header"]] [
              button ~a:[a_user_data "toggle" "collapse"; a_user_data "target" "#navbar"; a_class ["navbar-toggle"]; a_button_type `Button;] [

                span ~a:[a_class["sr-only"]] [pcdata "Toggle Navigation"];
                span ~a:[a_class["icon-bar"]] [];
                span ~a:[a_class["icon-bar"]] [];
                span ~a:[a_class["icon-bar"]] [];

              ];
              a ~a:[a_class["navbar-brand"]; a_href "/"] [pcdata "datakit-ci"];
            ];
            div ~a:[a_id "navbar"; a_class["collapse"; "navbar-collapse"]] [
              ul ~a:[a_class ["nav"; "navbar-nav"]] navbar;
              ul ~a:[a_class ["nav"; "navbar-nav"; "navbar-right"]] [
                li [a ~a:[a_href "/user/profile"] [pcdata user]]
              ];
            ];
          ];
        ];
        div ~a:[a_class ["container"]] [
          div ~a:[a_class ["content"]] children;
        ]
      ]
    )

let opt_warning ci =
  let dk = CI_engine.dk ci in
  match Lwt.state dk with
  | Lwt.Return _ -> []
  | Lwt.Sleep -> [div ~a:[a_class ["warning"]] [pcdata "Connecting to DataKit..."]]
  | Lwt.Fail ex ->
    let msg = Fmt.strf "DataKit connection is down: %s" (Printexc.to_string ex) in
    [div ~a:[a_class ["warning"]] [pcdata msg]]

let pr_table (id, (prs, _)) =
  div [
    h2 [pcdata (Fmt.strf "PR status for %a" CI_projectID.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "PR"; heading "Title"; heading "State"; heading "Details"] ::
      (int_map (pr_job ~project:id) prs)
    );
  ]

let branch_table (id, (_, refs)) =
  div [
    h2 [pcdata (Fmt.strf "Branches for %a" CI_projectID.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "Ref"; heading "State"; heading "Details"] ::
      (branch_map (ref_job ~project:id) refs)
    );
  ]

let tag_table (id, (_, refs)) =
  div [
    h2 [pcdata (Fmt.strf "Tags for %a" CI_projectID.pp id)];
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] (
      tr [heading "Ref"; heading "State"; heading "Details"] ::
      (tag_map (ref_job ~project:id) refs)
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

let html_of_user ~csrf_token (reason, log) =
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
  [
    br ();
    form ~a:[a_class ["cancel"]; a_action action; a_method `Post] [
      button ~a:(a_class ["btn"; "btn-default"] :: a_button_type `Submit :: cancel_attrs) [
        span ~a:[a_class ["glyphicon"; "glyphicon-remove"]] []; pcdata "Cancel"
      ];
    ];
    pcdata reason;
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

let login_page ~csrf_token ~user =
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/auth/login?%s" (Uri.encoded_of_query query) in
  let field descr ty name =
    let id = "field-" ^ name in
    div ~a:[a_class ["form-group"]] [
      label ~a:[a_label_for id] [pcdata descr];
      input ~a:[a_class ["form-control"]; a_id id; a_input_type ty; a_name name] ()
    ]
  in
  page "Login" Nav.Home ~user @@ [
      h2 [pcdata "Login"];
      form ~a:[a_class ["login-form"]; a_action action; a_method `Post; a_enctype "multipart/form-data"] [
        field "Username" `Text "user";
        field "Password" `Password "password";
        div [button ~a:[a_class ["btn"; "btn-primary"]; a_button_type `Submit] [pcdata "Log in"]];
      ]
    ]

let protected_page title tab body ~user =
  page title tab body ~user:(Some user)

let user_page ~csrf_token =
  let query = [
    "CSRFToken", [csrf_token];
  ] in
  let action = Printf.sprintf "/auth/logout?%s" (Uri.encoded_of_query query) in
  protected_page "Profile" Nav.Home [
    form ~a:[a_class ["logout-form"]; a_action action; a_method `Post] [
      button ~a:[a_class ["btn"; "btn-default"]; a_button_type `Submit] [
        pcdata "Log out"
      ]
    ]
  ]

let main_page ~csrf_token ~ci ~dashboards =
  let projects = CI_engine.targets ci in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (CI_projectID.Map.bindings projects |> List.map title_of_project |> String.concat ~sep:" / ") in
  let dashboard_widgets =
    let combined =
      CI_projectID.Map.merge (fun _ project_state dash_config ->
          match project_state, dash_config with
          | Some (_prs, refs), Some y -> Some (refs, y)
          | _ -> None
        ) projects dashboards
    in
    CI_projectID.Map.fold dashboard_table combined []
  in
  protected_page title Nav.Home @@ opt_warning ci @ dashboard_widgets @ [
      h2 [pcdata "Resource pools"];
      resource_pools ~csrf_token;
    ]

let prs_page ~ci =
  let projects = CI_engine.targets ci |> CI_projectID.Map.bindings in
  let sections = List.map pr_table projects in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (List.map title_of_project projects |> String.concat ~sep:" / ") in
  protected_page title Nav.PRs @@ opt_warning ci @ sections @ [
      h2 [pcdata "Forgotten PRs"];
      p [pcdata "If you think your PR should be listed here and it isn't, that probably means the event monitor didn't add it to ";
         a ~a:[a_href hooks_url] [pcdata "the datakit-test-dump repository"];
         pcdata ".";
        ];
    ]

let branches_page ~ci =
  let projects = CI_engine.targets ci |> CI_projectID.Map.bindings in
  let sections = List.map branch_table projects in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (List.map title_of_project projects |> String.concat ~sep:" / ") in
  protected_page title Nav.Branches @@ opt_warning ci @ sections

let tags_page ~ci =
  let projects = CI_engine.targets ci |> CI_projectID.Map.bindings in
  let sections = List.map tag_table projects in
  let title_of_project (id, _) = Fmt.to_to_string CI_projectID.pp id in
  let title = "CI: " ^ (List.map title_of_project projects |> String.concat ~sep:" / ") in
  protected_page title Nav.Tags @@ opt_warning ci @ sections

let history_button url =
  a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href url;] [span ~a:[a_class["glyphicon"; "glyphicon-time"]] []; pcdata "History"]

let log_end_anchor_link branch =
  Printf.sprintf "#log-%s" branch

let log_end_anchor branch =
  span ~a:[a_id (Fmt.strf "log-%s" branch)] []

let log_button_group history log_url anchor_link =
  div ~a:[a_class["btn-group";"pull-right"]] [
    history;
    a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href log_url;] [span ~a:[a_class["glyphicon"; "glyphicon-book"]] []; pcdata "Artefacts"];
    a ~a:[a_class["btn"; "btn-default"; "btn-sm"]; a_href anchor_link;] [span ~a:[a_class["glyphicon"; "glyphicon-chevron-down"]] []; pcdata "Skip to End"];
  ]

let rec log = function
  | `No_log -> []
  | `Live_log (data, branch) ->
    let link =
      match branch with
      | None -> []
      | Some branch -> [history_button (log_branch_history_url branch)]
    in
    [
      p (pcdata "Still running" :: link);
      pre ~a:[a_class ["log"]] [pcdata data]
    ]
  | `Saved_log (data, commit, branch) ->
    let commit_url = log_commit_url commit in
    let history = history_button (log_branch_history_url branch) in
    let log_url = log_branch_results_url branch in
    let end_link = log_end_anchor_link branch in
    let end_anchor = log_end_anchor branch in
    [
      div ~a:[a_class ["row"]] [
        div ~a:[a_class ["col-md-6"]] [
          p [pcdata "Loaded from commit "; a ~a:[a_href commit_url] [pcdata commit]]
        ];
        div ~a:[a_class ["col-md-6"]] [
          log_button_group history log_url end_link;
        ];
      ];
      div ~a:[a_class ["row"]] [
        div ~a:[a_class["col-md-12"]] [
          pre ~a:[a_class ["log"]] [pcdata data];
        ]
      ];
      end_anchor
    ]
  | `Pair (a, b) -> log a @ log b

let job_id job_name =
  Fmt.strf "job-%s" job_name

let job ~csrf_token ~page_url ~target (job, log_data) =
  let target = CI_engine.git_target target in
  let state = CI_engine.state job in
  let metadata =
    let row head content = tr [heading head; td content] in
    table ~a:[a_class["table"; "table-bordered"; "table-hover"]] [
      row "State"        [
        status state;
        pcdata " [ ";
        a ~a:[a_href (commit_history_url target)] [pcdata "history"];
        pcdata " ]"];
      row "Details"      [pcdata state.CI_state.descr];
    ] in
  let rebuild_buttons =
    CI_engine.rebuild_actions job |> List.map (fun label ->
        let query = [
          "action", [label];
          "CSRFToken", [csrf_token];
        ] in
        let job_name = CI_engine.job_name job in
        let action = Printf.sprintf "%s/%s/rebuild/?%s" page_url job_name (Uri.encoded_of_query query) in
        let msg = Fmt.strf "Rebuild : %s" label in
        form ~a:[a_class ["rebuild"]; a_action action; a_method `Post] [
          button ~a:[a_class ["btn"; "btn-default"]; a_button_type `Submit] [
            span ~a:[a_class ["glyphicon"; "glyphicon-refresh"; "pull-left"]] []; pcdata msg];
        ]
      )
  in
  let job_name = CI_engine.job_name job in
  div [
    h2 ~a:[a_id (job_id job_name)] [pcdata job_name];
    div ~a:[a_class ["job"]] (
      [metadata] @
      rebuild_buttons @
      [h3 [pcdata "Logs"]; ] @
      log log_data @
      [metadata];
    )
  ]

let job_row (job, _log_data) =
  let state = CI_engine.state job in
  let job_name = CI_engine.job_name job in
  tr [
    th [ a ~a:[a_href (Fmt.strf "#%s" (job_id job_name))] [pcdata job_name]];
    td [status state];
    td [pcdata state.CI_state.descr];
  ]

let pr_page ~csrf_token ~target jobs =
  let pr =
    match CI_engine.git_target target with
    | `PR pr -> pr
    | `Ref _ -> assert false
  in
  let pr_id = CI_github_hooks.PR.id pr in
  let title = Printf.sprintf "PR %d" pr_id in
  let project = CI_github_hooks.PR.project pr in
  let commit = CI_github_hooks.Commit.hash (CI_github_hooks.PR.head pr) in
  let row head content = tr [heading head; td content] in
  let state_summary =
    match jobs with
    | [_] -> []       (* Only display a summary when we have multiple jobs *)
    | jobs -> [table ~a:[a_class ["table"; "table-bordered"]] (List.map job_row jobs)]
  in
  let page_url = pr_url project pr_id in
  protected_page title Nav.PRs (
    table ~a:[a_class ["table"; "table-bordered"]] [
      row "PR on GitHub" [
        a ~a:[a_href (gh_pr_url pr)] [pcdata (string_of_int pr_id)];
        pcdata " [ ";
        a ~a:[a_href (pr_metadata_url pr)] [pcdata "history"];
        pcdata " ]";
      ];
      row "Title"        [pcdata (CI_github_hooks.PR.title pr)];
      row "Commit"       [a ~a:[a_href (commit_url ~project commit)] [pcdata commit]];
    ]
    :: state_summary @
    List.map (job ~csrf_token ~page_url ~target) jobs
  )

let ref_page ~csrf_token ~target jobs =
  match CI_engine.git_target target with
  | `PR _ -> assert false
  | `Ref r ->
    let project = CI_github_hooks.Ref.project r in
    let commit = CI_github_hooks.Commit.hash (CI_github_hooks.Ref.head r) in
    let row head content = tr [heading head; td content] in
    let state_summary =
      match jobs with
      | [_] -> []       (* Only display a summary when we have multiple jobs *)
      | jobs -> [table ~a:[a_class ["table"; "table-bordered"]] (List.map job_row jobs)]
    in
    let title = Fmt.strf "Ref on Github %a" Datakit_path.pp (CI_github_hooks.Ref.name r) in
    let page_url = ref_url project (CI_github_hooks.Ref.name r) in
    let nav =
      match CI_github_hooks.Ref.name r |> Datakit_path.unwrap with
      | "heads" :: _ -> Nav.Branches
      | "tags" :: _ -> Nav.Tags
      | _ -> assert false
    in
    protected_page title nav (
      table ~a:[a_class["table table-bordered"]][
        row "Branch on GitHub" [
          a ~a:[a_href (gh_ref_url r)] [pcdata title];
          pcdata " [ ";
          a ~a:[a_href (ref_metadata_url r)] [pcdata "history"];
          pcdata " ]";
        ];
        row "Commit" [a ~a:[a_href (commit_url ~project commit)] [pcdata commit]];
      ]
      :: state_summary @
      List.map (job ~csrf_token ~page_url ~target) jobs
    )
