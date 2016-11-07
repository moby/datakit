open DataKitCI

(* An example test that just always returns success. *)
let my_test =
  Term.return "Success!"

(* A list of GitHub projects to monitor. *)
let projects = [
  Config.project ~id:"me/my-project"    (* The project is at https://github.com/me/my-project *)
    ~dashboards:["master"]              (* Key branches to display in the dashboard overview *)
    [
      (* The tests to apply to the open PRs in this project. *)
      "my-test", my_test;
    ];
]

(* The URL of a mirror on GitHub of DataKit's state repository (optional). *)
let state_repo =
  None
  (* Some (Uri.of_string "https://github.com/my-org/my-project.logs") *)

let web_config =
  Web.config
    ~name:"example-ci"
    ~public:false               (* Allow (read-only) access when not logged in? *)
    ?state_repo
    ()

(* The main entry-point *)
let () =
  DataKitCI.Main.run (Cmdliner.Term.pure (Config.ci ~web_config ~projects))
