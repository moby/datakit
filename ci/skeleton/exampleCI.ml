open DataKitCI

(* An example test that just always returns success. *)
let my_test =
  Term.return "Success!"

(* The configuration for a project that has a single test called "my-test". *)
let my_project = [
  "my-test", my_test;
]

(* A list of GitHub projects to monitor and the tests to apply to the open PRs in each one. *)
let my_projects = [
  "me/my-project", my_project;
]

(* Parsing of command-line options (none in this example). *)
let projects =
  Cmdliner.Term.pure my_projects

(* The main entry-point *)
let () =
  DataKitCI.Main.run projects
