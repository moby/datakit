open Datakit_ci

(* An example test that just always returns success. *)
let my_test = Term.return "Success!"

let tests _target =
  [
    "my-test", my_test;
  ]

(* A list of GitHub projects to monitor. *)
let projects = [
  Config.project ~id:"me/my-project"    (* The project is at https://github.com/me/my-project *)
    ~dashboards:["master"]              (* Key branches to display in the dashboard overview *)
    tests                               (* The tests to apply to the open PRs in this project. *)
]

(* The URL of a mirror on GitHub of DataKit's state repository (optional). *)
let state_repo =
  None
  (* Some (Uri.of_string "https://github.com/my-org/my-project.logs") *)

let metrics_token =
  (* Provide metrics at [/metrics] (optional).
     The caller must provide an "Authorization: Bearer s3cret" header.
     To avoid keeping the secret directly in this file, we store the sha256 hash of TOKEN.
     To calculate this value, pick a *long* secret (not like this example) and hash it like this:
     base64.b64encode(hashlib.sha256('s3cret').digest())
  *)
  `SHA256 (B64.decode "HsHCa1DV08WNlYMYGvgHZlX+AHVr9yhZQLo2cPmfy6A=")

let web_config =
  Web.config
    ~name:"example-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(username "admin")
    ?state_repo
    ~metrics_token
    ()

(* The main entry-point *)
let () = run (Cmdliner.Term.pure (Config.v ~web_config ~projects))
