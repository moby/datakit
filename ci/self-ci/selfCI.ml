open DataKitCI
open DataKitCI.Term.Infix

let logs = Main.logs
let minute = 60.

let repo = DKCI_git.connect ~logs ~dir:"/data/repos/datakit"

let is_pages_branch =
  Term.target >|= function
  | `Ref r ->
    begin match Datakit_path.unwrap (Github_hooks.Ref.name r) with
      | ["heads"; "gh-pages"] -> true
      | _ -> false
    end
  | _ -> false

let docker_build ~timeout name =
  let dockerfile =
    match name with
    | "datakit" -> "Dockerfile"
    | name -> "Dockerfile." ^ name
  in
  let build = DKCI_git.command ~timeout ~logs ~label:("docker-build-" ^ name) ~clone:true
      [
        [| "docker"; "build"; "--pull"; "-f"; dockerfile; "." |]
      ]
  in
  let term =
    is_pages_branch >>= function
    | true -> Term.return "(nothing to test on gh-pages)"
    | false ->
      DKCI_git.fetch_head repo >>= fun src ->
      DKCI_git.run build src >>= fun () ->
      Term.return "Build succeeded"
  in
  (name, term)

let projects = [
  Config.project ~id:"docker/datakit"
    [
      docker_build ~timeout:(30. *. minute) "client";
      docker_build ~timeout:(30. *. minute) "ci";
      docker_build ~timeout:(30. *. minute) "server";
      docker_build ~timeout:(30. *. minute) "github";
      docker_build ~timeout:(30. *. minute) "datakit";
    ];
]

let web_config =
  Web.config
    ~name:"datakit-ci"
    ~state_repo:(Uri.of_string "https://github.com/docker/datakit.logs")
    ~can_read:ACL.everyone
    ~can_build:ACL.(username "admin")
    ()

let () =
  DataKitCI.Main.run (Cmdliner.Term.pure (Config.ci ~web_config ~projects))
