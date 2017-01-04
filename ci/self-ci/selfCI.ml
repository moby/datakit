open Datakit_ci
open Term.Infix

let minute = 60.

let repo = Git.v ~logs ~remote:"https://github.com/docker/datakit.git" "/data/repos/datakit"

let is_gh_pages = function
  | `Ref id -> (match id with (_, ["heads"; "gh-pages"]) -> true | _ -> false)
  | _ -> false

let docker_build target ~timeout name =
  let dockerfile =
    match name with
    | "datakit" -> "Dockerfile"
    | name -> "Dockerfile." ^ name
  in
  let build =
    Git.command ~timeout ~logs ~label:("docker-build-" ^ name) ~clone:true
      [
        [| "docker"; "build"; "--pull"; "-f"; dockerfile; "." |]
      ]
  in
  let term =
    Git.fetch_head repo target >>= fun src ->
    Git.run build src >|= fun () ->
    "Build succeeded"
  in
  (name, term)

let datakit_tests target =
  if is_gh_pages target then []
  else [
    docker_build target ~timeout:(30. *. minute) "client";
    docker_build target ~timeout:(30. *. minute) "ci";
    docker_build target ~timeout:(30. *. minute) "server";
    docker_build target ~timeout:(30. *. minute) "github";
    docker_build target ~timeout:(30. *. minute) "datakit";
  ]

let projects = [
  Config.project ~id:"docker/datakit" datakit_tests
]

let metrics_path = "./metrics_token"

let metrics_token =
  if Sys.file_exists metrics_path then (
    let ch = open_in "metrics_token" in
    let token = input_line ch in
    close_in ch;
    Some (`SHA256 (B64.decode token))
  ) else (
    Fmt.epr "%S does not exist; metrics will not be available@." metrics_path;
    None
  )

(* Override the default https listener because we live behind an nginx proxy. *)
let listen_addr = `HTTP 8080

let web_config =
  Web.config
    ~name:"datakit-ci"
    ~state_repo:(Uri.of_string "https://github.com/docker/datakit.logs")
    ~can_read:ACL.everyone
    ~can_build:ACL.(username "admin")
    ?metrics_token
    ~listen_addr
    ()

let () =
  run (Cmdliner.Term.pure (Config.v ~web_config ~projects))
