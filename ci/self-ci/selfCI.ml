open Datakit_ci
open Term.Infix

let minute = 60.

let pool = Monitored_pool.create "Docker build" 10

let dockerfile ~timeout file = Docker.create ~logs ~pool ~timeout ~label:file file

let prometheus  = dockerfile ~timeout:(30. *. minute) "Dockerfile.prometheus"
let client      = dockerfile ~timeout:(30. *. minute) "Dockerfile.client"
let ci          = dockerfile ~timeout:(30. *. minute) "Dockerfile.ci"
let server      = dockerfile ~timeout:(30. *. minute) "Dockerfile.server"
let github      = dockerfile ~timeout:(30. *. minute) "Dockerfile.github"
let datakit     = dockerfile ~timeout:(30. *. minute) "Dockerfile"

let repo = Git.v ~logs ~remote:"https://github.com/docker/datakit.git" "/data/repos/datakit"

let is_gh_pages = function
  | `Ref (_, ["heads"; "gh-pages"]) -> true
  | _ -> false

let build ?from dockerfile src =
  match from with
  | None -> src >>= Docker.build dockerfile ?from:None
  | Some from ->
    Term.pair src from >>= fun (src, from) ->
    Docker.build dockerfile ~from src

let test name term =
  name, (term >|= fun (_:Docker.Image.t) -> "Build succeeded")

let datakit_tests target =
  if is_gh_pages target then []
  else (
    let src = Git.fetch_head repo target in
    let server = build server src in
    [
      test "server"     @@ server;
      test "prometheus" @@ build prometheus src;
      test "client"     @@ build client     src;
      test "ci"         @@ build ci         src;
      test "github"     @@ build github     src ~from:server;
      test "datakit"    @@ build datakit    src ~from:server;
    ]
  )

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
