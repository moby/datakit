open Datakit_ci
open Term.Infix

let minute = 60.

let pool = Monitored_pool.create "Docker build" 10

let dockerfile ?label ~timeout file =
  let label = label |> Utils.default file in
  Docker.create ~logs ~pool ~timeout ~label file

let prometheus  = dockerfile ~timeout:(30. *. minute) "Dockerfile.prometheus"
let client      = dockerfile ~timeout:(30. *. minute) "Dockerfile.client"
let ci          = dockerfile ~timeout:(30. *. minute) "Dockerfile.ci"
let self_ci     = dockerfile ~timeout:(30. *. minute) "ci/self-ci/Dockerfile" ~label:"Dockerfile.self-ci"
let server      = dockerfile ~timeout:(30. *. minute) "Dockerfile.server"
let github      = dockerfile ~timeout:(30. *. minute) "Dockerfile.github"
let datakit     = dockerfile ~timeout:(30. *. minute) "Dockerfile"

let repo = Git.v ~logs ~remote:"https://github.com/docker/datakit.git" "/data/repos/datakit"

let alpine_4_02 = Term.return (Docker.Image.of_published "ocaml/opam:alpine_ocaml-4.02.3")

let is_gh_pages = function
  | `Ref (_, ["heads"; "gh-pages"]) -> true
  | _ -> false

let build ?from dockerfile src =
  match from with
  | None -> src >>= Docker.build dockerfile ?from:None
  | Some from ->
    Term.without_logs (Term.pair src from) >>= fun (src, from) ->
    Docker.build dockerfile ~from src

let test term =
  term >|= fun (_:Docker.Image.t) -> "Build succeeded"

let datakit_tests target =
  if is_gh_pages target then []
  else (
    let src = Git.fetch_head repo target in
    let server = build server src in
    let ci = build ci src in
    [
      "server",      test @@ server;
      "prometheus",  test @@ build prometheus src;
      "client",      test @@ build client     src;
      "client-4.02", test @@ build client     src ~from:alpine_4_02;
      "ci",          test @@ ci;
      "self-ci",     test @@ build self_ci    src ~from:ci;
      "github",      test @@ build github     src ~from:server;
      "datakit",     test @@ build datakit    src ~from:server;
    ]
  )

let projects = [
  Config.project ~id:"docker/datakit" datakit_tests
]

(* Override the default https listener because we live behind an nginx proxy. *)
let listen_addr = `HTTP 8080

let web_config =
  Web.config
    ~name:"datakit-ci"
    ~state_repo:(Uri.of_string "https://github.com/docker/datakit.logs")
    ~can_read:ACL.everyone
    ~can_build:ACL.(username "admin")
    ~listen_addr
    ()

let () =
  run (Cmdliner.Term.pure (Config.v ~web_config ~projects))
