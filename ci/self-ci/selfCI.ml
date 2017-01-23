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
let local_git   = dockerfile ~timeout:(30. *. minute) "Dockerfile.bridge-local-git"
let datakit     = dockerfile ~timeout:(30. *. minute) "Dockerfile"

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

let datakit_tests repo target =
  if is_gh_pages target then []
  else (
    let src = Git.fetch_head repo target in
    let server = build server src in
    let client_image = build client src in
    let ci = build ci src in
    [
      "server",      test @@ server;
      "prometheus",  test @@ build prometheus src;
      "client",      test @@ client_image;
      "client-4.02", test @@ build client     src ~from:alpine_4_02;
      "ci",          test @@ ci;
      "self-ci",     test @@ build self_ci    src ~from:ci;
      "github",      test @@ build github     src ~from:server;
      "datakit",     test @@ build datakit    src ~from:server;
      "local-git",   test @@ build local_git  src ~from:client_image;
    ]
  )

let projects repo = [
  Config.project ~id:"docker/datakit" (datakit_tests repo)
]

(* Override the default https listener because we live behind an nginx proxy. *)
let listen_addr = `HTTP 8080

let make_config ?state_repo ~remote () =
  let repo = Git.v ~logs ~remote "/data/repos/datakit" in
  let web_config =
    Web.config
      ~name:"datakit-ci"
      ?state_repo
      ~can_read:ACL.everyone
      ~can_build:ACL.(username "admin")
      ~listen_addr
      ()
  in
  Config.v ~web_config ~projects:(projects repo)

let profiles = [
  "production", `Production;    (* Running on Docker Cloud *)
  "localhost",  `Localhost;     (* Running locally with docker-compose *)
]

let profile =
  let open Cmdliner in
  let doc =
    Arg.info ~doc:"Which configuration profile to use."
      ~docv:"PROFILE" ["profile"]
  in
  Arg.(value @@ opt (enum profiles) `Production doc)

let config = function
  | `Production ->
    make_config
      ~state_repo:(Uri.of_string "https://github.com/docker/datakit.logs")
      ~remote:"https://github.com/docker/datakit.git"
      ()
  | `Localhost ->
    (* We pull from a shared volume, not from GitHub, and we don't push the results. *)
    make_config
      ~remote:"/mnt/datakit"
      ()

let () =
  let open! Cmdliner.Term in
  run (pure config $ profile)
