open! Astring
open Datakit_ci

let minute = 60.

let pool = Monitored_pool.create "Docker" 10

module Dockerfile = struct
  (* [v ~timeout file] is a caching builder for [file]. *)
  let v ?label ~timeout file =
    let label = label |> Utils.default file in
    Docker.create ~logs ~pool ~timeout ~label file

  let client      = v ~timeout:(30. *. minute) "Dockerfile.client"
  let ci          = v ~timeout:(30. *. minute) "Dockerfile.ci"
  let self_ci     = v ~timeout:(30. *. minute) "ci/self-ci/Dockerfile" ~label:"Dockerfile.self-ci"
  let github      = v ~timeout:(30. *. minute) "Dockerfile.github"
  let local_git   = v ~timeout:(30. *. minute) "Dockerfile.bridge-local-git"
  let datakit     = v ~timeout:(30. *. minute) "Dockerfile"
end

module Tests = struct
  open Term.Infix

  (* Rules to create the images from the Dockerfiles.
     Note that we may build multiple images from the same Dockerfile (to test different bases).
     We also define the dependencies between the builds here. *)
  let images src =
    let build ?from dockerfile =
      match from with
      | None -> src >>= Docker.build dockerfile ?from:None
      | Some from ->
        Term.without_logs (Term.pair src from) >>= fun (src, from) ->
        Docker.build dockerfile ~from src
    in
    object(self)
      method client      = build Dockerfile.client
      method local_git   = build Dockerfile.local_git  ~from:self#client
      method ci          = build Dockerfile.ci
      method self_ci     = build Dockerfile.self_ci    ~from:self#ci
      method github      = build Dockerfile.github
      method datakit     = build Dockerfile.datakit
    end

  let check_builds term =
    term >|= fun (_:Docker.Image.t) -> "Build succeeded"

  (* [datakit repo target] is the set of tests to run on [target]. *)
  let datakit repo = function
    | `Ref (_, ["heads"; "gh-pages"]) -> []       (* Don't try to build the gh-pages branch *)
    | target ->
      let src = Git.fetch_head repo target in
      let images = images src in
      [
        "ci",          check_builds images#ci;
        "self-ci",     check_builds images#self_ci;
        "github",      check_builds images#github;
        "datakit",     check_builds images#datakit;
        "local-git",   check_builds images#local_git;
        "libraries",   Term.wait_for_all [
          "client",      check_builds images#client;
        ] >|= fun () -> "Library tests succeeded"
      ]
end

let projects repo = [
  Config.project ~id:"moby/datakit" (Tests.datakit repo)
]

let can_build =
  let open ACL in
  any [
    username "admin";
    github_org "moby";
  ]

let make_config ?state_repo ~listen_addr ~remote () =
  let repo = Git.v ~logs ~remote "/data/repos/datakit" in
  let web_config =
    Web.config
      ~name:"datakit-ci"
      ?state_repo
      ~github_scopes_needed:[`Read_org]
      ~can_read:ACL.everyone
      ~can_build
      ~listen_addr
      ()
  in
  Config.v ~web_config ~projects:(projects repo)

let config_for = function
  | `Production ->
    make_config
      ~remote:"https://github.com/moby/datakit.git"
      ~state_repo:(Uri.of_string "https://github.com/moby/datakit.logs")
      ~listen_addr:(`HTTP 8080) (* We live behind an nginx proxy. *)
      ()
  | `Localhost ->
    (* We pull from a shared volume, not from GitHub, and we don't push the results. *)
    make_config
      ~remote:"/mnt/datakit"
      ~listen_addr:(`HTTP 8080) (* Don't bother with TLS when testing locally. *)
      ()

(* Command-line parsing *)

open Cmdliner

let profiles = [
  "production", `Production;    (* Running on Docker Cloud *)
  "localhost",  `Localhost;     (* Running locally with docker-compose *)
]

(* The "--profile=PROFILE" option *)
let profile =
  let doc =
    Arg.info ["profile"]
      ~docv:"PROFILE"
      ~doc:"Which configuration profile to use."
  in
  Arg.(value @@ opt (enum profiles) `Production doc)

let () =
  Datakit_ci.run Cmdliner.Term.(pure config_for $ profile)
