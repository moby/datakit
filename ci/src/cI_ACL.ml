open Datakit_github

type t =
  [ `Everyone
  | `Username of string
  | `Github_org of string
  | `Can_read of Repo.t
  | `Any of t list
  ]

let everyone = `Everyone
let username x = `Username x
let github_org x = `Github_org x
let any ts = `Any ts

let can_read_github repo =
  match Repo.of_string repo with
  | Some r -> `Can_read r
  | _      -> invalid_arg ("can_read_github: " ^ repo)
