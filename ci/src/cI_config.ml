open Datakit_github
open! Astring

type test = string CI_term.t

type project = {
  dashboards : CI_target.ID_Set.t;
  tests : CI_target.Full.t -> test String.Map.t;
}

type t = {
  web_config : CI_web_templates.t;
  projects : project Repo.Map.t;
}

let id_of_branch name =
  `Ref (Datakit_path.of_string_exn ("heads/" ^ name))

let project ~id ?(dashboards=["master"]) tests =
  let id = match Repo.of_string id with
    | None -> CI_utils.failf "Invalid repo ID %S" id
    | Some r -> r
  in
  let tests x = String.Map.of_list (tests x) in
  let dashboards = CI_target.ID_Set.of_list (List.map id_of_branch dashboards) in
  id, {tests; dashboards}

let ci ~web_config ~projects =
  let projects = Repo.Map.of_list projects in
  { web_config; projects }
