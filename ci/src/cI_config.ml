open Datakit_github
open! Astring

type test = string CI_term.t

type project = {
  dashboards : CI_target.Set.t;
  tests : CI_target.t -> test String.Map.t;
}

type t = {
  web_config : CI_web_templates.t;
  projects : project Repo.Map.t;
}

let id_of_branch repo name = `Ref (repo, "heads" :: String.cuts ~sep:"/" name)

let project ~id ?(dashboards=["master"]) tests =
  let id = match Repo.of_string id with
    | None -> CI_utils.failf "Invalid repo ID %S" id
    | Some r -> r
  in
  let tests x = String.Map.of_list (tests x) in
  let dashboards = CI_target.Set.of_list (List.map (id_of_branch id) dashboards) in
  id, {tests; dashboards}

let v ~web_config ~projects =
  let projects = Repo.Map.of_list projects in
  { web_config; projects }
