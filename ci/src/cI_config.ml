open! Astring

type test = string CI_term.t

type project = {
  dashboards : CI_target.ID_Set.t;
  tests : CI_target.Full.t -> test String.Map.t;
}

type t = {
  web_config : CI_web_templates.t;
  projects : project CI_projectID.Map.t;
}

let id_of_branch name = `Ref ("heads/" ^ name)

let project ~id ?(dashboards=["master"]) tests =
  let id = CI_projectID.of_string_exn id in
  let tests x = String.Map.of_list (tests x) in
  let dashboards = CI_target.ID_Set.of_list (List.map id_of_branch dashboards) in
  id, {tests; dashboards}

let ci ~web_config ~projects =
  let projects = CI_projectID.Map.of_list projects in
  { web_config; projects }
