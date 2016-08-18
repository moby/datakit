(* Implement API with direct GitHub API calls. *)

open Datakit_github
open Github_t
open Astring

type token = Github.Token.t

let run x = Github.Monad.run x

module PR = struct

  include PR

  let of_gh ~user ~repo pr = {
    user; repo;
    number = pr.pull_number;
    state  = pr.pull_state;
    head   = pr.pull_head.branch_sha;
    title  = pr.pull_title;
  }

  let to_gh pr = {
    update_pull_title = Some pr.title;
    update_pull_body  = None;
    update_pull_state = Some pr.state;
  }

  let of_event ~user ~repo pr = {
    user; repo;
    number = pr.pull_request_event_number;
    state  = pr.pull_request_event_pull_request.pull_state;
    head   = pr.pull_request_event_pull_request.pull_head.branch_sha;
    title  = pr.pull_request_event_pull_request.pull_title;
  }

end

module Status = struct

  include Status

  let to_list = function
    | None   -> ["default"]
    | Some c -> String.cuts ~empty:false ~sep:"/" c

  let of_list = function
    | ["default"] -> None
    | l           -> Some (String.concat ~sep:"/" l)

  let of_gh ~user ~repo ~commit s = {
    user; repo; commit;
    context     = to_list s.status_context;
    url         = s.status_target_url;
    description = s.status_description;
    state       = s.status_state;
  }

  let to_gh s = {
    new_status_context     = of_list s.context;
    new_status_target_url  = s.url;
    new_status_description = s.description;
    new_status_state       = s.state;
  }

  let of_event ~user ~repo s = {
    user; repo;
    context     = to_list s.status_event_context;
    url         = s.status_event_target_url;
    description = s.status_event_description;
    state       = s.status_event_state;
    commit      = s.status_event_sha;
  }

end

module Event = struct

  include Event

  let of_gh e =
    let user, repo = match String.cut ~sep:"/" e.event_repo.repo_name with
      | None -> failwith (e.event_repo.repo_name ^ " is not a valid repo name")
      | Some r -> r
    in
    match e.event_payload with
    | `Status s       -> Status (Status.of_event ~user ~repo s)
    | `PullRequest pr -> PR (PR.of_event ~user ~repo pr)
    | `Create _       -> Other "create"
    | `Delete _       -> Other "delete"
    | `Download       -> Other "download"
    | `Follow         -> Other "follow"
    | `Fork _         -> Other "fork"
    | `ForkApply      -> Other "fork-apply"
    | `Gist           -> Other "gist"
    | `Gollum _       -> Other "gollum"
    | `IssueComment _ -> Other "issue-comment"
    | `Issues _       -> Other "issues"
    | `Member _       -> Other "member"
    | `Public         -> Other "public"
    | `Push _         -> Other "push"
    | `Release _      -> Other "release"
    | `Watch _        -> Other "watch"
    | `PullRequestReviewComment _ -> Other "pull-request-review-comment"
    | `CommitComment _            -> Other "commit-comment"

end

open Lwt.Infix

let user_exists token ~user =
  try
    Github.User.info ~token ~user ()
    |> run
    >|= fun _ -> true
  with Github.Message _ ->
    Lwt.return_false

let repo_exists token ~user ~repo =
  try
    Github.Repo.info ~token ~user ~repo ()
    |> run
    >|= fun _ -> true
  with Github.Message _ ->
    Lwt.return_false

let repos token ~user =
  Github.User.repositories ~token ~user ()
  |> Github.Stream.to_list
  |> run
  >|= List.map (fun r -> r.repository_name)

let list_dedup f l =
  let tbl = Hashtbl.create (List.length l) in
  List.fold_left (fun acc s ->
      let x = f s in
      try let () = Hashtbl.find tbl x in acc
      with Not_found -> Hashtbl.add tbl x (); s :: acc
    ) [] l
  |> List.rev

let status token ~user ~repo ~commit =
  Github.Status.for_ref ~token ~user ~repo ~git_ref:commit ()
  |> Github.Stream.to_list
  |> run
  >|= fun l -> list_dedup (fun s -> s.status_context) l
  |> List.map (Status.of_gh ~user ~repo ~commit)

let set_status token status =
  let new_status = Status.to_gh status in
  let { Status.user; repo; commit; _ } = status in
  Github.Status.create ~token ~user ~repo ~sha:commit ~status:new_status ()
  |> run
  >|= ignore

let set_pr token pr =
  let new_pr = PR.to_gh pr in
  let { PR.user; repo; number; _ } = pr in
  Github.Pull.update ~token ~user ~repo ~num:number ~update_pull:new_pr ()
  |> run
  >|= ignore

let prs token ~user ~repo =
  Github.Pull.for_repo ~token ~state:`Open ~user ~repo ()
  |> Github.Stream.to_list
  |> run
  >|= List.map (PR.of_gh ~user ~repo)

let events token ~user ~repo =
  let open Lwt.Infix in
  let events = Github.Event.for_repo ~token ~user ~repo () in
  Github.Monad.run @@ Github.Stream.to_list events >>= fun events ->
  Lwt_list.rev_map_p (fun e -> Lwt.return (Event.of_gh e)) events
