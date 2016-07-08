(* Implement API with direct GitHub API calls. *)

open Vgithub
open Github_t

type token = Github.Token.t

let run x = Lwt_main.run (Github.Monad.run x)

module PR = struct

  include PR

  let of_gh pr = {
    number = pr.pull_number;
    state  = pr.pull_state;
    head   = pr.pull_head.branch_sha;
  }

  let of_event pr = {
    number = pr.pull_request_event_number;
    state  = pr.pull_request_event_pull_request.pull_state;
    head   = pr.pull_request_event_pull_request.pull_head.branch_sha;
  }

end

module Status = struct

  include Status

  let of_gh commit s = {
    context     = s.status_context;
    url         = s.status_target_url;
    description = s.status_description;
    state       = s.status_state;
    commit;
  }

  let to_gh s = {
    new_status_context     = s.context;
    new_status_target_url  = s.url;
    new_status_description = s.description;
    new_status_state       = s.state;
  }

  let of_event s = {
    context     = s.status_event_context;
    url         = s.status_event_target_url;
    description = s.status_event_description;
    state       = s.status_event_state;
    commit      = s.status_event_sha;
  }

end

module Event = struct

  include Event

  let of_gh e = match e.event_payload with
    | `Status s       -> Status (Status.of_event s)
    | `PullRequest pr -> PR (PR.of_event pr)
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

let user_exists token ~user =
  try
    Github.User.info ~token ~user ()
    |> run
    |> fun _ -> true
  with Github.Message _ ->
    false

let repo_exists token ~user ~repo =
  try
    Github.Repo.info ~token ~user ~repo ()
    |> run
    |> fun _ -> true
  with Github.Message _ ->
    false

let repos token ~user =
  Github.User.repositories ~token ~user ()
  |> Github.Stream.to_list
  |> run
  |> List.map (fun r -> r.repository_name)

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
  |> list_dedup (fun s -> s.status_context)
  |> List.map (Status.of_gh commit)

let set_status token ~user ~repo status =
  let new_status = Status.to_gh status in
  Github.Status.create ~token ~user ~repo ~sha:status.Status.commit
    ~status:new_status ()
  |> run
  |> ignore

let prs token ~user ~repo =
  Github.Pull.for_repo ~token ~state:`Open ~user ~repo ()
  |> Github.Stream.to_list
  |> run
  |> List.map PR.of_gh

let events token ~user ~repo =
  let open Lwt.Infix in
  let events = Github.Event.for_repo ~token ~user ~repo () in
  Github.Monad.run @@ Github.Stream.to_list events >>= fun events ->
  Lwt_list.map_p (fun e -> Lwt.return (Event.of_gh e)) events
