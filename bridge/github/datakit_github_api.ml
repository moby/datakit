open Datakit_github
open Github_t
open Astring

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

type token = Github.Token.t

module PR = struct

  include PR

  let of_gh repo pr =
    let head = Commit.create repo pr.pull_head.branch_sha in
    { head;
      number = pr.pull_number;
      state  = pr.pull_state;
      title  = pr.pull_title;
      base   = pr.pull_base.branch_ref;
    }

  let to_gh pr = {
    update_pull_title = Some pr.title;
    update_pull_body  = None;
    update_pull_state = Some pr.state;
    update_pull_base  = Some pr.base;
  }

  let of_event repo pr =
    let id = pr.pull_request_event_pull_request.pull_head.branch_sha in
    let head = Commit.create repo id in
    {
      head;
      number = pr.pull_request_event_number;
      state  = pr.pull_request_event_pull_request.pull_state;
      title  = pr.pull_request_event_pull_request.pull_title;
      base   = pr.pull_request_event_pull_request.pull_base.branch_ref;
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

  let of_gh_state = function
    | `Unknown (s, _) -> failwith ("unknown: " ^ s)
    | #Status_state.t as s -> s

  let to_gh_state s = (s :> Github_t.status_state)

  let of_gh commit s =
    let description = s.base_status_description in
    let url = s.base_status_target_url in
    let context = to_list s.base_status_context in
    let state = of_gh_state s.base_status_state in
    Status.create ?description ?url commit context state

  (* To avoid:
     Github: GitHub API error: 422 Unprocessable Entity (WebDAV) (RFC 4918)
       -- Validation Failed
     Resource type: Status
     Field: description
     Code: custom
     Message: description is too long (maximum is 140 characters) *)
  let assert_short_description = function
    | None   -> ()
    | Some s -> assert (String.length s <= 140)

  let to_gh s =
    assert_short_description (Status.description s);
    {
      new_status_context     = of_list (Status.context s);
      new_status_target_url  = Status.url s;
      new_status_description = Status.description s;
      new_status_state       = to_gh_state (Status.state s);
    }

  let of_event repo s =
    let commit = Commit.create repo s.status_event_sha in
    let description = s.status_event_description in
    let url = s.status_event_target_url in
    let context = to_list s.status_event_context in
    let state = of_gh_state s.status_event_state in
    Status.create ?description ?url commit context state

end

module Ref = struct

  include Ref

  let to_list s = match String.cuts ~empty:false ~sep:"/" s with
    | "refs" :: l | l -> l

  let commit_ref_of_tag ~token ~repo:{ Repo.user; repo } r =
    let open Github.Monad in
    assert (r.git_ref_obj.obj_ty = `Tag);
    let sha = r.git_ref_obj.obj_sha in
    let t =
      Github.Repo.get_tag ~token ~user ~repo ~sha () >>~ fun t ->
      (* FIXME: do we care about tags pointing to tags ?*)
      Github.Monad.return { r with git_ref_obj = t.tag_obj }
    in
    Github.Monad.run t

  open Lwt.Infix

  let to_commit_ref ~token ~repo r =
    match r.Github_t.git_ref_obj.obj_ty with
    | `Blob
    | `Tree   -> Lwt.return_none
    | `Commit -> Lwt.return (Some r)
    | `Tag    -> commit_ref_of_tag ~token ~repo r >|= fun r -> Some r

  let of_gh_commit_ref ~repo r =
    assert (r.git_ref_obj.obj_ty = `Commit);
    let head = Commit.create repo r.git_ref_obj.obj_sha in
    Ref.create head (to_list r.git_ref_name)

  let of_gh ~token ~repo r =
    to_commit_ref ~token ~repo r >|= function
    | Some r -> Some (of_gh_commit_ref ~repo r)
    | None   -> None

  let of_event_hook repo r =
    let name = to_list r.push_event_hook_ref in
    match r.push_event_hook_head_commit with
    | Some c ->
      let id = c.push_event_hook_commit_id in
      let head = Commit.create repo id in
      let t = Ref.create head name in
      if r.push_event_hook_created then `Created t else `Updated t
    | None ->
      `Removed (repo, name)

  let of_event repo r =
    let id = r.push_event_head in
    let head = Commit.create repo id in
    let t = Ref.create head (to_list r.push_event_ref) in
    `Updated t

end

module Event = struct

  include Event

  let of_gh_constr repo (e:Github_t.event_constr): t =
    let other str = Other (repo, str) in
    match e with
    | `Status s       -> Status (Status.of_event repo s)
    | `PullRequest pr -> PR (PR.of_event repo pr)
    | `Push p         -> Ref (Ref.of_event repo p)
    | `Create _       -> other "create"
    | `Delete _       -> other "delete"
    | `Download       -> other "download"
    | `Follow         -> other "follow"
    | `Fork _         -> other "fork"
    | `ForkApply      -> other "fork-apply"
    | `Gist           -> other "gist"
    | `Gollum _       -> other "gollum"
    | `IssueComment _ -> other "issue-comment"
    | `Issues _       -> other "issues"
    | `Member _       -> other "member"
    | `Public         -> other "public"
    | `Release _      -> other "release"
    | `Watch _        -> other "watch"
    | `Repository _   -> other "repository"
    | `Unknown (s, _) -> other ("unknown " ^ s)
    | `PullRequestReviewComment _ -> other "pull-request-review-comment"
    | `CommitComment _            -> other "commit-comment"

  let of_gh_hook_constr repo (e:Github_t.event_hook_constr): t =
    let other str = Other (repo, str) in
    match e with
    | `Status s       -> Status (Status.of_event repo s)
    | `PullRequest pr -> PR (PR.of_event repo pr)
    | `Push p         -> Ref (Ref.of_event_hook repo p)
    | `Create _       -> other "create"
    | `Delete _       -> other "delete"
    | `Download       -> other "download"
    | `Follow         -> other "follow"
    | `Fork _         -> other "fork"
    | `ForkApply      -> other "fork-apply"
    | `Gist           -> other "gist"
    | `Gollum _       -> other "gollum"
    | `IssueComment _ -> other "issue-comment"
    | `Issues _       -> other "issues"
    | `Member _       -> other "member"
    | `Public         -> other "public"
    | `Release _      -> other "release"
    | `Watch _        -> other "watch"
    | `Repository _   -> other "repository"
    | `Unknown (s, _) -> other ("unknown " ^ s)
    | `PullRequestReviewComment _ -> other "pull-request-review-comment"
    | `CommitComment _            -> other "commit-comment"

  let of_gh e =
    let repo = match String.cut ~sep:"/" e.event_repo.repo_name with
      | None  -> failwith (e.event_repo.repo_name ^ " is not a valid repo name")
      | Some (user, repo) -> Repo.create ~user ~repo
    in
    of_gh_constr repo e.event_payload

end

let event_hook_constr = Event.of_gh_hook_constr

open Rresult
open Lwt.Infix

type 'a result = ('a, string) Result.result Lwt.t

type t = Run: ('a Github.Monad.t * ('a -> unit)) -> t

module Run = struct

  let events = ref []
  let cond = Lwt_condition.create ()

  let enqueue e =
    events := e :: !events;
    Lwt_condition.signal cond ()

  let rec wait () =
    match List.rev !events with
    | []   -> Lwt_condition.wait cond >>= wait
    | h::t -> events := List.rev t; Lwt.return h

  let rec schedule () =
    let open Github.Monad in
    embed (wait ()) >>= fun (Run (x, send)) ->
    x >>= fun x ->
    send x;
    schedule ()

  let rec for_ever () =
    Lwt.catch
      (fun () -> Github.Monad.run @@ schedule ())
      (fun e  ->
         Log.err
           (fun l -> l "GitHub scheduler caught %a, restarting" Fmt.exn e);
         for_ever ())

end

let () = Lwt.async Run.for_ever

let run x =
  let t, u = Lwt.task () in
  Run.enqueue (Run (x, Lwt.wakeup u));
  Lwt.catch
    (fun () -> t >|= fun x -> Ok x)
    (fun e  -> Lwt.return (Error (Fmt.strf "Github: %a" Fmt.exn e)))

let user_exists token ~user =
  try
    Github.User.info ~token ~user ()
    |> run
    >|= R.map (fun _ -> true)
  with Github.Message _ ->
    Lwt.return (Ok false)

let repo_exists token { Repo.user; repo } =
  try
    Github.Repo.info ~token ~user ~repo ()
    |> run
    >|= R.map (fun _ -> true)
  with Github.Message _ ->
    Lwt.return (Ok false)

let repos token ~user =
  Github.User.repositories ~token ~user ()
  |> Github.Stream.to_list
  |> Github.Monad.map
  @@ List.map (fun r -> Repo.create ~user ~repo:r.repository_name)
  |> run

let user_repo c = c.Commit.repo.Repo.user, c.Commit.repo.Repo.repo

let status token commit =
  let user, repo = user_repo commit in
  let sha = Commit.id commit in
  Github.Status.get ~token ~user ~repo ~sha ()
  |> Github.Monad.map Github.Response.value
  |> run
  >|= R.map (fun r ->
      List.map (Status.of_gh commit) r.Github_t.combined_status_statuses
    )

let set_status token status =
  let new_status = Status.to_gh status in
  let user, repo = user_repo (Status.commit status) in
  let sha = Status.commit_id status in
  Github.Status.create ~token ~user ~repo ~sha ~status:new_status ()
  |> run
  >|= R.map ignore

let user_repo pr = user_repo (PR.commit pr)

let set_pr token pr =
  let new_pr = PR.to_gh pr in
  let user, repo = user_repo pr in
  let num = PR.number pr in
  Github.Pull.update ~token ~user ~repo ~num ~update_pull:new_pr ()
  |> run
  >|= R.map ignore

let not_implemented () = Lwt.fail_with "not implemented"
let set_ref _ _ = not_implemented ()
let remove_ref _ _ _ = not_implemented ()

let prs token r =
  let { Repo.user; repo } = r in
  Github.Pull.for_repo ~token ~state:`Open ~user ~repo ()
  |> Github.Stream.to_list
  |> Github.Monad.map @@ List.map (PR.of_gh r)
  |> run

let refs token r =
  let { Repo.user; repo } = r in
  let refs ty =
    Github.Repo.refs ~ty ~token ~user ~repo ()
    |>  Github.Stream.to_list
    |>  Github.Monad.run
    >>= Lwt_list.fold_left_s (fun acc ref ->
        Ref.of_gh ~token ~repo:r ref >|= function
        | None   -> acc
        | Some r -> r :: acc
      ) []
  in
  refs "heads" >>= fun heads ->
  refs "tags"  >|= fun tags  ->
  Ok (heads @ tags)

let events token r =
  let { Repo.user; repo } = r in
  let events = Github.Event.for_repo ~token ~user ~repo () in
  Github.Stream.to_list events
  |> Github.Monad.map (List.map Event.of_gh)
  |> run

module Webhook = struct
  module Conf = struct
    let src =
      Logs.Src.create "dkt-github-hooks" ~doc:"Github to Git bridge webhooks"
    module Log = (val Logs.src_log src : Logs.LOG)
    let secret_prefix = "datakit"
    let tls_config = None
  end

  module Hook = Github_hooks_unix.Make(Conf)

  include Hook

  let to_repo (user, repo) = Repo.create ~user ~repo
  let of_repo { Repo.user; repo } = user, repo

  let events t =
    List.map (fun (r, e) -> event_hook_constr (to_repo r) e) (events t)

  let repos t =
    repos t
    |> Github_hooks.Repo.Set.elements
    |> List.map to_repo
    |> Repo.Set.of_list

  let default_events = [
    `Create; `Delete; `Push; (* ref updates *)
    `Status;                 (* status updates *)
    `PullRequest;            (* PR updates *)
  ]

  let watch t r = watch t ~events:default_events (of_repo r)

end
