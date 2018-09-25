open Rresult
open Datakit_github
open Github_t
open Astring
open Lwt.Infix

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

type 'a result = ('a, string) Result.result Lwt.t

type token = {
  m    : unit Github.Monad.t;
  token: Github.Token.t;
}

let token ~user_agent ~token =
  let open Github.Monad in
  let m =
    Github.API.set_token token >>= fun () ->
    Github.API.set_user_agent user_agent
  in
  { m; token }

let (>>+=) = Github.Monad.(>>=)
let (>>+~) = Github.Monad.(>>~)

type t = Run: ('a Github.Monad.t * (('a, exn) Result.result -> unit)) -> t

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
    Github.Monad.embed (wait ()) >>+= fun (Run (x, send)) ->
    let worker () =
      Lwt.catch
        (fun () -> Github.(Monad.run @@ x >|= fun x -> send (Ok x)))
        (fun e  -> send (Error e); Lwt.return_unit)
    in
    Lwt.async worker;
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
  t >|= function
  | Ok x    -> Ok x
  | Error e ->
    let err = Fmt.strf "Github: %a" Fmt.exn e in
    Error err

module PR = struct

  include PR

  let comments ~token repo num =
    Log.debug (fun l -> l "API.PR.comments %a/pr/%d" Repo.pp repo num);
    let comments =
      token.m >>+= fun () ->
      let user = repo.Repo.user.User.name in
      let repo = repo.Repo.repo in
      let commits = Github.Issue.comments ~user ~repo ~num () in
      Github.Stream.to_list commits >>+= fun comments ->
      Github.Monad.return comments
    in
    Github.Monad.run comments >|= fun comments ->
    List.map (fun c ->
        let id = Int64.to_int c.issue_comment_id in
        let body = c.issue_comment_body in
        let user = User.v c.issue_comment_user.user_login in
        Comment.v ~id ~user ~body
      ) comments
    |> fun comments ->
    Log.debug (fun l -> l "XXX %a" Fmt.(Dump.list Comment.pp) comments);
    comments

  let of_gh ~token repo pr =
    let head = Commit.v repo pr.pull_head.branch_sha in
    let state = pr.pull_state in
    let title = pr.pull_title in
    let base = pr.pull_base.branch_ref in
    let owner = User.v pr.pull_user.user_login in
    let num = pr.pull_number in
    let body = Comment.v ~id:0 ~user:owner ~body:pr.pull_body in
    comments ~token repo num >|= fun comments ->
    let comments = Array.of_list (body :: comments) in
    PR.v ~state ~title ~base ~owner ~comments head num

  let to_gh pr = {
    update_pull_title = Some pr.title;
    update_pull_body  = None;
    update_pull_state = Some pr.state;
    update_pull_base  = Some pr.base;
  }

  let of_event ~token repo pr =
    let id = pr.pull_request_event_pull_request.pull_head.branch_sha in
    let head = Commit.v repo id in
    let state = pr.pull_request_event_pull_request.pull_state in
    let title = pr.pull_request_event_pull_request.pull_title in
    let base = pr.pull_request_event_pull_request.pull_base.branch_ref in
    let number = pr.pull_request_event_number in
    let owner =
      User.v pr.pull_request_event_pull_request.pull_user.user_login
    in
    comments ~token repo number >|= fun comments ->
    let body =
      let body = pr.pull_request_event_pull_request.pull_body in
      Comment.v ~id:0 ~user:owner ~body
    in
    let comments = Array.of_list (body :: comments) in
    Event.PR (PR.v ~state ~title ~base ~owner ~comments head number)

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
    | `Unknown s -> failwith ("unknown: " ^ s)
    | #Status_state.t as s -> s

  let to_gh_state s = (s :> Github_t.status_state)

  let map f = function None -> None | Some x -> Some (f x)

  let of_gh commit s =
    let description = s.base_status_description in
    let url = map Uri.of_string s.base_status_target_url in
    let context = to_list s.base_status_context in
    let state = of_gh_state s.base_status_state in
    Status.v ?description ?url commit context state

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
      new_status_target_url  = map Uri.to_string (Status.url s);
      new_status_description = Status.description s;
      new_status_state       = to_gh_state (Status.state s);
    }

  let of_event repo s =
    let commit = Commit.v repo s.status_event_sha in
    let description = s.status_event_description in
    let url = map Uri.of_string s.status_event_target_url in
    let context = to_list s.status_event_context in
    let state = of_gh_state s.status_event_state in
    Status.v ?description ?url commit context state

end

module Ref = struct

  include Ref

  let to_list s = match String.cuts ~empty:false ~sep:"/" s with
    | "refs" :: l | l -> l

  let commit_ref_of_tag ~token ~repo:{ Repo.user; repo } r =
    assert (r.git_ref_obj.obj_ty = `Tag);
    let sha = r.git_ref_obj.obj_sha in
    let user = User.name user in
    let t =
      token.m >>+= fun () ->
      Github.Repo.get_tag ~user ~repo ~sha () >>+~ fun t ->
      (* FIXME: do we care about tags pointing to tags ?*)
      Github.Monad.return { r with git_ref_obj = t.tag_obj }
    in
    run t

  let to_commit_ref ~token ~repo r =
    match r.Github_t.git_ref_obj.obj_ty with
    | `Blob
    | `Tree   -> Lwt.return_none
    | `Commit -> Lwt.return (Some r)
    | `Tag    ->
      commit_ref_of_tag ~token ~repo r >|= function
      | Ok r    -> Some r
      | Error _ -> None

  let of_gh_commit_ref ~repo r =
    assert (r.git_ref_obj.obj_ty = `Commit);
    let head = Commit.v repo r.git_ref_obj.obj_sha in
    Ref.v head (to_list r.git_ref_name)

  let of_gh ~token ~repo r =
    to_commit_ref ~token ~repo r >|= function
    | Some r -> Some (of_gh_commit_ref ~repo r)
    | None   -> None

  let of_event_hook repo r =
    let name = to_list r.push_event_hook_ref in
    match r.push_event_hook_head_commit with
    | Some c ->
      let id = c.push_event_hook_commit_id in
      let head = Commit.v repo id in
      let t = Ref.v head name in
      if r.push_event_hook_created then `Created t else `Updated t
    | None ->
      `Removed (repo, name)

  let of_event repo r =
    let id = r.push_event_head in
    let head = Commit.v repo id in
    let t = Ref.v head (to_list r.push_event_ref) in
    `Updated t

  let of_deleted_event repo r =
    match r.delete_event_ref with
    | `Repository -> Event.Other (repo, "repo-delete")
    | `Branch s   -> Event.Ref (`Removed (repo, ["refs"; "heads"; s]))
    | `Tag s      -> Event.Ref (`Removed (repo, ["refs"; "tags"; s]))

  let ref_of_name ~token ~repo:{ Repo.user; repo } name =
    let user = User.name user in
    (token.m >>+= fun () ->
     Github.Repo.get_ref ~user ~repo ~name () >>+~ fun x ->
     Github.Monad.return x)
    |> run
    >|= function
    | Ok x    -> Some x
    | Error _ -> None

  let of_created_event ~token repo r =
    let other str = Event.Other (repo, "create-" ^ str) in
    match r.create_event_ref with
    | `Repository -> Lwt.return (Event.Other (repo, "repo"))
    | `Branch b   ->
      (ref_of_name ~token ~repo ("heads/" ^ b) >>= function
        | None   -> Lwt.return (other @@ "branch-unknown-" ^ b)
        | Some e ->
          of_gh ~token ~repo e >|= function
          | None   -> other ("branch-unknownx-" ^ b)
          | Some e -> Event.Ref (`Created e))
    | `Tag t ->
      ref_of_name ~token ~repo ("tags/" ^ t) >>= function
      | None   -> Lwt.return (other @@ "tag-unknonw-" ^ t)
      | Some e ->
        of_gh ~token ~repo e >|= function
        | None   -> other ("tag-unknonwx-" ^ t)
        | Some e -> Event.Ref (`Created e)

end

module Event = struct

  include Event

  let of_gh_constr ~token repo (e:Github_t.event_constr): t Lwt.t =
    let other str = Lwt.return (Other (repo, str)) in
    let ret = Lwt.return in
    match e with
    | `Status s       -> ret @@ Status (Status.of_event repo s)
    | `PullRequest pr -> PR.of_event ~token repo pr
    | `Push p         -> ret @@ Ref (Ref.of_event repo p)
    | `Create c       -> Ref.of_created_event ~token repo c
    | `Delete d       -> ret @@ Ref.of_deleted_event repo d
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

  let of_gh_hook_constr ~token repo (e:Github_t.event_hook_constr): t Lwt.t =
    let other str = Lwt.return (Other (repo, str)) in
    let ret = Lwt.return in
    match e with
    | `Status s       -> ret @@ Status (Status.of_event repo s)
    | `PullRequest pr -> PR.of_event ~token repo pr
    | `Push p         -> ret @@ Ref (Ref.of_event_hook repo p)
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

  let of_gh ~token e =
    let repo = match String.cut ~sep:"/" e.event_repo.repo_name with
      | None  -> failwith (e.event_repo.repo_name ^ " is not a valid repo name")
      | Some (user, repo) ->
        let user = User.v user in
        Repo.v ~user ~repo
    in
    of_gh_constr ~token repo e.event_payload

end

let event_hook_constr = Event.of_gh_hook_constr

let user_exists token ~user =
  let user = User.name user in
  try
    (token.m >>+= fun () -> Github.User.info ~user ())
    |> run
    >|= R.map (fun _ -> true)
  with Github.Message _ ->
    Lwt.return (Ok false)

let repo_exists token { Repo.user; repo } =
  let user = User.name user in
  try
    (token.m >>+= fun () -> Github.Repo.info ~user ~repo ())
    |> run
    >|= R.map (fun _ -> true)
  with Github.Message _ ->
    Lwt.return (Ok false)

let repos token ~user =
  let user = User.name user in
  (token.m >>+= fun () ->
   Github.User.repositories ~user ()
   |> Github.Stream.to_list
   |> Github.Monad.map
   @@ List.map (fun r ->
       let user = User.v user in
       Repo.v ~user ~repo:r.repository_name))
  |> run

let user_repo c = c.Commit.repo.Repo.user, c.Commit.repo.Repo.repo

let status token commit =
  let user, repo = user_repo commit in
  let sha = Commit.hash commit in
  let user = User.name user in
  (token.m >>+= fun () ->
   Github.Status.get ~user ~repo ~sha ()
   |> Github.Monad.map Github.Response.value)
  |> run
  >|= R.map (fun r ->
      List.map (Status.of_gh commit) r.Github_t.combined_status_statuses
    )

let set_status token status =
  let new_status = Status.to_gh status in
  let user, repo = user_repo (Status.commit status) in
  let user = User.name user in
  let sha = Status.commit_hash status in
  (token.m >>+= fun () ->
   Github.Status.create ~user ~repo ~sha ~status:new_status ())
  |> run
  >|= R.map ignore

let user_repo pr = user_repo (PR.commit pr)

let set_pr token pr =
  let new_pr = PR.to_gh pr in
  let user, repo = user_repo pr in
  let user = User.name user in
  let num = PR.number pr in
  (token.m >>+= fun () ->
   Github.Pull.update ~user ~repo ~num ~update_pull:new_pr ())
  |> run
  >|= R.map ignore

let not_implemented () = Lwt.fail_with "not implemented"
let set_ref _ _ = not_implemented ()
let remove_ref _ _ _ = not_implemented ()

let prs token r =
  let { Repo.user; repo } = r in
  let user = User.name user in
  let of_gh x = Github.Monad.embed (PR.of_gh ~token r x >|= fun x -> [x]) in
  (token.m >>+= fun () ->
   Github.Pull.for_repo ~state:`Open ~user ~repo ()
   |> Github.Stream.map of_gh
   |> Github.Stream.to_list)
  |> run

let pr token (r, num) =
  let { Repo.user; repo } = r in
  let user = User.name user in
  (token.m >>+= fun () ->
   Github.Pull.get ~user ~repo ~num () >>+~ fun pr ->
  Github.Monad.embed (PR.of_gh ~token r pr))
  |> run >|= function
  | Error _ -> Ok None
  | Ok pr   -> Ok (Some pr)

let refs token r =
  let { Repo.user; repo } = r in
  let user = User.name user in
  let refs ty =
    (token.m >>+= fun () ->
     Github.Repo.refs ~ty ~user ~repo ()
    |> Github.Stream.to_list)
    |> run
    >>= function
    | Error e ->
      Log.err (fun l -> l "error while reading refs for %s/%s: %s" user repo e);
      Lwt.return_nil
    | Ok git_refs ->
      Lwt_list.fold_left_s (fun acc ref ->
        Ref.of_gh ~token ~repo:r ref >|= function
        | None   -> acc
        | Some r -> r :: acc
        ) [] git_refs
  in
  refs "heads" >>= fun heads ->
  refs "tags"  >|= fun tags  ->
  Ok (heads @ tags)

let ref token (r, name) =
  let name = String.concat ~sep:"/" name in
  let { Repo.user; repo } = r in
  let user = User.name user in
  (token.m >>+= fun () ->
   Github.Repo.get_ref ~user ~repo ~name () >>+~ fun r ->
   Github.Monad.return r)
  |> run >>= function
  | Error _ -> Lwt.return (Ok None)
  | Ok ref  -> Ref.of_gh ~token ~repo:r ref >|= fun r -> Ok r

let events token r =
  let { Repo.user; repo } = r in
  let user = User.name user in
  (token.m >>+= fun () ->
   let events = Github.Event.for_repo ~user ~repo () in
   Github.Stream.to_list events >>+= fun events ->
   Github.Monad.embed @@ Lwt_list.map_p (Event.of_gh ~token) events)
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

  type t = {
    token: token;
    hook : Hook.t;
  }

  let v token uri = { token; hook = Hook.create token.token uri }
  let to_repo (user, repo) = Repo.v ~user:(User.v user) ~repo
  let of_repo { Repo.user; repo } = User.name user, repo

  let events t =
    Lwt_list.map_p (fun (r, e) ->
        event_hook_constr ~token:t.token (to_repo r) e
      ) (Hook.events t.hook)

  let repos t =
    Hook.repos t.hook
    |> Github_hooks.Repo.Set.elements
    |> List.map to_repo
    |> Repo.Set.of_list

  let default_events = [
    `Create; `Delete; `Push; (* ref updates *)
    `Status;                 (* status updates *)
    `PullRequest;            (* PR updates *)
  ]

  let watch { hook; _ } r = Hook.watch hook ~events:default_events (of_repo r)
  let wait { hook; _ } = Hook.wait hook
  let clear { hook; _ } = Hook.clear hook
  let run { hook; _ } = Hook.run hook
end
