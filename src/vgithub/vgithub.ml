open Result
open Astring

let src = Logs.Src.create "vgithub" ~doc:"Virtual Github API"
module Log = (val Logs.src_log src : Logs.LOG)

let run x = Lwt_main.run (Github.Monad.run x)

let err_invalid_status s = Vfs.error "%S: invalid status" s

module Status_state = struct

    type t = [ `Error | `Pending | `Success | `Failure ]

    let to_string = function
    | `Error   -> "error"
    | `Failure -> "failure"
    | `Pending -> "pending"
    | `Success -> "success"

  let pp =  Fmt.of_to_string to_string

  let of_string = function
    | "error"   -> Some `Error
    | "failure" -> Some `Failure
    | "pending" -> Some `Pending
    | "success" -> Some `Success
    | _         -> None

end

module PR = struct

  open Github_t

  type t = {
    number: int;
    state: [`Open | `Closed];
    head: string; (* SHA1 *)
  }

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

  let pp_state ppf = function
    | `Open   -> Fmt.string ppf "open"
    | `Closed -> Fmt.string ppf "closed"

  let pp ppf t =
    Fmt.pf ppf "[number: %d, state: %a, head: %s]"
      t.number pp_state t.state t.head

end

module Status = struct

  open Github_t

  type t = {
    context: string option;
    url: string option;
    description: string option;
    state: Status_state.t;
    commit: string;
  }

  let path t = match t.context with
    | None   -> ["default"]
    | Some c -> String.cuts ~empty:false ~sep:"/" c

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

  let pp ppf t =
    let pp_opt k ppf v = match v with
      | None   -> ()
      | Some v -> Fmt.pf ppf "%s: %s, " k v
    in
    Fmt.pf ppf "[%a%a%a state: %a]"
      (pp_opt "context") t.context
      (pp_opt "url") t.url
      (pp_opt "description") t.description
      Status_state.pp t.state

end

module Event = struct

  open Github_t

  type t =
    | PR of PR.t
    | Status of Status.t
    | Other of string

  let pp ppf = function
    | PR pr    -> Fmt.pf ppf "PR: %a" PR.pp pr
    | Status s -> Fmt.pf ppf "Status: %a" Status.pp s
    | Other s  -> Fmt.pf ppf "Other: %s" s

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

module type API = sig
  type token
  val user_exists: token -> user:string -> bool
  val repo_exists: token -> user:string -> repo:string -> bool
  val repos: token -> user:string -> string list
  val status: token -> user:string -> repo:string -> commit:string ->
    Status.t list
  val set_status: token -> user:string -> repo:string -> Status.t -> unit
  val prs: token -> user:string -> repo:string -> PR.t list
  val events: token -> user:string -> repo:string -> Event.t list Lwt.t
end

module Make (API: API) = struct

  type t = {
    token: API.token;
    user: string;
    repo: string;
  }

  (* /github.com/${USER}/${REPO}/commit/${SHA1}/status/${S} *)
  let commit_status_dir t ?(extra_dirs=fun () -> []) s =
    Logs.debug (fun l ->
        l "status_file %s/%s %a" t.user t.repo Status_state.pp s.Status.state
      );
    let current_descr = ref None in
    let current_url = ref None in
    let current_state = ref s.Status.state in
    let init = Status_state.to_string s.Status.state ^ "\n" in
    let set_status () =
      let state = !current_state in
      let description = !current_descr in
      let url = !current_url in
      let new_status = { s with Status.description; url; state } in
      API.set_status t.token ~user:t.user ~repo:t.repo new_status;
    in
    let ctl = Vfs.File.command ~init (function
        | "update" -> set_status (); Vfs.ok ""
        | s        -> Vfs.error "%s is not a valid command" s
      ) in
    let state = Vfs.File.command ~init (fun str ->
        match Status_state.of_string str with
        | None   -> err_invalid_status str
        | Some s ->
          if s = !current_state then Vfs.ok (str ^ "\n")
          else (
            current_state := s;
            Vfs.ok (Status_state.to_string s ^ "\n");
          )
      ) in
    let descr = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_descr then Vfs.ok (str ^ "\n")
        else (
          current_descr := Some str;
          Vfs.ok (str ^ "\n")
        )
      ) in
    let url = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_url then Vfs.ok (str ^ "\n")
        else (
          current_url := Some str;
          Vfs.ok (str ^ "\n")
        )
      ) in
    let dir = [
      Vfs.Inode.file "state"  state;
      Vfs.Inode.file "descr"  descr;
      Vfs.Inode.file "url"    url;
      Vfs.Inode.file "ctl"    ctl;
    ] in
    Vfs.Dir.of_list (fun () -> dir @ extra_dirs ())

  let rec compare_context x y =
    match x, y with
    | [], [] -> 0
    | [], _  -> -1
    | _ , [] -> 1
    | h1::t1, h2::t2 ->
      match String.compare h1 h2 with
      | 0 -> compare_context t1 t2
      | i -> i

  let sort_by_hd childs =
    let childs = List.filter (fun (p, _) -> p <> []) childs in
    let compare_child (c1, _) (c2, _) = compare_context c1 c2 in
    let childs = List.sort compare_child childs in
    let rec aux (root, current, acc) = function
      | [] -> List.rev @@ (root, List.rev current) :: acc
      | ([]  , _)::_ -> assert false
      | (r::p, s)::t ->
        if r = root then
          let current = (p, s) :: current in
          aux (root, current, acc) t
        else
          let acc = (root, List.rev current) :: acc in
          let current = [ (p, s) ] in
          let root = r in
          aux (root, current, acc) t
    in
    match childs with
    | []           -> []
    | ([],_):: _   -> assert false
    | (r::p, s)::t -> aux (r, [ (p, s) ], []) t

  (* /github.com/${USER}/${REPO}/commit/${SHA1}/status *)
  let commit_status_dir t commit =
    Log.debug (fun l -> l "commit_status_dir %s/%s %s" t.user t.repo commit);
    let rec inodes childs =
      let root_status =
        try Some (List.find (fun (p, _) -> p = []) childs |> snd)
        with Not_found -> None
      in
      let childs = sort_by_hd childs in
      let childs () =
        List.map (fun (n, childs) -> Vfs.Inode.dir n @@ inodes childs) childs
      in
      match root_status with
      | None   -> Vfs.Dir.of_list childs
      | Some s -> commit_status_dir t ~extra_dirs:childs s
    in
    let ls () =
      API.status t.token ~user:t.user ~repo:t.repo ~commit
      |> List.map (fun s -> Status.path s, s)
      |> sort_by_hd
      |> List.map (fun (name, childs) -> Vfs.Inode.dir name @@ inodes childs)
      |> Vfs.ok
    in
    let lookup name =
      try
        API.status t.token ~user:t.user ~repo:t.repo ~commit
        |> List.map (fun s -> Status.path s, s)
        |> List.find_all (fun (c, _) -> List.hd c = name)
        |> List.map (fun (c, s) -> List.tl c, s)
        |> inodes
        |> Vfs.Inode.dir name
        |> Vfs.ok
      with Not_found ->
        Vfs.File.err_no_entry
    in
    let mkdir name =
      let new_status = {
        Status.context = Some name;
        url = None;
        description = None;
        state = `Pending;
        commit;
      } in
      API.set_status t.token ~user:t.user ~repo:t.repo new_status;
      Vfs.ok @@ Vfs.Inode.dir name @@ commit_status_dir t new_status
    in
    let mkfile _ _ = Vfs.error "TODO" in
    let remove _ = Vfs.error "TODO" in
    let rename _ _ = Vfs.error "TODO" in
    Vfs.Dir.create ~ls ~lookup ~mkfile ~mkdir ~remove ~rename

  let commit_root t =
    Logs.debug (fun l -> l "commit_root %s%s" t.user t.repo);
    let ls () = Vfs.ok [] in
    let lookup commit =
      let status = Vfs.Inode.dir "status" @@ commit_status_dir t commit in
      Vfs.Inode.dir commit @@ Vfs.Dir.of_list (fun () -> [status])
      |> Vfs.ok
    in
    let mkdir commit = (* TODO *) lookup commit in
    let remove () = Vfs.error "Cannot remove commits" in
    let rename _ _ = Vfs.error "Cannot rename commits" in
    Vfs.Dir.dir_only ~ls ~lookup ~mkdir ~remove ~rename

  (* /github.com/${USER}/${REPO}/pr/${PR}/head *)
  let pr_head t pr =
    Logs.debug (fun l ->
        l "pr_dir %s/%s %d" t.user t.repo pr.PR.number);
    let file, _ = Vfs.File.rw_of_string (pr.PR.head ^ "\n") in
    file

  (* /github.com/${USER}/${REPO}/pr/${PR} *)
  let pr_dir t pr =
    Logs.debug (fun l ->
        l "pr_dir %s/%s %d" t.user t.repo pr.PR.number);
    let dirs () = [
      Vfs.Inode.file "head"  @@ pr_head t pr;
    ] in
    Vfs.Dir.of_list dirs

  (* /github.com/${USER}/${REPO}/pr *)
  let pr_root t =
    Logs.debug (fun l -> l "pr_root %s/%s" t.user t.repo);
    let prs () =
      let prs = API.prs t.token ~user:t.user ~repo:t.repo in
      List.map (fun pr ->
          Vfs.Inode.dir (string_of_int pr.PR.number) @@ pr_dir t pr
        ) prs
    in
    Vfs.Dir.of_list prs

  (* /github.com/${USER}/${REPO}/events *)
  let repo_events t =
    let open Lwt.Infix in
    Logs.debug (fun l -> l "repo_events %s/%s" t.user t.repo);
    let f () =
      let buf = Buffer.create 1024 in
      let ppf = Format.formatter_of_buffer buf in
      API.events t.token ~user:t.user ~repo:t.repo >|= fun events ->
      List.iter (Fmt.pf ppf "%a\n" Event.pp) events;
      Buffer.contents buf
    in
    Vfs.File.status ~length:0 f

  (* /github.com/${USER}/${REPO} *)
  let repo_dir t =
    Logs.debug (fun l -> l "repo_root %s/%s" t.user t.repo);
    if not (API.repo_exists t.token ~user:t.user ~repo:t.repo) then None
    else
      let files = [
        Vfs.Inode.file "events" @@ repo_events t;
        Vfs.Inode.dir  "pr"     @@ pr_root t;
        Vfs.Inode.dir  "commit" @@ commit_root t;
      ] in
      let dir = Vfs.Dir.of_list (fun () -> files) in
      Some (Vfs.Inode.dir t.repo dir)

  (* /github.com/${USER}/ *)
  let user_dir ~token ~user =
    Logs.debug (fun l -> l "user_root %s/" user);
    if not (API.user_exists token ~user) then Vfs.Dir.err_no_entry
    else
      let ls () =
        API.repos token ~user
        |> List.rev_map (fun repo -> repo_dir { token; user; repo })
        |> List.fold_left (fun acc -> function
            | None   -> acc
            | Some x -> x :: acc)
          []
        |> Vfs.ok
      in
      let remove _ = Vfs.Dir.err_read_only in
      let lookup repo = match repo_dir { token; user; repo } with
        | None   -> Vfs.Dir.err_no_entry
        | Some x -> Vfs.ok x
      in
      let dir = Vfs.Dir.read_only ~ls ~remove ~lookup in
      Vfs.ok (Vfs.Inode.dir user dir)

  (* /github.com/ *)
  let create token =
    let ls () = Vfs.ok [] in
    let remove () = Vfs.Dir.err_read_only in
    let lookup name = user_dir ~token ~user:name in
    Vfs.Inode.dir "github.com" @@ Vfs.Dir.read_only ~ls ~remove ~lookup

end

module Sync (API: API) (DK: Datakit_S.CLIENT) = struct

  open Lwt.Infix

  let (/) = Datakit_path.(/)
  let (/@) = Datakit_path.(/@)

  let ( >>*= ) x f =
    x >>= function
    | Ok x -> f x
    | Error _ as e -> Lwt.return e

  let ok x = Lwt.return (Ok x)

  (* FIXME: should be simply DK.Tree.t and/or a Tree ID so that it
     should be much more efficient. *)
  type tree = Cstruct.t Datakit_path.Map.t

  type hook = { user: string; repo: string; tree: tree }

  let compare_user_repo x y = compare (x.user, x.repo) (y.user, y.repo)

  let compare_hook x y =
    match compare_user_repo x y with
    | 0 -> Datakit_path.Map.compare Cstruct.compare x.tree y.tree
    | i -> i

  let pp_hook ppf t = Fmt.pf ppf "%s/%s" t.user t.repo

  module FullHookSet = struct
    include Set.Make(struct type t = hook let compare = compare_hook end)
    let sdiff x y = union (diff x y) (diff y x)
  end

  module SimpleHookSet = struct (* discard tree *)
    include Set.Make(struct type t = hook let compare = compare_user_repo end)
    let sdiff x y = union (diff x y) (diff y x)
  end

  type state = {
    hooks: SimpleHookSet.t; (* active hooks *)
    trees: FullHookSet.t;   (* active hooks and their FS tree *)
  }

  let empty = { hooks = SimpleHookSet.empty; trees = FullHookSet.empty }

  let read_subtree tree root =
    let rec aux acc path =
      DK.Tree.read_dir tree path >>*= fun dirs ->
      List.fold_left (fun acc dir ->
          acc >>*= fun acc ->
          let k = path / dir in
          DK.Tree.exists_dir tree k >>*= fun is_dir ->
          if is_dir then aux acc k >>*= ok
          else
            DK.Tree.read_file tree k >>*= fun v ->
            ok (Datakit_path.Map.add k v acc)
        ) (ok acc) dirs
    in
    aux Datakit_path.Map.empty root

  let state_of_commit c =
    let tree = DK.Commit.tree c in
    let root = Datakit_path.empty in
    DK.Tree.read_dir tree root >>*= fun users ->
    List.fold_left (fun acc user ->
        DK.Tree.read_dir tree (root / user) >>*= fun repos ->
        List.fold_left (fun acc repo ->
            acc >>*= fun acc ->
            read_subtree tree (root / user / repo) >>*= fun tree ->
            let hooks = SimpleHookSet.add { user; repo; tree } acc.hooks in
            let trees = FullHookSet.add { user; repo; tree } acc.trees in
            ok { hooks; trees }
          ) acc repos
      ) (ok empty) users

  module Of_github = struct

    (* Convert GitHub events to DataKit state. *)

    let update_pr ~root t pr =
      let dir = root / "prs" / string_of_int pr.PR.number in
      Log.debug (fun l -> l "update_pr %s" @@ Datakit_path.to_hum dir);
      match pr.PR.state with
      | `Closed ->
        DK.Transaction.exists t dir >>*= fun exists ->
        if exists then DK.Transaction.remove t dir else ok ()
      | `Open   ->
        DK.Transaction.make_dirs t dir >>*= fun () ->
        let data = Cstruct.of_string pr.PR.head in
        DK.Transaction.create_or_replace_file t ~dir "head" data

    let update_status ~root t s =
      let dir =
        root / "commits" / s.Status.commit /@
        Datakit_path.of_steps_exn (Status.path s)
      in
      Log.debug (fun l -> l "update_status %s" @@ Datakit_path.to_hum dir);
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let some = function None -> "" | Some s -> s in
      let state = function
        | `Error -> "error" | `Failure -> "failure"
        | `Pending  -> "pending" | `Success -> "success"
      in
      let kvs = [
        "description", some  s.Status.description;
        "state"      , state s.Status.state;
        "target_url" , some  s.Status.url;
      ] in
      List.fold_left (fun acc (k, v) ->
          acc >>*= fun () ->
          let v = Cstruct.of_string v in
          DK.Transaction.create_or_replace_file t ~dir k v
        ) (ok ()) kvs

    let sync token ~user ~repo t =
      let root = Datakit_path.empty / user / repo in
      API.events token ~user ~repo >>= fun events ->
      Lwt_list.fold_left_s (fun acc e ->
          match acc with
          | Error e -> Lwt.return (Error e)
          | Ok ()   -> match e with
            | Event.PR pr    -> update_pr ~root t pr
            | Event.Status s -> update_status ~root t s
            | _               -> ok ()
        ) (Ok ()) (List.rev events)

  end

  (* Read the GitHub events for the repositories appearing in [diff]
     and populate [branch] with the result of applying all of the into
     the state. *)
  (* NOTE: quite slow, use it with care *)
  let sync_from_events ~token branch diff =
    if SimpleHookSet.is_empty diff then ok ()
    else DK.Branch.with_transaction branch (fun tr ->
        Lwt_list.iter_p (fun { user; repo; _ } ->
            Of_github.sync token ~user ~repo tr >|= function
            | Ok ()   -> ()
            | Error e ->
              Log.err (fun l ->
                  l "Error while syncing %s/%s: %a"
                    user repo DK.pp_error e);
          ) (SimpleHookSet.elements diff)
        >>= fun () ->
        let message =
          Fmt.strf "Syncing with events %a" Fmt.(list pp_hook)
          @@ SimpleHookSet.elements diff
        in
        DK.Transaction.commit tr ~message
      )

  (* Read the diff and do stuff: TODO *)
  let converge_state ~token branch diff =
    let _ = token in
    let _ = branch in
    let _ = diff in
    ok ()

  let sync token t ~branch =
    DK.branch t branch >>*= fun branch ->
    let current_state = ref empty in
    DK.Branch.wait_for_head branch (function
        | None   -> ok `Again
        | Some c ->
          state_of_commit c >>= function
          | Error e ->
            Log.err (fun l -> l "Error while reading hooks: %a" DK.pp_error e);
            ok `Again
          | Ok state ->
            let diff = SimpleHookSet.sdiff !current_state.hooks state.hooks in
            sync_from_events ~token branch diff >>*= fun () ->
            let diff = FullHookSet.sdiff !current_state.trees state.trees in
            converge_state ~token branch diff >>*= fun () ->
            ok `Again
      )

  let sync token t ~branch =
    sync token t ~branch >|= function
    | Ok _    -> ()
    | Error e -> Log.err (fun l -> l "Error: %a" DK.pp_error e)

end

module API = struct

  (* Implement API with direct GitHub API calls. *)

  open Github_t

  type token = Github.Token.t

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

  let status token ~user ~repo ~commit =
    Github.Status.for_ref ~token ~user ~repo ~git_ref:commit ()
    |> Github.Stream.to_list
    |> run
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

end
