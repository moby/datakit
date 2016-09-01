open Astring
open Test_utils
open Lwt.Infix
open Datakit_github
open Datakit_path.Infix
open Result

module Conv = Conv(DK)

module Counter = struct

  type t = {
    mutable events    : int;
    mutable prs       : int;
    mutable status    : int;
    mutable refs      : int;
    mutable set_status: int;
    mutable set_pr    : int;
    mutable set_ref   : int;
  }

  let zero () = {
    events = 0; prs = 0; status = 0; refs = 0;
    set_status = 0; set_pr = 0; set_ref = 0 ;
  }

  let sets t = t.set_status + t.set_pr + t.set_ref

  let pp ppf t =
    Fmt.pf ppf
      "event:%d prs:%d status:%d refs:%d set-status:%d set-pr:%d set-ref:%d"
      t.events t.prs t.status t.refs t.set_status t.set_pr t.set_ref

  let equal x y = Pervasives.compare x y = 0

end

module R = struct

  type t = {
    user  : string;
    repo  : string;
    mutable commits: (string * Status.t list) list;
    mutable prs    : PR.t list;
    mutable refs   : Ref.t list;
    mutable events : Event.t list;
  }

  let create { Repo.user; repo } =
    { user; repo; commits = []; prs = []; refs = []; events = [] }

  let prune r =
    let prs = List.filter (fun pr -> PR.state pr = `Open) r.prs in
    let commits =
      let prs = Commit.Set.of_list (List.map PR.commit prs) in
      let refs = Commit.Set.of_list (List.map Ref.commit r.refs) in
      Commit.Set.union prs refs
    in
    let commits =
      List.filter (fun (id, s) ->
          s <> [] && Commit.Set.exists (fun c -> Commit.id c = id) commits
        ) r.commits
    in
    { r with prs; commits }

  let is_empty t = t.commits = [] && t.prs = [] && t.refs = []
  let events t = t.events
  let clear t = t.events <- []

  let pp_status f = function
    | `Open   -> Fmt.string f "open"
    | `Closed -> Fmt.string f "closed"

  let pp_pr f pr =
    Fmt.pf f "{n=%d;head=%s;title=%S;%a}"
      pr.PR.number (PR.commit_id pr) pr.PR.title pp_status pr.PR.state

  let pp_state f (commit, states) =
    Fmt.pf f "%s->%a" commit (Fmt.Dump.list Status.pp) states

  let pp_refs f r =
    Fmt.pf f "{name=%a;head=%s}"
      Fmt.(Dump.list string) r.Ref.name (Ref.commit_id r)

  let pp f { commits; prs; refs; _ } =
    Fmt.pf f "prs=%a;@,refs=%a;@,commits=%a"
      (Fmt.Dump.list pp_pr) prs
      (Fmt.Dump.list pp_refs) refs
      (Fmt.Dump.list pp_state) commits

  let status_equal a b =
    a.Status.state       = b.Status.state &&
    a.Status.description = b.Status.description &&
    a.Status.url         = b.Status.url

  let equal_commit a b =
    let to_map x =
      x
      |> List.map (fun a -> String.concat ~sep:"/" a.Status.context, a)
      |> String.Map.of_list
    in
    let a = to_map a in
    let b = to_map b in
    String.Map.equal status_equal a b

  let equal_state a b =
    let to_map x =
      x
      |> List.filter (fun (_, status) -> status <> [])
      |> String.Map.of_list
    in
    let a = to_map a in
    let b = to_map b in
    String.Map.equal equal_commit a b

  let equal_pr a b =
    a.PR.title = b.PR.title &&
    a.PR.head = b.PR.head

  let equal_prs a b =
    let to_map x =
      x
      |> List.map (fun a -> String.of_int a.PR.number, a)
      |> String.Map.of_list
    in
    let a = to_map a in
    let b = to_map b in
    String.Map.equal equal_pr a b

  let equal a b = equal_state a.commits b.commits && equal_prs a.prs b.prs

end

module User = struct

  type t = {
    mutable repos : R.t String.Map.t;
  }

  let empty () = { repos = String.Map.empty }

  let mem_repo t r = String.Map.mem r.Repo.repo t.repos

  let add_repo t r =
    let repos = String.Map.add r.Repo.repo (R.create r) t.repos in
    { repos }

  let prune monitored_repos t =
    let repos =
      String.Map.filter (fun _ { R.user; repo; _ } ->
          Repo.Set.mem { Repo.user; repo } monitored_repos
        ) t.repos
    in
    let repos = String.Map.map R.prune repos in
    { repos }

  let prune_commits t =
    let repos =
      String.Map.map (fun r ->
          let commits =
            List.fold_left (fun acc (_, s as c) ->
                if s = [] then acc else c :: acc
              ) [] r.R.commits
            |> List.rev
          in
          { r with R.commits }
        ) t.repos
    in
    { repos }

  let fold f t acc = String.Map.fold (fun _ repo acc -> f repo acc ) t.repos acc

  let repos t =
    fold (fun { R.user; repo; _ } acc ->
        Repo.Set.add { Repo.user; repo } acc
      ) t Repo.Set.empty

  let commits t =
    fold (fun r acc ->
        let repo = { Repo.user = r.R.user; repo = r.R.repo } in
        List.fold_left (fun acc (id, _) ->
            Commit.Set.add { Commit.repo; id } acc
          ) acc r.R.commits
      ) t Commit.Set.empty

  let prs t =
    fold (fun r acc ->
        PR.Set.union acc (PR.Set.of_list r.R.prs)
      ) t PR.Set.empty

  let status t =
    fold (fun r acc ->
        List.fold_left (fun acc (_, s) ->
            Status.Set.union acc (Status.Set.of_list s)
          ) acc r.R.commits
      ) t Status.Set.empty

  let refs t =
    fold (fun r acc ->
        Ref.Set.union acc (Ref.Set.of_list r.R.refs)
      ) t Ref.Set.empty

  let events t = fold (fun r acc -> R.events r @ acc) t []
  let clear t = fold (fun r () -> R.clear r) t ()
  let pp f { repos } = String.Map.dump R.pp f repos
  let equal a b = String.Map.equal R.equal a.repos b.repos

end

module Users = struct

  type t = {
    mutable users: User.t String.Map.t;
  }

  let of_repos repos: t =
    let users =
      Repo.Set.fold (fun { Repo.user; _ } acc -> user :: acc) repos []
      |> List.map (fun u -> u, { User.repos = String.Map.empty })
      |> String.Map.of_list
    in
    Repo.Set.fold (fun ({Repo.user; repo} as r) acc ->
        let u = String.Map.get user acc in
        let u =
          { User.repos = String.Map.add repo (R.create r) u.User.repos }
        in
        String.Map.add user u acc
      ) repos users
    |> fun users -> { users }

  let empty () = { users = String.Map.empty }

  let mem_repo t r =
    String.Map.exists (fun u x -> u = r.Repo.user && User.mem_repo x r) t.users

  let add_repo t r =
    let user = match String.Map.find r.Repo.user t.users with
      | None   -> User.empty ()
      | Some u -> u
    in
    let user = User.add_repo user r in
    let users = String.Map.add r.Repo.user user t.users in
    t.users <- users

  let remove_repo t r =
    let users = String.Map.remove r.Repo.user t.users in
    t.users <- users

  let prune repos t = { users = String.Map.map (User.prune repos) t.users }

  let prune_commits t = { users = String.Map.map User.prune_commits t.users }

  let fold f t acc = String.Map.fold (fun _ user acc -> f user acc ) t.users acc

  let repos t =
    fold (fun u acc -> Repo.Set.union acc (User.repos u)) t Repo.Set.empty

  let commits t =
    fold (fun u acc -> Commit.Set.union acc (User.commits u)) t Commit.Set.empty

  let prs t =
    fold (fun u acc -> PR.Set.union acc (User.prs u)) t PR.Set.empty

  let status t =
    fold (fun u acc -> Status.Set.union acc (User.status u)) t Status.Set.empty

  let refs t =
    fold (fun u acc -> Ref.Set.union acc (User.refs u)) t Ref.Set.empty

  let pp f t = String.Map.dump User.pp f t.users

  let diff x y =
    let repos = Repo.Set.diff (repos x) (repos y) in
    let commits = Commit.Set.diff (commits x) (commits y) in
    let prs = PR.Set.diff (prs x) (prs y) in
    let status = Status.Set.diff (status x) (status y) in
    let refs = Ref.Set.diff (refs x) (refs y) in
    Snapshot.create ~repos ~commits ~status ~prs ~refs

  let diff_events new_t old_t =
    let news = diff new_t old_t in
    let olds = diff old_t new_t in
    let new_repos = Snapshot.repos news in
    let old_repos = Snapshot.repos olds in
    let keep f x = not (Repo.Set.mem (f x) old_repos) in
    let new_prs = Snapshot.prs news in
    let new_refs = Snapshot.refs news in
    let new_status = Snapshot.status news in
    let repos =
      List.map (Event.repo' `Monitored) (Repo.Set.elements new_repos)
      @ List.map (Event.repo' `Ignored) (Repo.Set.elements old_repos)
    in
    let prs =
      PR.Set.filter (keep PR.repo) new_prs
      |> PR.Set.elements
      |> List.map Event.pr
    in
    let refs =
      Ref.Set.filter (keep Ref.repo) new_refs
      |> Ref.Set.elements
      |> List.map (Event.ref `Updated)
    in
    let status =
      Status.Set.filter (keep Status.repo) new_status
      |> Status.Set.elements
      |> List.map Event.status
    in
    let close_prs =
      Snapshot.prs olds
      |> PR.Set.filter
        (fun pr -> keep PR.repo pr && not (PR.Set.exists (PR.same pr) new_prs))
      |> PR.Set.elements
      |> List.map (fun pr -> Event.PR { pr with PR.state = `Closed })
    in
    let close_refs =
      Snapshot.refs olds
      |> Ref.Set.filter
        (fun r -> keep Ref.repo r && not (Ref.Set.exists (Ref.same r) new_refs))
      |> Ref.Set.elements
      |> List.map (fun r -> Event.Ref (`Removed, r))
    in
    repos @ prs @ refs @ status @ close_prs @ close_refs

  let equal a b = String.Map.equal User.equal a.users b.users

  let mem x { users } = String.Map.mem x users
  let find x { users } = String.Map.find x users
  let iter x { users } = String.Map.iter x users
end

module API = struct

  let error_rate = ref None

  let return x = match !error_rate with
    | None   -> Lwt.return (Ok x)
    | Some n ->
      if Random.float 1. > n then Lwt.return (Ok x)
      else Lwt.return (Error "Randam error")

  type t = {
    users: Users.t;
    ctx  : Counter.t;
  }

  type 'a result = ('a, string) Result.result Lwt.t

  type token = t

  let fold f t acc = Users.fold f t.users acc
  let iter f t = Users.iter f t.users

  let lookup t { Repo.user; repo }  =
    match Users.find user t.users with
    | None      -> None
    | Some user -> String.Map.find repo user.User.repos

  let lookup_exn t repo =
    match lookup t repo with
    | Some repo -> repo
    | None      -> failwith (Fmt.strf "Unknown user/repo: %a" Repo.pp repo)

  let add_event t e =
    let r = lookup_exn t (Event.repo e) in
    r.R.events <- e :: r.R.events

  let user_exists t ~user = return (Users.mem user t.users)
  let repo_exists t repo  = return (lookup t repo <> None)

  let repos t ~user =
    match Users.find user t.users with
    | None   -> return []
    | Some u ->
      String.Map.dom u.User.repos
      |> String.Set.elements
      |> List.map (fun repo -> { Repo.user; repo })
      |> return

  let set_repo_aux t (s, r) =
    let t = t.users in
    match s with
    | `Monitored -> if not (Users.mem_repo t r) then Users.add_repo t r
    | `Ignored   -> if Users.mem_repo t r then Users.remove_repo t r

  let set_status_aux t s =
    let repo = lookup_exn t (Status.repo s) in
    let commit = Status.commit_id s in
    let keep (c, _) = c <> commit in
    let commits = List.filter keep repo.R.commits in
    let rest =
      try
        List.find (fun x -> not (keep x)) repo.R.commits
        |> snd
        |> List.filter (fun y -> y.Status.context <> s.Status.context)
      with Not_found ->
        []
    in
    let commits = (commit, s :: rest) :: commits in
    repo.R.commits <- commits;
    add_event t (Event.Status s)

  let set_status t s =
    t.ctx.Counter.set_status <- t.ctx.Counter.set_status + 1;
    set_status_aux t s;
    return ()

  let set_pr_aux t pr =
    let repo = lookup_exn t (PR.repo pr) in
    let num = pr.PR.number in
    let prs = List.filter (fun pr -> pr.PR.number <> num) repo.R.prs in
    repo.R.prs <- pr :: prs;
    add_event t (Event.PR pr)

  let set_pr t pr =
    t.ctx.Counter.set_pr <- t.ctx.Counter.set_pr + 1;
    set_pr_aux t pr;
    return ()

  let set_ref_aux t (s, r) =
    let repo = lookup_exn t (Ref.repo r) in
    let name = r.Ref.name in
    let refs = List.filter (fun r -> r.Ref.name <> name) repo.R.refs in
    match s with
    | `Removed -> ()
    | `Created | `Updated ->
      repo.R.refs <- r :: refs;
      add_event t (Event.Ref (s, r))

  let set_ref t r =
    t.ctx.Counter.set_ref <- t.ctx.Counter.set_ref + 1;
    set_ref_aux t (`Updated, r);
    return ()

  let remove_ref t repo name =
    t.ctx.Counter.set_ref <- t.ctx.Counter.set_ref + 1;
    set_ref_aux t (`Removed, { Ref.name; head = { Commit.repo; id = "" }});
    return ()

  let status t commit =
    t.ctx.Counter.status <- t.ctx.Counter.status + 1;
    match lookup t (Commit.repo commit) with
    | None   -> return []
    | Some r ->
      try return (List.assoc (Commit.id commit) r.R.commits)
      with Not_found -> return []

  let prs t repo =
    t.ctx.Counter.prs <- t.ctx.Counter.prs + 1;
    match lookup t repo with
    | None   -> return []
    | Some r -> return r.R.prs

  let events t repo =
    t.ctx.Counter.events <- t.ctx.Counter.events + 1;
    match lookup t repo with
    | None   -> return []
    | Some r -> return r.R.events

  let refs t repo =
    t.ctx.Counter.refs <- t.ctx.Counter.refs + 1;
    match lookup t repo with
    | None   -> return []
    | Some r -> return r.R.refs

  let apply_events t =
    t.users |> Users.iter @@ fun _ user ->
    user.User.repos |> String.Map.iter @@ fun _ repo ->
    repo.R.events |> List.iter @@ function
    | Event.Repo r   -> set_repo_aux t r
    | Event.PR pr    -> set_pr_aux t pr
    | Event.Status s -> set_status_aux t s
    | Event.Ref r    -> set_ref_aux t r
    | Event.Other _  -> ()

  let create ?(events=[]) users =
    let t = { users; ctx = Counter.zero () } in
    List.iter (add_event t) events;
    t

  module Webhook = struct

    type t = {
      state: token;
      mutable repos: Repo.Set.t;
    }

    let block () = let t, _ = Lwt.task () in t

    let create state _ = { state; repos = Repo.Set.empty  }
    let repos t = t.repos
    let run _ = block ()
    let wait _ = block ()

    let v ?old state =
      let repos = match old with None -> Repo.Set.empty | Some t -> t.repos in
      { state; repos }

    let watch t r =
      if not (Repo.Set.mem r t.repos) then t.repos <- Repo.Set.add r t.repos;
      Lwt.return_unit

    let events t =
      let x = fold (fun u acc -> User.events u @ acc) t.state [] in
      let x = List.filter (fun x -> Repo.Set.mem (Event.repo x) t.repos) x in
      List.rev x

    let clear t = iter (fun _ -> User.clear) t.state

  end

  let all_events t = fold (fun u acc -> User.events u @ acc) t []

  let all_repos t =
    fold (fun u acc -> Repo.Set.union (User.repos u) acc) t Repo.Set.empty

end

module VG = Sync(API)(DK)

let user = "test"
let repo = "test"
let pub = "test-pub"
let priv = "test-priv"

let repo = { Repo.user; repo }
let commit_bar = { Commit.repo; id = "bar" }
let commit_foo = { Commit.repo; id = "foo" }

let s1 = {
  Status.context = ["foo"; "bar"; "baz"];
  url            = None;
  description    = Some "foo";
  state          = `Pending;
  commit = commit_bar;
}

let s2 = {
  Status.context = ["foo"; "bar"; "toto"];
  url            = Some "toto";
  description    = None;
  state          = `Failure;
  commit = commit_bar;
}

let s3 = {
  Status.context = ["foo"; "bar"; "baz"];
  url            = Some "titi";
  description    = Some "foo";
  state          = `Success;
  commit = commit_foo;
}

let s4 = {
  Status.context = ["foo"];
  url            = None;
  description    = None;
  state          = `Pending;
  commit = commit_bar;
}

let s5 = {
  Status.context = ["foo"; "bar"; "baz"];
  url            = Some "titi";
  description    = None;
  state          = `Failure;
  commit = commit_foo;
}

let pr1 = { PR.number = 1; state = `Open  ; head = commit_foo; title = "" }
let pr2 = { PR.number = 1; state = `Closed; head = commit_foo; title = "foo" }
let pr3 = { PR.number = 2; state = `Open  ; head = commit_bar; title = "bar" }
let pr4 = { PR.number = 2; state = `Open  ; head = commit_bar; title = "toto" }

let ref1 = { Ref.head = commit_bar; name = ["heads";"master"] }
let ref2 = { Ref.head = commit_foo; name = ["heads";"master"] }

let events0 = [
  Event.PR pr1;
  Event.PR pr2;
  Event.PR pr3;
  Event.Status s1;
  Event.Status s2;
  Event.Status s3;
  Event.Status s4;
]

let events1 = [
  Event.PR pr1;
  Event.PR pr4;
  Event.Status s1;
  Event.Status s2;
  Event.Status s3;
  Event.Status s4;
]

let prs0    = [pr1; pr3]
let status0 = [s1; s2; s3; s4]
let refs0   = [ref1]
let repos0   = [repo]
let commits0 = [ commit_foo; commit_bar ]

let status_state: Status_state.t Alcotest.testable =
  (module struct include Status_state let equal = (=) end)

let snapshot: Snapshot.t Alcotest.testable =
  (module struct include Snapshot let equal x y = Snapshot.compare x y = 0 end)

let diff: Diff.t Alcotest.testable =
  (module struct include Diff let equal = (=) end)

let diffs = Alcotest.slist diff Diff.compare

let d id = { Diff.repo; id }

let counter: Counter.t Alcotest.testable = (module Counter)

let test_snapshot () =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  Test_utils.run (fun _repo conn ->
      let dk = DK.connect conn in
      DK.branch dk "test-snapshot" >>*= fun br ->
      let update ~prs ~status ~refs =
        let err e = Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e in
        DK.Branch.with_transaction br (fun tr ->
            begin Conv.update_repo tr `Monitored repo >>= function
              | Ok ()   -> Lwt.return_unit
              | Error e -> err e
            end >>= fun () ->
            Lwt_list.iter_p (fun pr ->
                Conv.update_pr tr pr >>= function
                | Ok ()   -> Lwt.return_unit
                | Error e -> err e
              ) prs
            >>= fun () ->
            Lwt_list.iter_p (fun s ->
                Conv.update_status tr s >>= function
                | Ok ()   -> Lwt.return_unit
                | Error e -> err e
              ) status
            >>= fun () ->
            Lwt_list.iter_p (fun r ->
                Conv.update_ref tr `Updated r >>= function
                | Ok ()   -> Lwt.return_unit
                | Error e -> err e
              ) refs
            >>= fun () ->
            Conv.snapshot "init" Conv.(tree_of_transaction tr) >>= fun s ->
            DK.Transaction.commit tr ~message:"init" >>*= fun () ->
            ok s)
      in
      update ~prs:prs0 ~status:status0 ~refs:refs0 >>*= fun s ->
      expect_head br >>*= fun head ->
      let se =
        let prs = PR.Set.of_list prs0 in
        let status = Status.Set.of_list status0 in
        let refs = Ref.Set.of_list refs0 in
        let repos = Repo.Set.of_list repos0 in
        let commits = Commit.Set.of_list commits0 in
        Snapshot.create ~repos ~commits ~prs ~status ~refs
      in
      Conv.snapshot "sh" Conv.(tree_of_commit head) >>= fun sh ->
      Alcotest.(check snapshot) "snap transaction" se s;
      Alcotest.(check snapshot) "snap head" se sh;

      update ~prs:[pr2] ~status:[] ~refs:[] >>*= fun s1 ->
      expect_head br >>*= fun head1 ->
      let tree1 = Conv.tree_of_commit head1 in
      Conv.safe_diff tree1 head >>= fun diff1 ->
      Alcotest.(check diffs) "diff1" [d (`PR 1)] (Diff.Set.elements diff1);
      Conv.snapshot "sd" ~old:(head, s) tree1 >>= fun sd ->
      Alcotest.(check snapshot) "snap diff" s1 sd;

      update ~prs:[] ~status:[s5] ~refs:[ref2] >>*= fun s2 ->
      expect_head br >>*= fun head2 ->
      let tree2 = Conv.tree_of_commit head2 in
      Conv.safe_diff tree2 head1 >>= fun diff2 ->
      Alcotest.(check diffs) "diff2"
        [d (`Status ("foo", ["foo";"bar";"baz"]));
         d (`Ref ["heads";"master"])]
        (Diff.Set.elements diff2);
      Conv.snapshot "sd1" ~old:(head , s ) tree2 >>= fun sd1 ->
      Conv.snapshot "ss2" ~old:(head1, s1) tree2 >>= fun sd2 ->
      Alcotest.(check snapshot) "snap diff1" s2 sd1;
      Alcotest.(check snapshot) "snap diff2" s2 sd2;

      DK.Branch.with_transaction br (fun tr ->
          DK.Transaction.make_dirs tr (p "test/toto") >>*= fun () ->
          DK.Transaction.create_or_replace_file tr ~dir:(p "test/toto") "FOO"
            (v "") >>*= fun () ->
          DK.Transaction.commit tr ~message:"test/foo"
        ) >>*= fun () ->
      expect_head br >>*= fun head3 ->
      let tree3 = Conv.tree_of_commit head3 in
      Conv.safe_diff tree3 head2 >>= fun diff3 ->
      let d = { Diff.repo = { Repo. user; repo = "toto" }; id = `Unknown } in
      Alcotest.(check diffs) "diff3" [d] (Diff.Set.elements diff3);

      Lwt.return_unit
    )

let init status refs events =
  let tbl = Hashtbl.create (List.length status) in
  List.iter (fun s ->
      let v =
        try Hashtbl.find tbl s.Status.commit
        with Not_found -> []
      in
      Hashtbl.replace tbl s.Status.commit (s :: v)
    ) status;
  let commits = Hashtbl.fold (fun k v acc -> (Commit.id k, v) :: acc) tbl [] in
  let users = String.Map.singleton user {
      User.repos = String.Map.singleton repo.Repo.repo
          { R.user; repo = repo.Repo.repo; commits; refs; prs = []; events }
    } in
  let t = API.create { Users.users } in
  API.apply_events t;
  t

let root { Repo.user; repo } = Datakit_path.(empty / user / repo)

let run_with_test_test f () =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  Test_utils.run (fun _repo conn ->
      let dk = DK.connect conn in
      DK.branch dk pub  >>*= fun pub ->
      DK.branch dk priv >>*= fun priv ->
      let t = init [] [] [] in
      let s = VG.empty in
      VG.sync ~policy:`Once s ~priv ~pub ~token:t >>= fun _s ->
      DK.Branch.with_transaction pub (fun tr ->
          Conv.update_repo tr `Monitored repo >>*= fun () ->
          DK.Transaction.commit tr ~message:"init"
        )
      >>*= fun () ->
      f dk
    )

let check_dirs = Alcotest.(check (slist string String.compare))
let check_data msg x y = Alcotest.(check string) msg x (Cstruct.to_string y)

let check name tree =
  (* check test/test/commit *)
  let commit = root repo / "commit" in
  DK.Tree.exists_dir tree commit >>*= fun exists ->
  Alcotest.(check bool) (name ^ " commit dir exists")  exists true;
  DK.Tree.read_dir tree commit >>*= fun dirs ->
  check_dirs "commits" ["bar"] dirs;
  DK.Tree.read_dir tree (commit / "bar"/ "status" ) >>*= fun dirs ->
  check_dirs "status 0" ["foo"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" ) >>*= fun dirs ->
  check_dirs "status 1" ["state";"bar"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar")
  >>*= fun dirs ->
  check_dirs "status 2" ["baz";"toto"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar" / "baz")
  >>*= fun dirs ->
  check_dirs "status 3" ["description";"state"] dirs;
  DK.Tree.read_file tree
    (commit / "bar" / "status" / "foo" / "bar" / "baz" / "state")
  >>*= fun data ->
  check_data "status/state" "pending\n" data;
  DK.Tree.read_file tree
    (commit / "bar" / "status" / "foo" / "bar" / "baz" / "description")
  >>*= fun data ->
  check_data "status/description" "foo\n" data;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar" / "toto")
  >>*= fun dirs ->
  check_dirs "status 3" ["target_url";"state"] dirs;

  (* check test/test/pr *)
  let pr = root repo / "pr" in
  DK.Tree.exists_dir tree pr >>*= fun exists ->
  Alcotest.(check bool) "pr dir exists" true exists;
  DK.Tree.read_dir tree pr >>*= fun dirs ->
  check_dirs "pr 1" ["2"] dirs ;
  DK.Tree.read_dir tree (pr / "2") >>*= fun dirs ->
  check_dirs "pr 2" dirs ["state"; "head"; "title"];
  DK.Tree.read_file tree (pr / "2" / "state") >>*= fun data ->
  check_data "state" "open\n" data;
  DK.Tree.read_file tree (pr / "2" / "head") >>*= fun data ->
  check_data "head" "bar\n" data ;

  Lwt.return_unit

open! Counter

let test_events dk =
  let t = init status0 refs0 events0 in
  let s = VG.empty in
  DK.branch dk priv >>*= fun priv ->
  DK.branch dk pub  >>*= fun pub  ->
  let sync s = VG.sync ~policy:`Once ~priv ~pub ~token:t s in
  Alcotest.(check counter) "counter: 0"
    { events = 0; prs = 0; status = 0; refs = 0;
      set_pr = 0; set_status = 0; set_ref = 0 }
    t.API.ctx;
  sync s >>= fun s ->
  sync s >>= fun s ->
  sync s >>= fun s ->
  sync s >>= fun s ->
  sync s >>= fun s ->
  sync s >>= fun s ->
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 1; status = 1; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    t.API.ctx;
  sync s >>= fun _s ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 1; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    t.API.ctx;
  expect_head priv >>*= fun head ->
  check "priv" (DK.Commit.tree head) >>= fun () ->
  expect_head pub >>*= fun head ->
  check "pub" (DK.Commit.tree head)

let update_status br dir state =
  DK.Branch.with_transaction br (fun tr ->
      let dir = dir / "status" / "foo" / "bar" / "baz" in
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let state = Cstruct.of_string  (Status_state.to_string state ^ "\n") in
      DK.Transaction.create_or_replace_file tr ~dir "state" state >>*= fun () ->
      DK.Transaction.commit tr ~message:"Test"
    )

let find_status t repo =
  let repo = API.lookup_exn t repo in
  try List.find (fun (c, _) -> c = "foo") repo.R.commits |> snd |> List.hd
  with Not_found -> Alcotest.fail "foo not found"

let find_pr t repo =
  let repo = API.lookup_exn t repo in
  try List.find (fun pr -> pr.PR.number = 2) repo.R.prs
  with Not_found -> Alcotest.fail "foo not found"

let test_updates dk =
  let t = init status0 refs0 events1 in
  let s = VG.empty in
  DK.branch dk priv >>*= fun priv ->
  DK.branch dk pub  >>*= fun pub ->
  let sync s = VG.sync ~policy:`Once ~priv ~pub ~token:t s in
  Alcotest.(check counter) "counter: 0"
    { events = 0; prs = 0; status = 0; refs = 0;
      set_pr = 0; set_status = 0; set_ref = 0  }
    t.API.ctx;
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    t.API.ctx;
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 1'"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    t.API.ctx;

  (* test status update *)
  let dir = root repo / "commit" / "foo" in
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  update_status pub dir `Pending >>*= fun () ->
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    t.API.ctx;
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 3"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    t.API.ctx;
  let status = find_status t repo in
  Alcotest.(check status_state) "update status" `Pending status.Status.state;

  (* test PR update *)
  let dir = root repo / "pr" / "2" in
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  expect_head priv >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist commit/foo" true exists;
  DK.Branch.with_transaction pub (fun tr ->
      DK.Transaction.create_or_replace_file tr ~dir
        "title" (Cstruct.of_string "hahaha\n")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"Test"
    ) >>*= fun () ->
  sync s >>= fun _s ->
  Alcotest.(check counter) "counter: 4"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 1; set_status = 1; set_ref = 0 }
    t.API.ctx;
  let pr = find_pr t repo in
  Alcotest.(check string) "update pr's title" "hahaha" pr.PR.title;
  Lwt.return_unit

let test_startup dk =
  let t = init status0 refs0 events1 in
  let s = VG.empty in
  DK.branch dk priv >>*= fun priv ->
  DK.branch dk pub  >>*= fun pub ->
  let sync s = VG.sync ~policy:`Once ~priv ~pub ~token:t s in
  let dir = root repo / "commit" / "foo" in

  (* start from scratch *)
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 0; status = 0; refs = 0;
      set_pr = 0; set_status = 0; set_ref = 0 }
    t.API.ctx;
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    t.API.ctx;
  update_status pub dir `Pending >>*= fun () ->
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 3"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    t.API.ctx;

  sync s >>= fun s ->
  sync s >>= fun s ->
  sync s >>= fun _s ->
  Alcotest.(check counter) "counter: 3'"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    t.API.ctx;

  (* restart *)
  let s = VG.empty in
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 4"
    { events = 0; prs = 2; status = 4; refs = 2;
      set_pr = 0; set_status = 1; set_ref = 0 }
    t.API.ctx;
  sync s >>= fun s ->
  sync s >>= fun _s ->
  Alcotest.(check counter) "counter: 4'"
    { events = 0; prs = 2; status = 4; refs = 2;
      set_pr = 0; set_status = 1; set_ref = 0 }
    t.API.ctx;

  (* restart with dirty public branch *)
  let s = VG.empty in
  update_status pub dir `Failure >>*= fun () ->
  sync s >>= fun s ->
  sync s >>= fun s ->
  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 5"
    { events = 0; prs = 3; status = 6; refs = 3;
      set_pr = 0; set_status = 2; set_ref = 0 }
    t.API.ctx;
  let status = find_status t repo in
  Alcotest.(check status_state) "update status" `Failure status.Status.state;

  sync s >>= fun s ->
  Alcotest.(check counter) "counter: 6"
    { events = 0; prs = 3; status = 6; refs = 3;
      set_pr = 0; set_status = 2; set_ref = 0 }
    t.API.ctx;

  (* changes done in the public branch are never overwritten
     FIXME: we might want to improve/change this in the future. *)
  sync s >>= fun _s ->
  Alcotest.(check counter) "counter: 7"
    { events = 0; prs = 3; status = 6; refs = 3;
      set_pr = 0; set_status = 2; set_ref = 0 }
    t.API.ctx;
  let status_dir = dir / "status" / "foo" / "bar" / "baz" in
  expect_head pub >>*= fun h ->
  let tree = DK.Commit.tree h in
  DK.Tree.exists_dir tree status_dir >>*= fun dir_exists ->
  Alcotest.(check bool) "dir exists" true dir_exists;
  DK.Tree.exists_file tree (status_dir / "state") >>*= fun file_exists ->
  Alcotest.(check bool) "file exists" true file_exists;
  DK.Tree.read_file tree (status_dir / "state") >>*= fun buf ->
  Alcotest.(check string) "webhook update" "failure\n" (Cstruct.to_string buf);

  Lwt.return_unit

let users = (module Users : Alcotest.TESTABLE with type t = Users.t)

let opt_read_file tree path =
  DK.Tree.read_file tree path >|= function
  | Ok data -> Some (String.trim (Cstruct.to_string data))
  | Error (`Msg "No such file or directory") -> None
  | Error (`Msg x) -> failwith x

let rec read_state ~user ~repo ~commit tree path context =
  DK.Tree.read_dir tree path >>= function
  | Error _ -> Lwt.return []
  | Ok items ->
    let open Datakit_path.Infix in
    DK.Tree.read_file tree (path / "state") >>= begin function
      | Error _ -> Lwt.return []
      | Ok status ->
        opt_read_file tree (path / "description") >>= fun description ->
        opt_read_file tree (path / "target_url") >>= fun url ->
        let state =
          let status = String.trim (Cstruct.to_string status) in
          match Status_state.of_string status with
          | None -> failwith (Fmt.strf "Bad state %S" status)
          | Some x -> x
        in
        let repo = { Repo.user; repo } in
        let commit = { Commit.repo; id = commit } in
        Lwt.return [{ Status.commit; state; context; description; url }]
    end
    >>= fun this_state ->
    items |> Lwt_list.map_s (function
        | "status" | "description" | "target_url" -> Lwt.return []
        | item ->
          read_state ~user ~repo ~commit tree (path / item) (context @ [item])
      )
    >>= fun children ->
    Lwt.return (this_state @ List.concat children)

let read_opt_dir tree path =
  DK.Tree.read_dir tree path >|= function
  | Ok items -> items
  | Error (`Msg "No such file or directory") -> []
  | Error (`Msg x) -> failwith x

let read_commits tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "commit"] in
  read_opt_dir tree path >>=
  Lwt_list.map_s (fun commit ->
      let path =
        Datakit_path.of_steps_exn [user; repo; "commit"; commit; "status"]
      in
      read_state ~user ~repo ~commit tree path [] >>= fun states ->
      Lwt.return (commit, states)
    )

let read_prs tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "pr"] in
  read_opt_dir tree path >>=
  Lwt_list.map_s (fun number ->
      let path = Datakit_path.of_steps_exn [user; repo; "pr"; number] in
      let number = int_of_string number in
      let read name =
        DK.Tree.read_file tree (path / name) >>*= fun data ->
        Lwt.return (String.trim (Cstruct.to_string data))
      in
      read "head" >>= fun head ->
      read "title" >>= fun title ->
      let repo = { Repo.user; repo } in
      let head = { Commit.repo; id = head } in
      Lwt.return { PR.head; number; state = `Open; title; }
    )

let read_refs tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "ref"] in
  let rec aux acc name =
    let path = Datakit_path.(path /@ of_steps_exn name) in
    DK.Tree.read_file tree (path / "head") >>= begin function
      | Error _ -> Lwt.return acc
      | Ok head ->
        let head = String.trim (Cstruct.to_string head) in
        let repo = { Repo.user; repo } in
        let head = { Commit.repo; id = head } in
        Lwt.return ({ Ref.head; name} :: acc )
    end
    >>= fun acc ->
    DK.Tree.read_dir tree path >>= function
    | Error _   -> Lwt.return acc
    | Ok childs ->
      Lwt_list.fold_left_s (fun acc n -> aux acc (name @ [n])) acc childs
  in
  aux [] []

let safe_exists_file tree path =
  DK.Tree.exists_file tree path >|= function
  | Ok b    -> b
  | Error _ -> false

let state_of_branch b =
  expect_head b >>*= fun head ->
  let tree = DK.Commit.tree head in
  DK.Tree.read_dir tree Datakit_path.empty >>*=
  Lwt_list.fold_left_s (fun acc user ->
      DK.Tree.exists_dir tree Datakit_path.(empty / user) >>*= function
      | false -> Lwt.return acc
      | true  ->
        let path = Datakit_path.of_steps_exn [user] in
        DK.Tree.read_dir tree path >>*=
        Lwt_list.fold_left_s (fun acc repo ->
            safe_exists_file tree (path / repo / ".monitor") >>= function
            | false -> Lwt.return acc
            | true  ->
              read_commits tree ~user ~repo >>= fun commits ->
              read_prs tree ~user ~repo >>= fun prs ->
              read_refs tree ~user ~repo >>= fun refs ->
              let v = { R.user; repo; commits; prs; refs; events = [] } in
              String.Map.add repo v acc
              |> Lwt.return
          ) String.Map.empty
        >>= fun repos ->
        Lwt.return (String.Map.add user { User.repos } acc)
    ) String.Map.empty
  >|= fun users ->
  { Users.users }

let ensure_in_sync ~msg github pub =
  state_of_branch pub >>= fun pub_users ->
  Fmt.pr "GitHub:@\n@[%a@]@.DataKit:@\n@[%a@]@."
    Users.pp github.API.users Users.pp pub_users;
  let repos = Users.repos pub_users in
  let github = Users.prune repos github.API.users in
  let pub_users = Users.prune_commits pub_users in
  Alcotest.check snapshot (msg ^ "[github-pub]")
    Snapshot.empty (Users.diff github pub_users);
  Alcotest.check snapshot (msg ^ "[pub-github]")
    Snapshot.empty (Users.diff pub_users github);
  Alcotest.check users msg github pub_users;
  Lwt.return ()

let test_contexts = [|
  ["ci"; "datakit"; "test"];
  ["ci"; "datakit"; "build"];
  ["ci"; "circleci"];
|]

let test_pr_commits =  [| "123"; "456"; "789"; "0ab" |]
let test_ref_commits = [| "123"; "456"; "abc"; "def" |]

let test_names = [|
  ["heads"; "master"];
  ["tags" ; "foo"; "bar"];
  ["heads"; "gh-pages"];
|]

let test_descriptions = [|
  Some "Testing...";
  None;
|]

let test_state = [|
  `Pending;
  `Success;
  `Failure;
  `Error;
|]

let test_user = ["a"; "b"]
let test_repo = ["a"; "b"]

let random_choice ~random options =
  options.(Random.State.int random (Array.length options))

let random_pr_commit ~random ~repo =
  let id = random_choice ~random test_pr_commits in
  { Commit.repo; id }

let random_ref_commit ~random ~repo =
  let id = random_choice ~random test_ref_commits in
  { Commit.repo; id }

let random_description ~random = random_choice ~random test_descriptions

let random_state ~random = random_choice ~random test_state

let random_refs ~random ~repo ~old_refs =
  test_names
  |> Array.to_list
  |> List.map (fun name ->
      if Random.State.bool random then (
        match List.find (fun r -> r.Ref.name = name) old_refs with
        | exception Not_found -> []
        | old_ref ->
          if Random.State.bool random then [] else
            let head = random_ref_commit ~random ~repo in
            [{ old_ref with Ref.head }]
      ) else (
        let head = random_ref_commit ~random ~repo in
        [{ Ref.head; name }]
      ))
  |> List.concat

let random_status ~random ~old_status commit =
  let old_status = match List.assoc (Commit.id commit) old_status with
    | exception Not_found -> []
    | old_status -> old_status
  in
  test_contexts
  |> Array.to_list
  |> List.map (fun context ->
      if Random.State.bool random then (
        match List.find (fun s -> s.Status.context = context) old_status with
        | exception Not_found -> []
        | old_status -> [old_status]    (* GitHub can't delete statuses *)
      ) else (
        let state = random_state ~random in
        let description = random_description ~random in
        [{ Status.state; commit; description; url = None; context }]
      ))
  |> List.concat

let random_prs ~random ~repo ~old_prs =
  let n_prs = Random.State.int random 4 in
  let old_prs = List.rev old_prs in
  let old_prs =
    List.fold_left (fun prs pr ->
        if Random.State.bool random then prs
        else
          let state =
            match pr.PR.state with
            | `Open when Random.State.bool random -> `Closed
            | s -> s
          in
          let head = random_pr_commit ~random ~repo in
          { pr with PR.state; head } :: prs
      ) [] old_prs
  in
  let next_pr =
    ref (List.fold_left (fun n pr -> max (PR.number pr + 1) n) 0 old_prs)
  in
  let rec make_prs acc = function
    | 0 -> acc
    | n ->
      let head = random_pr_commit ~random ~repo in
      let number = !next_pr in
      incr next_pr;
      let pr = {
        PR.head;
        number;
        state = `Open;
        title = "PR#" ^ string_of_int number
      } in
      make_prs (pr :: acc) (n - 1)
  in
  make_prs old_prs n_prs |> List.rev

let random_state ~random ~repo ~old_prs ~old_status ~old_refs =
  let prs = random_prs ~random ~repo ~old_prs in
  let refs = random_refs ~random ~repo ~old_refs in
  let commits =
    Commit.Set.union
      (Commit.Set.of_list @@ List.map PR.commit prs)
      (Commit.Set.of_list @@ List.map Ref.commit refs)
  in
  let status =
    Commit.Set.fold (fun c acc ->
        match random_status ~random ~old_status c with
        | [] -> acc
        | s  -> (Commit.id c, s) :: acc
      ) commits []
  in
  prs, status, refs

let random_users ?(old=Users.empty ()) ~random =
  Users.iter (fun _ repo -> User.clear repo) old;
  test_user |> List.map (fun user ->
      let old_user =
        Users.find user old |> default { User.repos = String.Map.empty }
      in
      let repos =
        test_repo |> List.map (fun repo ->
            let r = { Repo.user; repo } in
            let old_prs, old_status, old_refs =
              match String.Map.find repo old_user.User.repos with
              | None      -> [], [], []
              | Some repo -> repo.R.prs, repo.R.commits, repo.R.refs
            in
            let prs, commits, refs =
              random_state ~random ~repo:r ~old_prs ~old_status ~old_refs
            in
            repo, { R.user; repo; commits; prs; refs; events = [] }
          )
        |> String.Map.of_list
      in
      user, { User.repos }
    )
  |> String.Map.of_list
  |> fun users -> { Users.users }

let test_random_gh ~quick _repo conn =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  let random = Random.State.make [| 1; 2; 3 |] in
  let dk = DK.connect conn in
  DK.branch dk pub  >>*= fun pub ->
  DK.Branch.with_transaction pub (fun t ->
      let monitor ~user ~repo =
        Conv.update_repo t `Monitored { Repo.user; repo }
      in
      monitor ~user:"a" ~repo:"a" >>*= fun () ->
      monitor ~user:"a" ~repo:"b" >>*= fun () ->
      monitor ~user:"b" ~repo:"a" >>*= fun () ->
      monitor ~user:"b" ~repo:"b" >>*= fun () ->
      DK.Transaction.commit t ~message:"Monitor repos"
    )
  >>*= fun () ->
  DK.branch dk priv >>*= fun priv ->
  let sync w t s =
    VG.sync ~policy:`Once s ~pub ~priv ~token:t ~webhook:w >|= fun s ->
    Alcotest.(check int) "API.set-*" 0 (Counter.sets t.API.ctx);
    s
  in
  let nsync ~fresh n w t s =
    let s = ref (if fresh then VG.empty else s) in
    let w = ref w in
    let rec aux k old =
      let users = random_users ~random ~old in
      let events = Users.diff_events users old in
      let t = API.create ~events users in
      w := API.Webhook.v ~old:!w t;
      VG.sync ~policy:`Once !s ~pub ~priv ~token:t ~webhook:!w >>= fun new_s ->
      Alcotest.(check int) "API.set-*" 0 (Counter.sets t.API.ctx);
      if not fresh then s := new_s;
      let msg = Fmt.strf "update %d (fresh=%b)" (n - k + 1) fresh in
      ensure_in_sync ~msg t pub >>= fun () ->
      if k > 1 then aux (k-1) users else Lwt.return (!w, t, !s)
    in
    aux n t.API.users
  in
  let s = VG.empty in
  let users = random_users ~random ?old:None in
  let t = API.create users in
  let w = API.Webhook.v t in
  sync w t s >>= fun _s ->
  ensure_in_sync ~msg:"init" t pub >>= fun () ->
  let users = random_users ~random ~old:users in
  let t = API.create users in
  let w = API.Webhook.v t ~old:w in
  sync w t s >>= fun s ->
  ensure_in_sync ~msg:"update" t pub >>= fun () ->
  nsync ~fresh:false (if quick then 2 else 10) w t s >>= fun (w, t, s) ->
  nsync ~fresh:true (if quick then 2 else 30) w t s  >>= fun (w, t, s) ->
  nsync ~fresh:false (if quick then 2 else 20) w t s >>= fun (w, t, s) ->
  let users = Users.of_repos (API.all_repos t) in
  let events = Users.diff_events users t.API.users in
  let t = API.create ~events users in
  let w = API.Webhook.v t ~old:w in
  sync w t s >>= fun _s ->
  ensure_in_sync ~msg:"empty" t pub >>= fun () ->
  Lwt.return_unit

exception DK_error of DK.error

let test_random_dk ~quick _repo conn =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  let random = Random.State.make [| 1; 2; 3 |] in
  let dk = DK.connect conn in
  DK.branch dk pub  >>*= fun pub ->
  DK.branch dk priv >>*= fun priv ->
  DK.Branch.with_transaction priv (fun t ->
      let dir = Datakit_path.empty in
      let empty = Cstruct.of_string "" in
      DK.Transaction.create_file t ~dir "README"  empty >>*= fun () ->
      DK.Transaction.commit t ~message:"Init private branch"
    )
  >>*= fun () ->
  let update ?new_state old =
    let users = match new_state with
      | None   -> random_users ~random ~old
      | Some s -> s
    in
    let events = Users.diff_events users old in
    DK.Branch.with_transaction pub (fun tr ->
        Lwt_list.iter_s (fun e ->
            Conv.update_event tr e >>= function
            | Error e -> Lwt.fail (DK_error e)
            | Ok ()   -> Lwt.return_unit
          ) events >>= fun () ->
        DK.Transaction.commit tr ~message:"User updates"
      ) >>= function
    | Error e -> Lwt.fail (DK_error e)
    | Ok ()   -> Lwt.return users
  in
  let sync t s = VG.sync ~policy:`Once s ~pub ~priv ~token:t in
  let nsync ~fresh n users s =
    let s = ref (if fresh then VG.empty else s) in
    let rec aux k old =
      update old >>= fun users ->
      let t = API.create old in
      VG.sync ~policy:`Once !s ~pub ~priv ~token:t >>= fun new_s ->
      if not fresh then s := new_s;
      let msg = Fmt.strf "update %d (fresh=%b)" (n - k + 1) fresh in
      ensure_in_sync ~msg t pub >>= fun () ->
      if k > 1 then aux (k-1) users else Lwt.return (!s, users)
    in
    aux n users
  in
  let s = VG.empty in
  let repos =
    List.fold_left (fun acc user ->
        List.fold_left (fun acc repo ->
            Repo.Set.add {Repo.user; repo} acc
          ) acc test_repo
      ) Repo.Set.empty test_user
  in
  let t = API.create (Users.of_repos repos) in
  update (Users.empty ()) >>= fun users ->
  sync t s >>= fun _s ->
  ensure_in_sync ~msg:"init" t pub >>= fun () ->
  let t = API.create users in
  update users >>= fun users ->
  sync t s >>= fun s ->
  ensure_in_sync ~msg:"update" t pub >>= fun () ->
  nsync ~fresh:false (if quick then 2 else 10) users s >>= fun (s, users) ->
  update users >>= fun users ->
  nsync ~fresh:true (if quick then 2 else 30) users s  >>= fun (s, users) ->
  update users >>= fun users ->
  nsync ~fresh:false (if quick then 2 else 20) users s  >>= fun (s, users) ->
  let new_state = Users.empty () in
  update ~new_state users >>= fun _ ->
  let t = API.create new_state in
  sync t s >>= fun _s ->
  let empty = API.create (Users.empty ()) in
  ensure_in_sync ~msg:"empty" empty pub >>= fun () ->
  Lwt.return_unit

let runx f () = Test_utils.run f

let test_set = [
  "snapshot"  , `Quick, test_snapshot;
  "events"    , `Quick, run_with_test_test test_events;
  "updates"   , `Quick, run_with_test_test test_updates;
  "startup"   , `Quick, run_with_test_test test_startup;
  "random-gh" , `Quick, runx (test_random_gh ~quick:true);
  "random-gh*", `Slow , runx (test_random_gh ~quick:false);
  "random-dk" , `Quick, runx (test_random_dk ~quick:true);
  "random-dk*", `Slow , runx (test_random_dk ~quick:false);

]
