open Astring
open Test_utils
open Lwt.Infix
open Datakit_path.Infix
open Datakit_github

open Result

let ( >>*= ) = ( >>**= )

let src = Logs.Src.create "test" ~doc:"Datakit tests"
module Log = (val Logs.src_log src)

module Conv = Datakit_github_conv.Make(DK)

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

  let v { Repo.user; repo } =
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
          s <> [] && Commit.Set.exists (fun c -> Commit.hash c = id) commits
        ) r.commits
    in
    { r with prs; commits }

  let events t = t.events
  let clear t = t.events <- []

  let pp_status f = function
    | `Open   -> Fmt.string f "open"
    | `Closed -> Fmt.string f "closed"

  let pp_pr f pr =
    Fmt.pf f "{n=%d;head=%s;title=%S;%a}"
      pr.PR.number (PR.commit_hash pr) pr.PR.title pp_status pr.PR.state

  let pp_state f (commit, states) =
    let states = List.sort Status.compare states in
    Fmt.pf f "%s->%a" commit (Fmt.Dump.list Status.pp) states

  let pp_refs f r =
    Fmt.pf f "{name=%a;head=%s}"
      Fmt.(Dump.list string) r.Ref.name (Ref.commit_hash r)

  let pp f { commits; prs; refs; _ } =
    let prs = List.sort PR.compare prs in
    let refs = List.sort Ref.compare refs in
    let commits =
      List.map (fun (n, s) -> n, List.sort Status.compare s) commits
      |> List.sort (fun (x, _) (y, _) -> String.compare x y)
    in
    Fmt.pf f "prs=%a;@,refs=%a;@,commits=%a"
      (Fmt.Dump.list pp_pr) prs
      (Fmt.Dump.list pp_refs) (List.sort Ref.compare refs)
      (Fmt.Dump.list pp_state) commits

  let status_equal a b =
    Status.state a       = Status.state b &&
    Status.description a = Status.description b &&
    Status.url a         = Status.url b

  let equal_commit a b =
    let to_map x =
      x
      |> List.map (fun a -> String.concat ~sep:"/" @@ Status.context a, a)
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

  let is_empty t = String.Map.is_empty t.repos
  let empty () = { repos = String.Map.empty }
  let mem_repo t r = String.Map.mem r.Repo.repo t.repos

  let add_repo t r =
    if String.Map.mem r.Repo.repo t.repos then t
    else
      let repos = String.Map.add r.Repo.repo (R.v r) t.repos in
      { repos }

  let lookup t r =
    match String.Map.find r.Repo.repo t.repos with
    | Some r -> r
    | None   ->
      let repo = R.v r in
      t.repos <-String.Map.add r.Repo.repo repo t.repos;
      repo

  let prune monitored_repos t =
    let repos =
      String.Map.filter (fun _ { R.user; repo; _ } ->
          Repo.Set.mem (Repo.v ~user ~repo) monitored_repos
        ) t.repos
    in
    let repos = String.Map.map R.prune repos in
    { repos }

  let fold f t acc = String.Map.fold (fun _ repo acc -> f repo acc ) t.repos acc

  let repos t =
    fold (fun { R.user; repo; _ } acc ->
        Repo.Set.add (Repo.v ~user ~repo) acc
      ) t Repo.Set.empty

  let commits t =
    fold (fun r acc ->
        let repo = Repo.v ~user:r.R.user ~repo:r.R.repo in
        List.fold_left (fun acc (id, _) ->
            Commit.Set.add (Commit.v repo id) acc
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
          { User.repos = String.Map.add repo (R.v r) u.User.repos }
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

  let lookup t r =
    let user = match String.Map.find r.Repo.user t.users with
      | Some user -> user
      | None      ->
        let user = User.empty () in
        let users = String.Map.add r.Repo.user user t.users in
        t.users <- users;
        user
    in
    User.lookup user r

  let remove_repo t r =
    let users = String.Map.remove r.Repo.user t.users in
    t.users <- users

  let prune repos t =
    let users = String.Map.map (User.prune repos) t.users in
    let users = String.Map.filter (fun _ u -> not (User.is_empty u)) users in
    { users }

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
    Snapshot.v ~repos ~commits ~status ~prs ~refs

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
      List.map (Event.of_repo `Monitored) (Repo.Set.elements new_repos)
      @ List.map (Event.of_repo `Ignored) (Repo.Set.elements old_repos)
    in
    let prs =
      PR.Set.filter (keep PR.repo) new_prs
      |> PR.Set.elements
      |> List.map Event.of_pr
    in
    let refs =
      Ref.Set.filter (keep Ref.repo) new_refs
      |> Ref.Set.elements
      |> List.map (fun e -> Event.of_ref (`Updated e))
    in
    let status =
      Status.Set.filter (keep Status.repo) new_status
      |> Status.Set.elements
      |> List.map Event.of_status
    in
    let close_prs =
      Snapshot.prs olds
      |> PR.Set.filter
        (fun pr ->
           keep PR.repo pr && not (PR.Set.exists (PR.same_id pr) new_prs))
      |> PR.Set.elements
      |> List.map (fun pr ->
          let title = pr.PR.title in
          let base = pr.PR.base in
          let commit = pr.PR.head in
          let number = pr.PR.number in
          let pr = PR.v ~state:`Closed ~title ~base commit number in
          Event.PR pr)
    in
    let close_refs =
      Snapshot.refs olds
      |> Ref.Set.filter
        (fun r ->
           keep Ref.repo r && not (Ref.Set.exists (Ref.same_id r) new_refs))
      |> Ref.Set.elements
      |> List.map (fun r -> Event.Ref (`Removed (Ref.id r)))
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
  let lookup t r  = Users.lookup t.users r

 let lookup_opt t { Repo.user; repo }  =
   match Users.find user t.users with
   | None      -> None
   | Some user -> String.Map.find repo user.User.repos

  let add_event t e =
    Log.info (fun l -> l "TEST: add_event %a" Event.pp e);
    let r = lookup t (Event.repo e) in
    r.R.events <- e :: r.R.events

  let user_exists t ~user = return (Users.mem user t.users)
  let repo_exists t repo  = return (lookup_opt t repo <> None)

  let repos t ~user =
    match Users.find user t.users with
    | None   -> return []
    | Some u ->
      String.Map.dom u.User.repos
      |> String.Set.elements
      |> List.map (fun repo -> Repo.v ~user ~repo)
      |> return

  let set_repo_aux t (s, r) =
    let t = t.users in
    match s with
    | `Monitored -> if not (Users.mem_repo t r) then Users.add_repo t r
    | `Ignored   -> if Users.mem_repo t r then Users.remove_repo t r

  let set_status_aux t s =
    let repo = lookup t (Status.repo s) in
    let commit = Status.commit_hash s in
    let keep (c, _) = c <> commit in
    let commits = List.filter keep repo.R.commits in
    let rest =
      try
        List.find (fun x -> not (keep x)) repo.R.commits
        |> snd
        |> List.filter (fun y -> Status.context y <> Status.context s)
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
    let repo = lookup t (PR.repo pr) in
    let num = pr.PR.number in
    let prs = List.filter (fun pr -> pr.PR.number <> num) repo.R.prs in
    repo.R.prs <- pr :: prs;
    add_event t (Event.PR pr)

  let set_pr t pr =
    t.ctx.Counter.set_pr <- t.ctx.Counter.set_pr + 1;
    set_pr_aux t pr;
    return ()

  let set_ref_aux t r =
    let repo = lookup t (Ref.repo r) in
    let name = r.Ref.name in
    let refs = List.filter (fun r -> r.Ref.name <> name) repo.R.refs in
    repo.R.refs <- r :: refs;
    add_event t (Event.Ref (`Updated r))

  let set_ref t r =
    t.ctx.Counter.set_ref <- t.ctx.Counter.set_ref + 1;
    set_ref_aux t r;
    return ()

  let remove_ref_aux t (r, name as ref_) =
    let repo = lookup t r in
    let refs = List.filter (fun r -> r.Ref.name <> name) repo.R.refs in
    repo.R.refs <- refs;
    add_event t (Event.Ref (`Removed ref_))

    let remove_ref t repo name =
    t.ctx.Counter.set_ref <- t.ctx.Counter.set_ref + 1;
    remove_ref_aux t (repo, name);
    return ()

  let status t commit =
    t.ctx.Counter.status <- t.ctx.Counter.status + 1;
    match lookup_opt t (Commit.repo commit) with
    | None   -> return []
    | Some r ->
      try return (List.assoc (Commit.hash commit) r.R.commits)
      with Not_found -> return []

  let prs t repo =
    t.ctx.Counter.prs <- t.ctx.Counter.prs + 1;
    match lookup_opt t repo with
    | None   -> return []
    | Some r -> return r.R.prs

  let pr t (repo, _ as id) =
    t.ctx.Counter.prs <- t.ctx.Counter.prs + 1;
    match lookup_opt t repo with
    | None   -> return None
    | Some r ->
      try
        List.find (fun pr -> PR.compare_id id (PR.id pr) = 0) r.R.prs
        |> fun pr -> return @@ Some pr
      with Not_found ->
        return None

  let events t repo =
    t.ctx.Counter.events <- t.ctx.Counter.events + 1;
    match lookup_opt t repo with
    | None   -> return []
    | Some r -> return r.R.events

  let refs t repo =
    t.ctx.Counter.refs <- t.ctx.Counter.refs + 1;
    match lookup_opt t repo with
    | None   -> return []
    | Some r -> return r.R.refs

  let ref t (repo, _ as id) =
    t.ctx.Counter.refs <- t.ctx.Counter.refs + 1;
    match lookup_opt t repo with
    | None   -> return None
    | Some r ->
      try
        List.find (fun r -> Ref.compare_id id (Ref.id r) = 0) r.R.refs
        |> fun r -> return (Some r)
      with Not_found ->
        return None

  let apply_events t =
    t.users |> Users.iter @@ fun _ user ->
    user.User.repos |> String.Map.iter @@ fun _ repo ->
    repo.R.events |> List.iter @@ function
    | Event.Repo r   -> set_repo_aux t r
    | Event.PR pr    -> set_pr_aux t pr
    | Event.Status s -> set_status_aux t s
    | Event.Ref (`Removed r) -> remove_ref_aux t r
    | Event.Ref (`Updated r
                |`Created r) -> set_ref_aux t r
    | Event.Other _  -> ()

  let create ?(events=[]) users =
    let t = { users; ctx = Counter.zero () } in
    List.iter (add_event t) events;
    t

  let all_repos t =
    fold (fun u acc -> Repo.Set.union (User.repos u) acc) t Repo.Set.empty

  module Webhook = struct

    type t = {
      state: token;
      mutable repos: Repo.Set.t;
    }

    let block () = let t, _ = Lwt.task () in t

    let repos t = t.repos
    let run _ = block ()
    let wait _ = block ()

    let create ?repos state =
      let repos = match repos with None -> all_repos state | Some r -> r in
      { state; repos }

    let watch t r =
      if not (Repo.Set.mem r t.repos) then (
        Users.add_repo t.state.users r;
        t.repos <- Repo.Set.add r t.repos;
      );
      Lwt.return_unit

    let events t =
      let x = fold (fun u acc -> User.events u @ acc) t.state [] in
      let x = List.filter (fun x -> Repo.Set.mem (Event.repo x) t.repos) x in
      List.rev x

    let clear t = iter (fun _ -> User.clear) t.state

    let v state _ = { state; repos = Repo.Set.empty  }

  end

end

module Bridge = Datakit_github_sync.Make(API)(DK)
module State = Datakit_github_state.Make(API)

let user = "test"
let repo = "test"
let branch = "test"

let repo = Repo.v ~user ~repo
let commit_bar = Commit.v repo "bar"
let commit_foo = Commit.v repo "foo"
let r1 = Repo.v ~user:"xxx" ~repo:"yyy"

let s1 =
  Status.v ~description:"foo" commit_bar ["foo"; "bar"; "baz"] `Pending

let toto = Uri.of_string "toto"
let titi = Uri.of_string "titi"

let s2 = Status.v ~url:toto commit_bar ["foo"; "bar"; "toto"] `Failure

let s3 =
  Status.v ~url:titi ~description:"foo"
    commit_foo ["foo"; "bar"; "baz"] `Success

let s4 = Status.v commit_bar ["foo"] `Pending

let s5 = Status.v ~url:titi commit_foo ["foo"; "bar"; "baz"] `Failure

let base = "master"

let pr1 = PR.v ~state:`Open   ~title:""     ~base commit_foo 1
let pr2 = PR.v ~state:`Closed ~title:"foo"  ~base commit_foo 1
let pr3 = PR.v ~state:`Open   ~title:"bar"  ~base commit_bar 2
let pr4 = PR.v ~state:`Open   ~title:"toto" ~base commit_bar 2

let ref1 = Ref.v commit_bar ["heads";"master"]
let ref2 = Ref.v commit_foo ["heads";"master"]

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
  (module struct include Diff let equal x y = Diff.compare x y = 0 end)

let dirty = Alcotest.testable Elt.IdSet.pp Elt.IdSet.equal
let counter: Counter.t Alcotest.testable = (module Counter)
let capabilities: Capabilities.t Alcotest.testable = (module Capabilities)
let ref_t = Alcotest.testable Ref.pp (fun x y -> Ref.compare x y = 0)

let mk_snapshot ?(repos=[]) ?(commits=[]) ?(status=[]) ?(prs=[]) ?(refs=[]) () =
  Snapshot.v
    ~repos:(Repo.Set.of_list repos)
    ~commits:(Commit.Set.of_list commits)
    ~status:(Status.Set.of_list status)
    ~prs:(PR.Set.of_list prs)
    ~refs:(Ref.Set.of_list refs)

let mk_diff l =
  let diff acc = function
    | `Update x -> Diff.with_update x acc
    | `Remove x -> Diff.with_remove x acc
  in
  List.fold_left diff Diff.empty l

let mk_dirty = Elt.IdSet.of_list

module Data = struct

  let contexts = [|
    ["ci"; "datakit"; "test"];
    ["ci"; "datakit"; "build"];
    ["ci"; "circleci"];
  |]

  let titles = [| "it works!"; "merge me"; "yay!" |]
  let commits =  [| "123"; "456"; "789"; "0ab"; "abc"; "def" |]
  let bases = [| "master"; "test"; "foo" |]
  let pr_states = [| `Closed; `Open  |]
  let users = [| "a"; "b" |]
  let repos = [| "a"; "b"; "c" |]

  let names = [|
    ["heads"; "master"];
    ["tags" ; "foo"; "bar"];
    ["heads"; "gh-pages"];
  |]

  let descriptions = [|
    Some "Testing...";
    None;
  |]

  let build_states = [|
    `Pending;
    `Success;
    `Failure;
    `Error;
  |]

  let urls = [|
    None;
    Some (Uri.of_string "http://example.com/")
  |]

end

module Gen = struct

  let choose ~random options =
    options.(Random.State.int random (Array.length options))

  let repo ?(x=[]) ~random () =
    let rec aux () =
      let user = choose ~random Data.users in
      let repo = choose ~random Data.repos in
      let r = Repo.v ~user ~repo in
      if List.exists (fun x -> Repo.compare x r = 0) x then aux () else r
    in
    aux ()

  let description ~random = choose ~random Data.descriptions
  let build_state ~random = choose ~random Data.build_states
  let pr_state ~random = choose ~random Data.pr_states
  let base ~random = choose ~random Data.bases
  let title ~random = choose ~random Data.titles
  let url ~random = choose ~random Data.urls
  let context ~random = choose ~random Data.contexts

  let commit ?(x=[]) ?repo:r ~random () =
    let rec aux () =
      let id = choose ~random Data.commits in
      let repo = match r with Some r -> r | None -> repo ~random () in
      let r = Commit.v repo id in
      if List.exists (fun x -> Commit.compare x r = 0) x then aux () else r
    in
    aux ()

  let pr ?(x=[]) ?repo ~random () =
    let rec aux () =
      let head = commit ?repo ~random () in
      let title = title ~random in
      let number = Random.State.int random 10 in
      let state = pr_state ~random in
      let base = base ~random in
      let r = PR.v ~title ~base ~state head number in
      if List.exists (PR.same_id r) x then aux () else r
    in
    aux ()

  let ref ?(x=[]) ?repo ~random () =
    let rec aux () =
      let head = commit ?repo ~random () in
      let name = choose ~random Data.names in
      let r = Ref.v head name in
      if List.exists (Ref.same_id r) x then aux () else r
    in
    aux ()

  let status ?(x=[]) ?repo ~random () =
    let rec aux () =
      let url = url ~random in
      let commit = commit ?repo ~random () in
      let context = context ~random in
      let description = description ~random in
      let state = build_state ~random in
      let r = Status.v ?description ?url commit context state in
      if List.exists (Status.same_id r) x then aux () else r
    in
    aux ()

  let refs ~random ~repo ~old_refs =
    Data.names
    |> Array.to_list
    |> List.map (fun name ->
        if Random.State.bool random then (
          match List.find (fun r -> r.Ref.name = name) old_refs with
          | exception Not_found -> []
          | old_ref ->
            if Random.State.bool random then [] else
              let head = commit ~random ~repo () in
              [ Ref.v head (Ref.name old_ref) ]
        ) else (
          let head = commit ~random ~repo () in
          [ Ref.v head name ]
        ))
    |> List.concat

  let statuses ~random ~old_status commit =
    let old_status = match List.assoc (Commit.hash commit) old_status with
      | exception Not_found -> []
      | old_status -> old_status
    in
    Data.contexts
    |> Array.to_list
    |> List.map (fun context ->
        if Random.State.bool random then (
          match List.find (fun s -> Status.context s = context) old_status with
          | exception Not_found -> []
          | old_status -> [old_status]    (* GitHub can't delete statuses *)
        ) else (
          let state = build_state ~random in
          let description = description ~random in
          [Status.v ?description commit context state]
        ))
    |> List.concat

  let prs ~random ~repo ~old_prs =
    let n_prs = Random.State.int random 4 in
    let old_prs = List.rev old_prs in
    let old_prs =
      List.fold_left (fun prs pr ->
          let mk_title = title and mk_base = base in
          let state = pr.PR.state in
          let title = pr.PR.title in
          let head = pr.PR.head in
          let base = pr.PR.base in
          let n = pr.PR.number in
          let pr = match Random.State.int random 5 with
            | 0 ->
              let state = match pr.PR.state with
                | `Open -> `Closed | `Closed -> `Open
              in
              PR.v ~state ~title ~base head n
            | 1 ->
              let head = commit ~random ~repo () in
              PR.v ~state ~title ~base head n
            | 2 ->
              let title = mk_title ~random in
              PR.v ~state ~title ~base head n
            | 3 ->
              let base = mk_base ~random in
              PR.v ~state ~title ~base head n
            | _ -> pr
          in
          pr :: prs
        ) [] old_prs
    in
    let next_pr =
      Pervasives.ref
        (List.fold_left (fun n pr -> max (PR.number pr + 1) n) 0 old_prs)
    in
    let rec make_prs acc = function
      | 0 -> acc
      | n ->
        let head = commit ~random ~repo () in
        let base = base ~random in
        let number = !next_pr in
        incr next_pr;
        let title = "PR#" ^ string_of_int number in
        let pr = PR.v ~state:`Open ~title ~base head number in
        make_prs (pr :: acc) (n - 1)
    in
    make_prs old_prs n_prs |> List.rev

  let state ~random ~repo ~old_prs ~old_status ~old_refs =
    let prs = prs ~random ~repo ~old_prs in
    let refs = refs ~random ~repo ~old_refs in
    let commits =
      let (++) = Commit.Set.union in
      let l f s = Commit.Set.of_list (List.map f s) in
      l (fun (id, _) -> Commit.v repo id) old_status
      ++ l PR.commit prs ++ l Ref.commit refs
    in
    let commits =
      Commit.Set.fold (fun c acc ->
          match statuses ~random ~old_status c with
          | [] -> acc
          | s  -> (Commit.hash c, s) :: acc
        ) commits []
    in
    prs, commits, refs

  let users ?(old=Users.empty ()) ~random =
    Users.iter (fun _ repo -> User.clear repo) old;
    Data.users |> Array.to_list |> List.map (fun user ->
        let old_user =
          Users.find user old |> default { User.repos = String.Map.empty }
        in
        let repos =
          Data.repos |> Array.to_list |> List.map (fun repo ->
              let r = Repo.v ~user ~repo in
              let old_prs, old_status, old_refs =
                match String.Map.find repo old_user.User.repos with
                | None      -> [], [], []
                | Some repo -> repo.R.prs, repo.R.commits, repo.R.refs
              in
              let prs, commits, refs =
                state ~random ~repo:r ~old_prs ~old_status ~old_refs
              in
              repo, { R.user; repo; commits; prs; refs; events = [] }
            )
          |> String.Map.of_list
        in
        user, { User.repos }
      )
    |> String.Map.of_list
    |> fun users -> { Users.users }

  let snapshot ~random =
    let rec mk acc f = function
      | 0 -> acc
      | n -> mk (f acc) f (n-1)
    in
    let int n = Random.State.int random n in
    let repos = mk Repo.Set.empty (Repo.Set.add (repo ~random ())) (int 2) in
    let commits =
      mk Commit.Set.empty (Commit.Set.add (commit ~random ())) (int 2)
    in
    let prs = mk PR.Set.empty (PR.Set.add (pr ~random ())) (int 4) in
    let refs = mk Ref.Set.empty (Ref.Set.add (ref ~random ())) (int 3) in
    let status =
      mk Status.Set.empty (Status.Set.add (status ~random ())) (int 5)
    in
    Snapshot.v ~repos ~commits ~prs ~refs ~status

end

let commit_diff new_commits old_commits =
  let new_commits = Commit.Set.of_list new_commits in
  let old_commits = Commit.Set.of_list old_commits in
  let updates =
    Commit.Set.diff new_commits old_commits
    |> Commit.Set.elements
    |> List.map (fun c -> `Update (`Commit c))
  in
  let removes =
    Commit.Set.diff old_commits new_commits
    |> Commit.Set.elements
    |> List.map (fun c -> `Remove (`Commit c))
  in
  updates @ removes

let test_basic_snapshot_once random =
  (* repos *)
  let r1 = Gen.repo ~random () in
  let r2 = Gen.repo ~x:[r1] ~random () in
  let r3 = Gen.repo ~x:[r1;r2] ~random () in
  let s1 = mk_snapshot ~repos:[r1; r3] () in
  let s2 = mk_snapshot ~repos:[r2; r3] () in
  let d = Snapshot.diff s1 s2 in
  let x = mk_diff [`Update (`Repo r1); `Remove (`Repo r2)] in
  Alcotest.(check diff) "repos" x d;

  (* prs *)
  let repo = Gen.repo ~random () in
  let pr1 = Gen.pr ~random ~repo () in
  let pr2 = Gen.pr ~x:[pr1] ~random ~repo () in
  let pr3 = Gen.pr ~x:[pr1; pr2] ~random ~repo () in
  let s1 = mk_snapshot ~prs:[pr1; pr3] () in
  let s2 = mk_snapshot ~prs:[pr2; pr3] () in
  let d = Snapshot.diff s1 s2 in
  let x =
    mk_diff ([`Update (`PR pr1); `Remove (`PR (PR.id pr2))]
             @ commit_diff
               [PR.commit pr1; PR.commit pr3]
               [PR.commit pr2; PR.commit pr3])
  in
  Alcotest.(check diff) "prs" x d;

  (* refs *)
  let repo = Gen.repo ~random () in
  let r1 = Gen.ref ~random ~repo () in
  let r2 = Gen.ref ~x:[r1] ~random ~repo () in
  let r3 =
    let head = Gen.commit ~x:[Ref.commit r2] ~repo ~random () in
    Ref.v head (Ref.name r2)
  in
  let s1 = mk_snapshot ~refs:[r2; r1] () in
  let s2 = mk_snapshot ~refs:[r3; r1] () in
  let d = Snapshot.diff s1 s2 in
  let x =
    mk_diff ([`Update (`Ref r2)]
             @ commit_diff
               [Ref.commit r1; Ref.commit r2]
               [Ref.commit r1; Ref.commit r3])
  in
  Alcotest.(check diff) "refs" x d;

  (* status *)
  let repo = Gen.repo ~random () in
  let b1 = Gen.status ~random ~repo () in
  let b2 = Gen.status ~x:[b1] ~random ~repo () in
  let b3 = Gen.status ~x:[b1;b2] ~random ~repo () in
  let s1 = mk_snapshot ~status:[b1; b2] () in
  let s2 = mk_snapshot ~status:[b3] () in
  let d = Snapshot.diff s1 s2 in
  let x =
    mk_diff ([`Update (`Status b2); `Update (`Status b1);
              `Remove (`Status (Status.id b3))] @
             commit_diff
               [Status.commit b1; Status.commit b2]
               [Status.commit b3])
  in
  Alcotest.(check diff) "status" x d;

  (* diff *)
  let s1 = Gen.snapshot ~random in
  let s2 = Gen.snapshot ~random in
  let d = Snapshot.diff s1 s2 in
  let s3 = Diff.apply d s2 in
  Alcotest.(check snapshot) "diff/apply" s1 s3

let test_basic_snapshot () =
  let random = Random.State.make [| 4; 5; 6 |] in
  for _ = 0 to 100 do
    test_basic_snapshot_once random
  done

let cap str = match Capabilities.parse str with
  | `Ok c    -> c
  | `Error e -> Alcotest.fail ("capability " ^ str ^ ": " ^ e)

  let test_capabilities () =
  let caps = [
    "*:rw";
    "repo:r,repo[samoht]:w";
    "*:r,status[foo/bar]:w";
    "pr:rw,status:w";
    "*:w,commit:,webhook:r";
  ] in
  let to_string = Fmt.to_to_string Capabilities.pp in
  List.iter (fun str ->
      let c = cap str in
      Alcotest.(check string) str str (to_string c)
    ) caps;
  let caps = [
    Capabilities.all;
    Capabilities.none;
    Capabilities.(allow all `Read (`Status ["foo";"bar"]));
    Capabilities.(allow all `Write `Commit);
  ] in
  List.iter (fun c ->
      let str = Fmt.to_to_string Capabilities.pp c in
      let d = cap str in
      Alcotest.(check capabilities) str c d
    ) caps;
  let checks = [
    cap "*:rw", `Read , `Status ["foo"], true;
    cap "*:rw", `Write, `Status ["foo"], true;
    cap "*:rw", `Excl , `Status ["foo"], false;
    cap "*:w" , `Read , `PR, false;
    cap "*:w" , `Write, `PR, true;
    cap "*:w" , `Excl , `PR, false;
    cap "*:x" , `Read , `PR, true;
    cap "*:x" , `Write, `PR, true;
    cap "*:x" , `Excl , `PR, true;
    cap "repo[samoht]:w,repo:r", `Read , `Repo ["samoht";"test"], false;
    cap "repo[samoht]:w,repo:r", `Write, `Repo ["samoht";"test"], true;
    cap "*:w,pr:r", `Read , `PR, true;
    cap "*:w,pr:r", `Write, `PR, false;
    cap "*:w,pr:r", `Excl , `PR, false;
    cap "*:w,pr:r", `Read , `Commit, false;
    cap "*:w,pr:r", `Write, `Commit, true;
    cap "*:w,pr:r", `Excl , `Commit, false;
    cap "*:r,status[foo]:x,webhook:w", `Excl, `Status ["foo"], true;
    cap "status[foo/bar]:r", `Read, `Status ["foo";"bar";"0"], true;
    cap "status[foo]:r,status[foo/bar]:w",
    `Write, `Status ["foo";"bar";"0"], true;
  ]
  in
  List.iter (fun (c, op, r, b) ->
      let msg =
        Fmt.strf "%a - %a - %a"
          Capabilities.pp c Capabilities.pp_op op Capabilities.pp_resource r
      in
      Alcotest.(check bool) msg b Capabilities.(check c op r)
    ) checks

let test_snapshot () =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  Test_utils.run (fun _repo conn ->
      let dkt = DK.connect conn in
      DK.branch dkt "test-snapshot" >>*= fun br ->
      let update ~prs ~status ~refs ~dirty =
        DK.Branch.with_transaction br (fun tr ->
            Lwt_list.iter_p (Conv.update_elt tr)
              (`Repo repo ::
               (List.map (fun pr -> `PR pr) prs) @
               (List.map (fun s  -> `Status s) status) @
               (List.map (fun r  -> `Ref r) refs))
            >>= fun () ->
            Conv.stain tr (Elt.IdSet.of_list dirty) >>= fun () ->
            DK.Transaction.commit tr ~message:"init" >>*= fun () ->
            Conv.of_branch ~debug:"init" br >>= fun (_, s) ->
            ok (Conv.snapshot s))
      in
      update ~prs:prs0 ~status:status0 ~refs:refs0 ~dirty:[] >>*= fun s ->
      expect_head br >>*= fun head ->
      let se =
        let prs = PR.Set.of_list prs0 in
        let status = Status.Set.of_list status0 in
        let refs = Ref.Set.of_list refs0 in
        let repos = Repo.Set.of_list repos0 in
        let commits = Commit.Set.of_list commits0 in
        Snapshot.v ~repos ~commits ~prs ~status ~refs
      in
      Conv.of_commit ~debug:"sh" head >>= fun sh ->
      Alcotest.(check snapshot) "snap transaction" se s;
      Alcotest.(check snapshot) "snap head" se (Conv.snapshot sh);

      update ~prs:[pr2] ~status:[] ~refs:[] ~dirty:[`PR (PR.id pr1)]
      >>*= fun s1 ->
      expect_head br >>*= fun head1 ->
      Conv.diff head1 head >>= fun (diff1, dirty1) ->
      Alcotest.(check diff) "diff1" (mk_diff [`Update (`PR pr2)]) diff1;
      Alcotest.(check dirty) "dirty1" (mk_dirty [`PR (PR.id pr1)]) dirty1;
      Conv.of_commit ~debug:"sd" ~old:sh head1 >>= fun sd ->
      Alcotest.(check snapshot) "snap diff" s1 (Conv.snapshot sd);

      update ~prs:[] ~status:[s5] ~refs:[ref2] ~dirty:[`Ref (Ref.id ref2);
                                                       `Repo r1]
      >>*= fun s2 ->
      expect_head br >>*= fun head2 ->
      Conv.diff head2 head1 >>= fun (diff2, dirty2) ->
      Alcotest.(check diff) "diff2"
        (mk_diff [`Update (`Status s5); `Update (`Ref ref2);
                  `Update (`Commit commit_foo)])
        diff2;
      Alcotest.(check dirty) "dirty2"
        (mk_dirty [`Ref (Ref.id ref2); `Repo r1]) dirty2;
      Conv.of_commit ~debug:"sd1" ~old:sh head2 >>= fun sd1 ->
      Conv.of_commit ~debug:"ss2" ~old:sd head2 >>= fun sd2 ->
      Alcotest.(check snapshot) "snap diff1" s2 (Conv.snapshot sd1);
      Alcotest.(check snapshot) "snap diff2" s2 (Conv.snapshot sd2);

      DK.Branch.with_transaction br (fun tr ->
          DK.Transaction.make_dirs tr (p "test/toto") >>*= fun () ->
          DK.Transaction.create_or_replace_file tr (p "test/toto/FOO")
            (v "") >>*= fun () ->
          DK.Transaction.commit tr ~message:"test/foo"
        ) >>*= fun () ->
      expect_head br >>*= fun head3 ->

      Conv.diff head3 head2 >>= fun (diff3, _) ->
      Alcotest.(check diff) "diff3" Diff.empty diff3;

      Lwt.return_unit
    )

let init_github status refs events =
  let tbl = Hashtbl.create (List.length status) in
  List.iter (fun s ->
      let v =
        try Hashtbl.find tbl (Status.commit s)
        with Not_found -> []
      in
      Hashtbl.replace tbl (Status.commit s) (s :: v)
    ) status;
  let commits = Hashtbl.fold (fun k v acc -> (Commit.hash k, v) :: acc) tbl [] in
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
      let dkt = DK.connect conn in
      DK.branch dkt branch >>*= fun br ->
      let gh = init_github [] [] [] in
      let b = Bridge.empty in
      Bridge.sync ~policy:`Once ~token:gh br b >>= fun _s ->
      DK.Branch.with_transaction br (fun tr ->
          Conv.update_elt tr (`Repo repo) >>= fun () ->
          DK.Transaction.commit tr ~message:"init"
        )
      >>*= fun () ->
      f dkt
    )

let check_dirs = Alcotest.(check (slist string String.compare))
let check_data msg x y = Alcotest.(check string) msg x (Cstruct.to_string y)

let check tree =
  (* check test/test/commit *)
  let commit = root repo / "commit" in
  DK.Tree.exists_dir tree commit >>*= fun exists ->
  Alcotest.(check bool) "commit dir exists"  exists true;
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
  check_dirs "pr 2" ["state"; "head"; "title"; "base"] dirs ;
  DK.Tree.read_file tree (pr / "2" / "state") >>*= fun data ->
  check_data "state" "open\n" data;
  DK.Tree.read_file tree (pr / "2" / "head") >>*= fun data ->
  check_data "head" "bar\n" data ;

  Lwt.return_unit

open! Counter

let test_events dk =
  let gh = init_github status0 refs0 events0 in
  let b = Bridge.empty in
  DK.branch dk branch >>*= fun branch ->
  let sync b = Bridge.sync ~policy:`Once ~token:gh branch b in
  Alcotest.(check counter) "counter: 0"
    { events = 0; prs = 0; status = 0; refs = 0;
      set_pr = 0; set_status = 0; set_ref = 0 }
    gh.API.ctx;
  sync b >>= fun b ->
  sync b >>= fun b ->
  sync b >>= fun b ->
  sync b >>= fun b ->
  sync b >>= fun b ->
  sync b >>= fun b ->
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 1; status = 1; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    gh.API.ctx;
  sync b >>= fun _b ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 1; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    gh.API.ctx;
  expect_head branch >>*= fun head ->
  check (DK.Commit.tree head)

let update_status br commit context state =
  DK.Branch.with_transaction br (fun tr ->
      let dir =
        root repo / "commit" / commit / "status"
        /@ Datakit_path.of_steps_exn context
      in
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let state = Cstruct.of_string  (Status_state.to_string state ^ "\n") in
      DK.Transaction.create_or_replace_file tr (dir / "state") state >>*= fun () ->
      DK.Transaction.commit tr ~message:"Test"
    )

let find_status t repo =
  let repo = API.lookup t repo in
  try List.find (fun (c, _) -> c = "foo") repo.R.commits |> snd |> List.hd
  with Not_found -> Alcotest.fail "foo not found"

let find_pr t repo =
  let repo = API.lookup t repo in
  try List.find (fun pr -> pr.PR.number = 2) repo.R.prs
  with Not_found -> Alcotest.fail "foo not found"

let find_ref t repo n =
  let repo = API.lookup t repo in
  try List.find (fun r -> r.Ref.name = n) repo.R.refs
  with Not_found -> Alcotest.fail "foo not found"

let test_cleanup dk =
  let cap = match Capabilities.parse "*:r" with `Ok c -> Some c | _ -> None in
  let b = Bridge.empty in
  DK.branch dk branch >>*= fun branch ->

  let ref1 = Ref.v (Commit.v repo "bar") ["heho"] in
  let gh = init_github [] [ref1] [] in
  Alcotest.(check counter) "counter: 0"
    { events = 0; prs = 0; status = 0; refs = 0;
      set_pr = 0; set_status = 0; set_ref = 0  }
    gh.API.ctx;

  Bridge.sync ?cap ~policy:`Once ~token:gh branch b >>= fun b ->
  expect_head branch >>*= fun head ->
  let tree = DK.Commit.tree head in
  DK.Tree.read_file tree (root repo / "ref" / "heho" / "head")
  >>*= fun v ->
  Alcotest.(check string) "bar" "bar\n" (Cstruct.to_string v);
  DK.Tree.exists_dir tree (root repo / "commit" / "bar" ) >>*= fun c ->
  Alcotest.(check bool) "exists commit bar" false c;
  Alcotest.(check ref_t) "heho is bar" ref1 (find_ref gh repo ["heho"]);
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 1; status = 1; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0  }
    gh.API.ctx;

  let ref2 = Ref.v (Commit.v repo "foo") ["heho"] in
  let gh = init_github [] [] [Event.Ref (`Updated ref2)] in
  let w = API.Webhook.create gh in
  Bridge.sync ?cap ~policy:`Once ~token:gh ~webhook:w branch b >>= fun b ->
  expect_head branch >>*= fun head ->
  let tree = DK.Commit.tree head in
  DK.Tree.read_file tree (root repo / "ref" / "heho" / "head")
  >>*= fun v ->
  Alcotest.(check string) "foo" "foo\n" (Cstruct.to_string v);
  DK.Tree.exists_dir tree (root repo / "commit" / "foo" ) >>*= fun c ->
  Alcotest.(check bool) "exists commit foo" false c;
  Alcotest.(check ref_t) "heho is foo" ref2 (find_ref gh repo ["heho"]);

  expect_head branch >>*= fun head1 ->
  Bridge.sync ?cap ~policy:`Once ~token:gh branch b >>= fun b ->
  expect_head branch >>*= fun head2 ->
  Alcotest.(check commit) "same head" head1 head2;
  Bridge.sync ?cap ~policy:`Once ~token:gh branch b >>= fun b ->
  expect_head branch >>*= fun head2 ->
  Alcotest.(check commit) "same head" head1 head2;
  Bridge.sync ?cap ~policy:`Once ~token:gh branch b >>= fun _b ->
  expect_head branch >>*= fun head2 ->
  Alcotest.(check commit) "same head" head1 head2;

  Lwt.return_unit

let test_updates dk =
  let gh = init_github status0 refs0 events1 in
  let b = Bridge.empty in
  DK.branch dk branch >>*= fun branch ->
  let sync b = Bridge.sync ~policy:`Once ~token:gh branch b in
  Alcotest.(check counter) "counter: 0"
    { events = 0; prs = 0; status = 0; refs = 0;
      set_pr = 0; set_status = 0; set_ref = 0  }
    gh.API.ctx;
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    gh.API.ctx;
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 1'"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    gh.API.ctx;

  (* test status update *)
  let commit_foo = root repo / "commit" / "foo" in
  expect_head branch >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) commit_foo >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  update_status branch "foo" ["foo"; "bar";"baz"] `Pending >>*= fun () ->
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    gh.API.ctx;
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 3"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    gh.API.ctx;
  let status = find_status gh repo in
  Alcotest.(check status_state) "update status" `Pending (Status.state status);

  (* test PR update *)
  let dir = root repo / "pr" / "2" in
  expect_head branch >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist private commit/foo" true exists;
  expect_head branch >>*= fun h ->
  DK.Tree.exists_dir (DK.Commit.tree h) dir >>*= fun exists ->
  Alcotest.(check bool) "exist commit/foo" true exists;
  DK.Branch.with_transaction branch (fun tr ->
      DK.Transaction.create_or_replace_file tr (dir / "title")
        (Cstruct.of_string "hahaha\n")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"Test"
    ) >>*= fun () ->
  sync b >>= fun _b ->
  Alcotest.(check counter) "counter: 4"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 1; set_status = 1; set_ref = 0 }
    gh.API.ctx;
  let pr = find_pr gh repo in
  Alcotest.(check string) "update pr's title" "hahaha" pr.PR.title;
  Lwt.return_unit

let test_startup dk =
  let gh = init_github status0 refs0 events1 in
  let b = Bridge.empty in
  DK.branch dk branch >>*= fun branch ->
  let sync ?cap b =
    let cap = match cap with
      | None   -> None
      | Some s -> match Capabilities.parse s with `Ok s -> Some s | _ -> None
    in
    Bridge.sync ~policy:`Once ~token:gh ?cap branch b
  in
  let dir = root repo / "commit" / "foo" in

  (* start from scratch *)
  Alcotest.(check counter) "counter: 1"
    { events = 0; prs = 0; status = 0; refs = 0;
      set_pr = 0; set_status = 0; set_ref = 0 }
    gh.API.ctx;
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 2"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 0; set_ref = 0 }
    gh.API.ctx;
  update_status branch "foo" ["foo"; "bar";"baz"] `Pending >>*= fun () ->
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 3"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    gh.API.ctx;

  sync b >>= fun b ->
  sync b >>= fun b ->
  sync b >>= fun _b ->
  Alcotest.(check counter) "counter: 3'"
    { events = 0; prs = 1; status = 2; refs = 1;
      set_pr = 0; set_status = 1; set_ref = 0 }
    gh.API.ctx;

  (* restart *)
  let b = Bridge.empty in
  sync b >>= fun b ->
  Alcotest.(check counter) "counter: 4"
    { events = 0; prs = 2; status = 4; refs = 2;
      set_pr = 0; set_status = 1; set_ref = 0 }
    gh.API.ctx;
  sync b >>= fun b ->
  sync b >>= fun _b ->
  Alcotest.(check counter) "counter: 4'"
    { events = 0; prs = 2; status = 4; refs = 2;
      set_pr = 0; set_status = 1; set_ref = 0 }
    gh.API.ctx;

  (* restart with dirty datakit branch + exclusive access  *)
  let b = Bridge.empty in
  let cap = "*:r,status[foo/bar/baz]:x" in
  update_status branch "foo" ["foo"; "bar";"baz"] `Failure >>*= fun () ->
  sync ~cap b >>= fun b ->
  sync ~cap b >>= fun b ->
  sync ~cap b >>= fun b ->
  Alcotest.(check counter) "counter: 5"
    { events = 0; prs = 3; status = 6; refs = 3;
      set_pr = 0; set_status = 2; set_ref = 0 }
    gh.API.ctx;
  let status = find_status gh repo in
  Alcotest.(check status_state) "update status" `Failure (Status.state status);

  sync b >>= fun b ->
  sync b >>= fun _b ->
  Alcotest.(check counter) "counter: 6"
    { events = 0; prs = 3; status = 6; refs = 3;
      set_pr = 0; set_status = 2; set_ref = 0 }
    gh.API.ctx;

  let status_dir = dir / "status" / "foo" / "bar" / "baz" in
  expect_head branch >>*= fun h ->
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
  | Error `Does_not_exist -> None
  | Error x -> failwith (Fmt.to_to_string DK.pp_error x)

let mapo f = function None -> None | Some x -> Some (f x)

let rec read_state ~user ~repo ~commit tree path context =
  DK.Tree.read_dir tree path >>= function
  | Error _ -> Lwt.return []
  | Ok items ->
    let open Datakit_path.Infix in
    DK.Tree.read_file tree (path / "state") >>= begin function
      | Error _ -> Lwt.return []
      | Ok status ->
        opt_read_file tree (path / "description") >>= fun description ->
        opt_read_file tree (path / "target_url") >|= fun url ->
        let state =
          let status = String.trim (Cstruct.to_string status) in
          match Status_state.of_string status with
          | None   -> failwith (Fmt.strf "Bad state %S" status)
          | Some x -> x
        in
        let url = mapo Uri.of_string url in
        let repo = Repo.v ~user ~repo in
        let commit = Commit.v repo commit in
         [ Status.v ?description ?url commit context state]
    end
    >>= fun this_state ->
    items |> Lwt_list.map_p (function
        | "status" | "description" | "target_url" -> Lwt.return []
        | item ->
          read_state ~user ~repo ~commit tree (path / item) (context @ [item])
      )
    >>= fun children ->
    Lwt.return (this_state @ List.concat children)

let read_opt_dir tree path =
  DK.Tree.read_dir tree path >|= function
  | Ok items -> items
  | Error `Does_not_exist -> []
  | Error x -> failwith (Fmt.to_to_string DK.pp_error x)

let read_commits tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "commit"] in
  read_opt_dir tree path >>=
  Lwt_list.map_p (fun commit ->
      let path =
        Datakit_path.of_steps_exn [user; repo; "commit"; commit; "status"]
      in
      read_state ~user ~repo ~commit tree path [] >>= fun states ->
      Lwt.return (commit, states)
    )

let read_prs tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "pr"] in
  read_opt_dir tree path >>=
  Lwt_list.map_p (fun number ->
      let path = Datakit_path.of_steps_exn [user; repo; "pr"; number] in
      let number = int_of_string number in
      let read name =
        DK.Tree.read_file tree (path / name) >>*= fun data ->
        Lwt.return (String.trim (Cstruct.to_string data))
      in
      read "head"  >>= fun head ->
      read "title" >>= fun title ->
      read "base"  >|= fun base ->
      let repo = Repo.v ~user ~repo in
      let head = Commit.v repo head in
      PR.v ~state:`Open ~title ~base head number
    )

let read_refs tree ~user ~repo =
  let path = Datakit_path.of_steps_exn [user; repo; "ref"] in
  let rec aux acc name =
    let path = Datakit_path.(path /@ of_steps_exn name) in
    DK.Tree.read_file tree (path / "head") >|= begin function
      | Error _ -> acc
      | Ok head ->
        let head = String.trim (Cstruct.to_string head) in
        let repo = Repo.v ~user ~repo in
        let head = Commit.v repo head in
        Ref.v head name :: acc
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

let safe_remove t path =
  DK.Transaction.remove t path >|= function
  | Error _ | Ok () -> ()

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

let ensure_datakit_in_sync ~msg github datakit =
  state_of_branch datakit >>= fun dkt_users ->
  Log.info (fun l -> l  "GitHub:@\n@[%a@]@.DataKit:@\n@[%a@]@."
               Users.pp github.API.users Users.pp dkt_users);
  let repos = Users.repos dkt_users in
  let github = Users.prune repos github.API.users in
  let dkt_users = Users.prune repos dkt_users in
  Alcotest.check snapshot (msg ^ "[github-datakit]")
    Snapshot.empty (Users.diff github dkt_users);
  Alcotest.check snapshot (msg ^ "[datakit-github]")
    Snapshot.empty (Users.diff dkt_users github);
  Alcotest.check users msg github dkt_users;
  Lwt.return ()

let ensure_github_in_sync ~msg github datakit =
  Log.info (fun l -> l  "GitHub:@\n@[%a@]@.DataKit:@\n@[%a@]@."
               Users.pp github.API.users Users.pp datakit);
  let repos = Users.repos github.API.users in
  let github = Users.prune repos github.API.users in
  Alcotest.check snapshot (msg ^ "[github-datakit]")
    Snapshot.empty (Users.diff github datakit);
  Alcotest.check snapshot (msg ^ "[datakit-github]")
    Snapshot.empty (Users.diff datakit github);
  Alcotest.check users msg github datakit;
  Lwt.return ()

let all_repos =
  Array.fold_left (fun acc user ->
      Array.fold_left (fun acc repo ->
          Repo.Set.add (Repo.v ~user ~repo) acc
        ) acc Data.repos
    ) Repo.Set.empty Data.users

exception DK_error of DK.error

let monitor repos branch =
  DK.Branch.with_transaction branch (fun t ->
      let monitor ~user ~repo =
        Conv.update_elt t (`Repo (Repo.v ~user ~repo))
      in
      Lwt_list.iter_p (fun { Repo.user; repo } ->
          monitor ~user ~repo
        ) repos
      >>= fun () ->
      DK.Transaction.commit t ~message:"Monitor repos"
    )

let random_monitor ~random branch =
  DK.Branch.with_transaction branch (fun t ->
      let monitor ~user ~repo =
        let elt = `Repo (Repo.v ~user ~repo) in
        match Random.State.bool random with
        | true  -> Conv.update_elt t elt
        | false -> Conv.remove_elt t elt
      in
      Lwt_list.iter_p (fun { Repo.user; repo } ->
          monitor ~user ~repo
        ) (Repo.Set.elements all_repos)
      >>= fun () ->
      DK.Transaction.commit t ~message:"Monitor repos"
    ) >>= function
  | Ok ()   -> Lwt.return_unit
  | Error e -> Lwt.fail_with @@ Fmt.to_to_string DK.pp_error e

(* Generate a random GitHub state and ensure that Datakit converges:
   in that test, the source of truth is GitHub *)
let test_random_github ~quick _repo conn =
  let cap = cap "*:r" in
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  let random = Random.State.make [| 1; 2; 3 |] in
  let dkt = DK.connect conn in
  DK.branch dkt branch >>*= fun branch ->
  let sync (gh, b) =
    let w = API.Webhook.create gh in
    random_monitor ~random branch >>= fun () ->
    Bridge.sync ~cap ~policy:`Once ~token:gh ~webhook:w branch b >|= fun b ->
    Alcotest.(check int) "API.set-*" 0 (Counter.sets gh.API.ctx);
    b
  in
  let nsync ~fresh n (gh, b) =
    let rec aux k (gh, b) =
      let b = if fresh then Bridge.empty else b in
      let gh =
        let users = Gen.users ~random ~old:gh.API.users in
        let events = Users.diff_events users gh.API.users in
        API.create ~events users
      in
      let w = API.Webhook.create gh in
      random_monitor ~random branch >>= fun () ->
      Bridge.sync ~cap ~policy:`Once ~token:gh ~webhook:w branch b >>= fun b ->
      Alcotest.(check int) "API.set-*" 0 (Counter.sets gh.API.ctx);
      let msg = Fmt.strf "update %d (fresh=%b)" (n - k + 1) fresh in
      ensure_datakit_in_sync ~msg gh branch >>= fun () ->
      if k > 1 then aux (k-1) (gh, b) else Lwt.return (gh, b)
    in
    aux n (gh, b)
  in
  let gh = API.create (Gen.users ~random ?old:None) in
  sync (gh, Bridge.empty) >>= fun _ ->
  ensure_datakit_in_sync ~msg:"init" gh branch >>= fun () ->
  let gh = API.create (Gen.users ~random ~old:gh.API.users) in
  sync (gh, Bridge.empty) >>= fun b ->
  ensure_datakit_in_sync ~msg:"update" gh branch >>= fun () ->
  nsync ~fresh:false (if quick then 2 else 10) (gh, b) >>= fun (gh, b) ->
  nsync ~fresh:true (if quick then 2 else 30)  (gh, b) >>= fun (gh, b) ->
  nsync ~fresh:false (if quick then 2 else 20) (gh, b) >>= fun (gh, b) ->
  let users = Users.of_repos (API.all_repos gh) in
  let events = Users.diff_events users gh.API.users in
  let gh = API.create ~events users in
  sync (gh, b) >>= fun _s ->
  ensure_datakit_in_sync ~msg:"empty" gh branch >>= fun () ->
  Lwt.return_unit

(* Generate a random datakit state and ensure that GitHub converges:
   in that test, the source of truth is DataKit. *)
let test_random_datakit ~quick _repo conn =
  let cap = cap "*:x" in
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  let random = Random.State.make [| 1; 2; 3 |] in
  let dkt = DK.connect conn in
  DK.branch dkt branch  >>*= fun branch ->
  Bridge.sync
    ~policy:`Once ~token:(API.create (Users.empty ()))
    ~cap branch Bridge.empty
  >>= fun _ ->
  let update_datakit users =
    let events = Users.diff_events users (Users.empty ()) in
    DK.Branch.with_transaction branch (fun tr ->
        Lwt_list.iter_p (fun { Repo.user; repo } ->
            safe_remove tr Datakit_path.(empty / user / repo)
          ) (Repo.Set.elements all_repos)
        >>= fun () ->
        Lwt_list.iter_p (Conv.update_event tr) events >>= fun () ->
        DK.Transaction.commit tr ~message:"User updates"
      ) >>= function
    | Error e -> Lwt.fail (DK_error e)
    | Ok ()   -> Lwt.return_unit
  in
  let prune = Users.prune all_repos in
  let sync msg users (b, gh) =
    update_datakit users >>= fun () ->
    monitor (Repo.Set.elements (Users.repos users)) branch >>*= fun () ->
    Bridge.sync ~cap ~policy:`Once ~token:gh branch b >>= fun b ->
    Log.debug (fun l -> l "API.set-* = %d" (Counter.sets gh.API.ctx));
    ensure_github_in_sync ~msg gh (prune users) >|= fun () ->
    (b, gh)
  in
  let nsync n users x =
    let rec aux k users x =
      let users = Gen.users ~random ~old:users in
      let msg = Fmt.strf "update %d" (n - k + 1) in
      sync msg users x >>= fun x ->
      if k > 1 then aux (k-1) users x else Lwt.return x
    in
    aux n users x
  in
  let users = Users.empty () in
  let x = Bridge.empty, API.create (Users.empty ()) in
  sync "init" users x >>= fun s ->
  nsync (if quick then 3 else 30) (Users.empty ()) s
  >|= ignore

let runx f () = Test_utils.run f

let test_set = [
  "basic-snapshot", `Quick, test_basic_snapshot;
  "snapshot"      , `Quick, test_snapshot;
  "capabilities"  , `Quick, test_capabilities;
  "cleanup", `Quick, run_with_test_test test_cleanup;
  "events" , `Quick, run_with_test_test test_events;
  "updates", `Quick, run_with_test_test test_updates;
  "startup", `Quick, run_with_test_test test_startup;
  "random-github"   , `Quick, runx (test_random_github  ~quick:true);
  "random-github-*" , `Slow , runx (test_random_github  ~quick:false);
  "random-datakit"  , `Quick, runx (test_random_datakit ~quick:true);
  "random-datakit-*", `Slow , runx (test_random_datakit ~quick:false);
]

let () =
  Alcotest.run "datakit-github" [
    "github" , test_set;
  ]
