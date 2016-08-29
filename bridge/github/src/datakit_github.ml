open Result
open Astring

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

module type ELT = sig
  include Set.OrderedType
  val pp: t Fmt.t
end

module type SET = Set.S

module Set (E: ELT) = struct

  include Set.Make(E)

  let pp ppf t = Fmt.(list ~sep:(unit "@;") E.pp) ppf (elements t)

  let index t f =
    let tbl = Hashtbl.create (cardinal t) in
    iter (fun x ->
        let i = f x in
        let v =
          try Hashtbl.find tbl i
          with Not_found -> []
        in
        Hashtbl.replace tbl i (x :: v)
      ) t;
    tbl

  exception Found of elt

  let findf f t =
    try iter (fun e -> if f e then raise (Found e)) t; None
    with Found e -> Some e

end

let pp_path = Fmt.(list ~sep:(unit "/") string)

module Repo = struct
  type t = { user: string; repo: string }
  let pp ppf t = Fmt.pf ppf "%s/%s" t.user t.repo
  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)
end

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

module Commit = struct

  type t = { repo: Repo.t; id : string }

  let pp ppf t = Fmt.pf ppf "%a:%s" Repo.pp t.repo t.id
  let id t = t.id
  let repo t = t.repo

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

end

module PR = struct

  type t = {
    head: Commit.t;
    number: int;
    state: [`Open | `Closed];
    title: string;
  }

  let compare: t -> t -> int = Pervasives.compare

  let string_of_state = function
    | `Open   -> "open"
    | `Closed -> "closed"

  let state_of_string  = function
    | "open"   -> Some `Open
    | "closed" -> Some `Closed
    | _        -> None

  let pp_state ppf = function
    | `Open   -> Fmt.string ppf "open"
    | `Closed -> Fmt.string ppf "closed"

  let repo t = t.head.Commit.repo
  let commit t = t.head
  let commit_id t = t.head.Commit.id

  let pp ppf t =
    Fmt.pf ppf "{%a %d[%s] %a %S}"
      Repo.pp (repo t) t.number (commit_id t) pp_state t.state t.title

  let number t = t.number
  let title t = t.title
  let state t = t.state
  let same x y = repo x = repo y && number x = number y

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

end

module Status = struct

  type t = {
    commit: Commit.t;
    context: string list;
    url: string option;
    description: string option;
    state: Status_state.t;
  }

  let compare: t -> t -> int = Pervasives.compare

  let context t = match t.context with
    | [] -> ["default"]
    | l  -> l

  let path s = Datakit_path.of_steps_exn (context s)
  let repo t = t.commit.Commit.repo
  let commit t = t.commit
  let commit_id t = t.commit.Commit.id
  let same x y = commit x = commit y && context x = context y


  let pp_opt k ppf v = match v with
    | None   -> ()
    | Some v -> Fmt.pf ppf " %s=%s" k v

  let pp ppf t =
    Fmt.pf ppf "{%a %s:%a[%a]%a%a}"
      Repo.pp (repo t) (commit_id t)
      pp_path t.context
      Status_state.pp t.state
      (pp_opt "url") t.url
      (pp_opt "descr") t.description

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

end

module Ref = struct

  type t = {
    head: Commit.t;
    name: string list;
  }

  let compare: t -> t -> int  = Pervasives.compare
  let repo t = t.head.Commit.repo
  let commit t = t.head
  let commit_id t = t.head.Commit.id
  let name t = t.name
  let same x y = repo x = repo y && name x = name y
  let path s = Datakit_path.of_steps_exn s.name

  let pp ppf t =
    Fmt.pf ppf "{%a %a[%s]}" Repo.pp (repo t) pp_path t.name (commit_id t)

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

  type state = [`Created | `Updated | `Deleted]

  let pp_state ppf = function
    | `Created -> Fmt.string ppf "+"
    | `Updated -> Fmt.string ppf "*"
    | `Deleted -> Fmt.string ppf "-"

end

module Event = struct

  type t =
    | PR of PR.t
    | Status of Status.t
    | Ref of (Ref.state * Ref.t)
    | Other of (Repo.t * string)

  let pp ppf = function
    | PR pr    -> Fmt.pf ppf "PR: %a" PR.pp pr
    | Status s -> Fmt.pf ppf "Status: %a" Status.pp s
    | Ref(s,r) -> Fmt.pf ppf "Ref: %a%a" Ref.pp_state s Ref.pp r
    | Other o  -> Fmt.pf ppf "Other: %s" @@ snd o

  let repo = function
    | PR pr    -> PR.repo pr
    | Status s -> Status.repo s
    | Ref r    -> Ref.repo (snd r)
    | Other o  -> fst o

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)
end

module type API = sig
  type token
  type 'a result = ('a, string) Result.result Lwt.t
  val user_exists: token -> user:string -> bool result
  val repo_exists: token -> Repo.t -> bool result
  val repos: token -> user:string -> Repo.t list result
  val status: token -> Commit.t -> Status.t list result
  val set_status: token -> Status.t -> unit result
  val set_pr: token -> PR.t -> unit result
  val prs: token -> Repo.t -> PR.t list result
  val refs: token -> Repo.t -> Ref.t list result
  val events: token -> Repo.t -> Event.t list result
  module Webhook: sig
    type t
    val create: token -> Uri.t -> t
    val run: t -> unit Lwt.t
    val repos: t -> Repo.Set.t
    val watch: t -> Repo.t -> unit Lwt.t
    val events: t -> Event.t list
    val clear: t -> unit
  end
end

open Lwt.Infix
open Datakit_path.Infix

let ( >>*= ) x f =
  x >>= function
  | Ok x         -> f x
  | Error _ as e -> Lwt.return e

let ( >|*= ) x f =
  x >|= function
  | Ok x         -> Ok (f x)
  | Error _ as e -> e

let ok x = Lwt.return (Ok x)

let list_iter_s f l =
  Lwt_list.map_s f l >|= fun l ->
  List.fold_left (fun acc x -> match acc, x with
      | Ok (), Ok ()            -> Ok ()
      | Error e, _ | _, Error e -> Error e
    ) (Ok ()) (List.rev l)

let list_map_s f l =
  Lwt_list.map_s f l >|= fun l ->
  List.fold_left (fun acc x -> match acc, x with
      | Ok acc, Ok x            -> Ok (x :: acc)
      | Error e, _ | _, Error e -> Error e
    ) (Ok []) (List.rev l)

module Snapshot = struct

  type t = {
    repos  : Repo.Set.t;
    commits: Commit.Set.t;
    status : Status.Set.t;
    prs    : PR.Set.t;
    refs   : Ref.Set.t;
  }

  let repos t = t.repos
  let status t = t.status
  let prs t = t.prs
  let refs t = t.refs
  let commits t = t.commits

  let empty =
    { repos = Repo.Set.empty;
      commits = Commit.Set.empty;
      status = Status.Set.empty;
      prs = PR.Set.empty;
      refs = Ref.Set.empty }

  let union x y = {
    repos   = Repo.Set.union x.repos y.repos;
    commits = Commit.Set.union x.commits y.commits;
    status  = Status.Set.union x.status y.status;
    prs     = PR.Set.union x.prs y.prs;
    refs    = Ref.Set.union x.refs y.refs;
  }

  let create ~repos  ~commits ~status ~prs ~refs =
    let repos, commits =
      Status.Set.fold (fun x (repos, commits) ->
          Repo.Set.add (Status.repo x) repos,
          Commit.Set.add (Status.commit x) commits
        ) status (repos, commits)
    in
    let repos, commits =
      PR.Set.fold (fun x (repos, commits) ->
          Repo.Set.add (PR.repo x) repos,
          Commit.Set.add (PR.commit x) commits
        ) prs (repos, commits)
    in
    let repos, commits =
      Ref.Set.fold (fun x (repos, commits) ->
          Repo.Set.add (Ref.repo x) repos,
          Commit.Set.add (Ref.commit x) commits
        ) refs (repos, commits)
    in
    { repos; commits; status; prs; refs }

  let compare_repos x y = Repo.Set.compare x.repos y.repos
  let compare_commits x y = Commit.Set.compare x.commits y.commits
  let compare_status x y = Status.Set.compare x.status y.status
  let compare_prs x y = PR.Set.compare x.prs y.prs
  let compare_refs x y = Ref.Set.compare x.refs y.refs

  let compare_fold fs x y =
    List.fold_left (fun acc f ->
        match acc with
        | 0 -> f x y
        | i -> i
      ) 0 (List.rev fs)

  let compare = compare_fold [
      compare_repos;
      compare_commits;
      compare_status;
      compare_prs;
      compare_refs
    ]

  let pp ppf t =
    if compare t empty = 0 then Fmt.string ppf "empty"
    else
      Fmt.pf ppf "{@[<2>repos:%a@]@;@[<2>commits:%a@]@;@[<2>status:%a@]@;\
                  @[<2>prs:%a@]@;@[<2>refs:%a@]}"
        Repo.Set.pp t.repos Commit.Set.pp t.commits Status.Set.pp t.status
        PR.Set.pp t.prs Ref.Set.pp t.refs

  let remove_commit t (r, id) =
    let keep x = r <> Commit.repo x || id <> Commit.id x in
    { t with commits = Commit.Set.filter keep t.commits }

  let add_commit c t =
    let commits = Commit.Set.add c t.commits in
    let repos   = Repo.Set.add (Commit.repo c) t.repos in
    { t with repos; commits }

  let replace_commit c t =
    let id = Commit.repo c, Commit.id c in
    add_commit c (remove_commit t id)

  let remove_pr t (r, id) =
    let keep pr = r  <> PR.repo pr || id <>  pr.PR.number in
    { t with prs = PR.Set.filter keep t.prs }

  let add_pr pr t =
    let prs     = PR.Set.add pr t.prs in
    let repos   = Repo.Set.add (PR.repo pr) t.repos in
    let commits = Commit.Set.add (PR.commit pr) t.commits in
    { t with prs; repos; commits }

  let replace_pr pr t =
    let id = PR.repo pr, pr.PR.number in
    add_pr pr (remove_pr t id)

  let remove_status t (s, l) =
    let keep x = s <> Status.commit x || l <> x.Status.context in
    { t with status = Status.Set.filter keep t.status }

  let add_status s t =
    let status  = Status.Set.add s t.status in
    let repos   = Repo.Set.add (Status.repo s) t.repos in
    let commits = Commit.Set.add (Status.commit s) t.commits in
    { t with status; repos; commits }

  let replace_status s t =
    let cc = s.Status.commit, s.Status.context in
    add_status s (remove_status t cc)

  let remove_ref t (r, l) =
    let keep x = r <> Ref.repo x || l <> x.Ref.name in
    { t with refs = Ref.Set.filter keep t.refs }

  let add_ref r t =
    let refs    = Ref.Set.add r t.refs in
    let repos   = Repo.Set.add (Ref.repo r) t.repos in
    { t with refs; repos }

  let replace_ref r t =
    let name = Ref.repo r, r.Ref.name in
    add_ref r (remove_ref t name)

  let replace_event t = function
    | Event.PR pr             -> replace_pr pr t
    | Event.Ref (`Deleted, r) -> remove_ref t (Ref.repo r, Ref.name r)
    | Event.Ref (_, r)        -> replace_ref r t
    | Event.Status s          -> replace_status s t
    | Event.Other _           -> t

  (* [prune t] is [t] with all the closed PRs pruned. *)
  let prune t =
    let status = Status.Set.index t.status Status.repo in
    let prs = PR.Set.index t.prs PR.repo in
    let refs = Ref.Set.index t.refs Ref.repo in
    let commits = Commit.Set.index t.commits Commit.repo in
    let find r x = try Hashtbl.find x r with Not_found -> []  in
    let aux repo =
      let status  = find repo status  |> Status.Set.of_list in
      let prs     = find repo prs     |> PR.Set.of_list in
      let refs    = find repo refs    |> Ref.Set.of_list in
      let commits = find repo commits |> Commit.Set.of_list in
      let open_prs, closed_prs =
        PR.Set.fold (fun pr (open_prs, closed_prs) ->
            match pr.PR.state with
            | `Open   -> PR.Set.add pr open_prs, closed_prs
            | `Closed -> open_prs, PR.Set.add pr closed_prs
          ) prs (PR.Set.empty, PR.Set.empty)
      in
      Log.debug (fun l -> l "[prune]+prs:@;%a" PR.Set.pp open_prs);
      Log.debug (fun l -> l "[prune]-prs:@;%a" PR.Set.pp closed_prs);
      let is_commit_open c =
        PR.Set.exists (fun pr -> PR.commit pr = c) open_prs
        || Ref.Set.exists (fun r -> Ref.commit r = c) refs
      in
      let open_commits, closed_commits =
        Commit.Set.fold (fun c (open_commit, closed_commit) ->
            match is_commit_open c with
            | false -> open_commit, Commit.Set.add c closed_commit
            | true  -> Commit.Set.add c open_commit, closed_commit
          ) commits (Commit.Set.empty, Commit.Set.empty)
      in
      Log.debug (fun l -> l "[prune]+commits:@;%a" Commit.Set.pp open_commits);
      Log.debug (fun l -> l "[prune]-commits:@;%a" Commit.Set.pp closed_commits);
      let is_status_open s =
        Commit.Set.exists (fun c -> s.Status.commit = c ) open_commits
      in
      let open_status, closed_status =
        Status.Set.fold (fun s (open_status, closed_status) ->
            match is_status_open s with
            | false -> open_status, Status.Set.add s closed_status
            | true  -> Status.Set.add s open_status, closed_status
          ) status (Status.Set.empty, Status.Set.empty)
      in
      let cleanup = {
        repos   = Repo.Set.empty;
        refs    = Ref.Set.empty;
        prs     = closed_prs;
        status  = closed_status;
        commits = closed_commits;
      } in
      Log.debug (fun l -> l "[prune]+status:@;%a" Status.Set.pp open_status);
      Log.debug (fun l -> l "[prune]-status:@;%a" Status.Set.pp closed_status);
      let repos   = Repo.Set.singleton repo in
      let status  = open_status in
      let prs     = open_prs in
      let commits = open_commits in
      let t = { repos; status; prs; refs; commits } in
      let cleanup =
        if PR.Set.is_empty closed_prs && Commit.Set.is_empty closed_commits
        then `Clean
        else `Prune cleanup
      in
      (t, cleanup)
    in
    (* status cannot be removed, so simply monitor updates in
       [new_status]. *)
    let result, cleanup =
      Repo.Set.fold (fun r (result, cleanup) ->
          let (x, c) = aux r in
          let result = union result x in
          let cleanup = match c with
            | `Clean   -> cleanup
            | `Prune c -> union cleanup c
          in
          result, cleanup
        ) t.repos (empty, empty)
    in
    if PR.Set.is_empty cleanup.prs && Commit.Set.is_empty cleanup.commits then (
      assert (compare t result = 0);
      result, None
    ) else
      result, Some cleanup

  let filter repos t =
    let keep f r = not (Repo.Set.mem (f r) repos) in
    let repos = Repo.Set.diff t.repos repos in
    let prs = PR.Set.filter (keep PR.repo) t.prs in
    let refs = Ref.Set.filter (keep Ref.repo) t.refs in
    let commits = Commit.Set.filter (keep Commit.repo) t.commits in
    let status = Status.Set.filter (keep Status.repo) t.status in
    { repos; prs; refs; commits; status }

end

module Diff = struct

  type id = [
    | `PR of int
    | `Status of string * string list
    | `Ref of string list
    | `Unknown
  ]

  type t = {
    repo: Repo.t;
    id  : id;
  }

  let pp ppf t =
    match t.id with
    | `Unknown       -> Fmt.pf ppf "%a:?" Repo.pp t.repo
    | `PR n          -> Fmt.pf ppf "%a:%d" Repo.pp t.repo n
    | `Ref l         -> Fmt.pf ppf "%a:%a" Repo.pp t.repo pp_path l
    | `Status (s, l) -> Fmt.pf ppf "%a:%s[%a]" Repo.pp t.repo s pp_path l

  let compare: t -> t -> int = Pervasives.compare

  module Set = Set(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)

  let path_of_diff = function
    | `Added f | `Removed f | `Updated f -> Datakit_path.unwrap f

  let repos diff =
    List.fold_left (fun acc d ->
        match path_of_diff d with
        | user :: repo :: _ -> Repo.Set.add { Repo.user; repo } acc
        | _ -> acc
      ) Repo.Set.empty diff

  let changes diff =
    let without_last l = List.rev (List.tl (List.rev l)) in
    List.fold_left (fun acc d ->
        let t = match path_of_diff d with
          | user :: repo :: "pr" :: id :: _ ->
            let repo = { Repo.user; repo } in
            Some { repo; id = `PR (int_of_string id) }
          | user :: repo :: "commit" :: id :: "status" :: (_ :: _ :: _ as tl) ->
            let repo = { Repo.user; repo } in
            Some { repo; id = `Status (id, without_last tl) }
          | user :: repo :: "ref" :: ( _ :: _ :: _ as tl)  ->
            let repo = { Repo.user; repo } in
            Some { repo; id = `Ref (without_last tl) }
          | user :: repo :: _ ->
            let repo = { Repo.user; repo } in
            Some { repo; id = `Unknown }
          | _ -> None
        in
        match t with
        | None   -> acc
        | Some t -> Set.add t acc
      ) Set.empty diff

end

module Conv (DK: Datakit_S.CLIENT) = struct

  let safe_remove t path =
    DK.Transaction.exists t path >>*= fun exists ->
    if not exists then ok () else DK.Transaction.remove t path

  type nonrec 'a result = ('a, DK.error) result Lwt.t

  (* conversion between GitHub and DataKit states. *)

  module type TREE = sig
    include Datakit_S.READABLE_TREE with type 'a or_error := 'a DK.or_error
    val diff: DK.Commit.t -> Diff.Set.t result
  end

  type tree = E: (module TREE with type t = 'a) * 'a -> tree

  let error fmt = Fmt.kstrf (fun str -> DK.error "conv: %s" str) fmt

  let walk
      (type elt) (type t) (module Set: SET with type elt = elt and type t = t)
      tree root (file, fn) =
    let E ((module Tree), t) = tree in
    let rec aux context =
      let ctx = match Datakit_path.of_steps context with
        | Ok x    -> ok x
        | Error e -> error "%s" e
      in
      ctx >>*= fun ctx ->
      let dir = root /@ ctx in
      Tree.exists_dir t dir >>*= fun exists ->
      if not exists then ok Set.empty
      else (
        Tree.read_dir t dir >>*= fun child ->
        list_map_s (fun c -> aux (context @ [c])) child >>*= fun child ->
        let child = List.fold_left Set.union Set.empty child in
        Tree.exists_file t (dir / file) >>*= fun exists ->
        if exists then
          fn context >>*= function
          | None   -> ok child
          | Some s -> ok (Set.add s child)
        else
          ok child
      )
    in
    aux []

  let tree_of_commit c =
    let module Tree = struct
      include DK.Tree
      let diff x = DK.Commit.diff c x >>*= fun d -> ok (Diff.changes d)
    end in
    E ((module Tree), DK.Commit.tree c)

  let tree_of_transaction tr =
    let module Tree = struct
      include DK.Transaction
      let diff x = DK.Transaction.diff tr x >>*= fun d -> ok (Diff.changes d)
    end in
    E ((module Tree), tr)

  let empty = Datakit_path.empty

  (* Repos *)

  let repos (E ((module Tree), tree)) =
    Log.debug (fun l -> l "repos");
    let root = Datakit_path.empty in
    Tree.exists_dir tree root >>*= fun is_dir ->
    if not is_dir then ok Repo.Set.empty
    else
      Tree.read_dir tree root >>*= fun users ->
      List.fold_left (fun acc user ->
          Tree.exists_dir tree (root / user) >>*= fun is_dir ->
          if not is_dir then acc
          else
            Tree.read_dir tree (root / user) >>*= fun repos ->
            List.fold_left (fun acc repo ->
                acc >>*= fun acc ->
                Repo.Set.add { Repo.user; repo } acc
                |> ok
              ) acc repos
        ) (ok Repo.Set.empty) users

  (* PRs *)

  let root r = empty / r.Repo.user / r.Repo.repo

  let update_pr t pr =
    let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
    Log.debug (fun l -> l "update_pr %s" @@ Datakit_path.to_hum dir);
    match pr.PR.state with
    | `Closed -> safe_remove t dir
    | `Open   ->
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let head = Cstruct.of_string (PR.commit_id pr ^ "\n")in
      let state = Cstruct.of_string (PR.string_of_state pr.PR.state ^ "\n") in
      let title = Cstruct.of_string (pr.PR.title ^ "\n") in
      DK.Transaction.create_or_replace_file t ~dir "head" head >>*= fun () ->
      DK.Transaction.create_or_replace_file t ~dir "state" state >>*= fun () ->
      DK.Transaction.create_or_replace_file t ~dir "title" title

  let update_prs tr prs = list_iter_s (update_pr tr) (PR.Set.elements prs)

  let pr (E ((module Tree), t)) repo number =
    let dir = root repo / "pr" / string_of_int number in
    Log.debug (fun l -> l "read_pr %s" @@ Datakit_path.to_hum dir);
    Tree.exists_file t (dir / "head")  >>*= fun exists_head ->
    Tree.exists_file t (dir / "state") >>*= fun exists_state ->
    Tree.exists_file t (dir / "title") >>*= fun exists_title ->
    if not exists_head then
      Log.debug (fun l ->
          l "error: %a/pr/%d/head does not exist" Repo.pp repo number);
    if not exists_state then
      Log.debug (fun l ->
          l "error: %a/pr/%d/state does not exist" Repo.pp repo number);
    if not exists_head || not exists_state then ok None
    else (
      Tree.read_file t (dir / "head") >>*= fun head ->
      Tree.read_file t (dir / "state") >>*= fun state ->
      (if not exists_title then ok (Cstruct.of_string "")
       else Tree.read_file t (dir / "title"))
      >>*= fun title ->
      let parse s = String.trim (Cstruct.to_string s) in
      let head = { Commit.repo; id = parse head } in
      let title = parse title in
      let state = parse state in
      match PR.state_of_string state with
      | None       -> error "%s is not a valid PR state" state
      | Some state ->
        ok (Some { PR.head; number; state; title })
    )

  let prs_of_repo tree repo =
    Log.debug (fun l -> l "prs_of_repo %a" Repo.pp repo);
    let E ((module Tree), t) = tree in
    let dir = root repo / "pr"  in
    Log.debug (fun l -> l "read_prs %s" @@ Datakit_path.to_hum dir);
    Tree.exists_dir t dir >>*= fun exists ->
    if not exists then ok PR.Set.empty
    else
      Tree.read_dir t dir >>*=
      list_map_s (fun num -> pr tree repo (int_of_string num))
      >>*= function l ->
        List.fold_left
          (fun acc pr -> match pr with None -> acc | Some x -> PR.Set.add x acc)
          PR.Set.empty l
        |> ok

  let prs ?repos:rs tree =
    Log.debug (fun l -> l "prs");
    (match rs with None -> repos tree | Some rs -> ok rs) >>*= fun repos ->
    list_map_s (prs_of_repo tree) (Repo.Set.elements repos)
    >>*= fun prs ->
    ok (List.fold_left PR.Set.union PR.Set.empty prs)

  (* Commits *)

  let commit (E ((module Tree), t)) repo id =
    let dir = root repo / "commit" / id in
    Tree.exists_dir t dir >|*= fun exists ->
    Log.debug (fun l -> l "commit %a %b" Datakit_path.pp dir exists);
    if not exists then None
    else Some { Commit.repo; id }

  let commits_of_repo tree repo =
    Log.debug (fun l -> l "commits_of_repo %a" Repo.pp repo);
    let E ((module Tree), t) = tree in
    let dir = root repo / "commit" in
    Tree.exists_dir t dir >>*= fun exists ->
    if not exists then ok Commit.Set.empty
    else
      Tree.read_dir t dir >|*= fun childs ->
      List.fold_left (fun s id ->
          Commit.Set.add { Commit.repo; id } s
        ) Commit.Set.empty childs

  let commits ?repos:rs tree =
    Log.debug (fun l -> l "commits");
    (match rs with None -> repos tree | Some rs -> ok rs) >>*= fun repos ->
    list_map_s (commits_of_repo tree) (Repo.Set.elements repos) >>*= fun rs ->
    ok (List.fold_left Commit.Set.union Commit.Set.empty rs)

  (* Status *)

  let update_status t s =
    let dir = root (Status.repo s) / "commit" / (Status.commit_id s)
              / "status" /@ Status.path s
    in
    Log.debug (fun l -> l "update_status %s" @@ Datakit_path.to_hum dir);
    DK.Transaction.make_dirs t dir >>*= fun () ->
    let kvs = [
      "description", s.Status.description;
      "state"      , Some (Status_state.to_string s.Status.state);
      "target_url" , s.Status.url;
    ] in
    list_iter_s (fun (k, v) -> match v with
        | None   -> safe_remove t (dir / k)
        | Some v ->
          let v = Cstruct.of_string (v ^ "\n") in
          DK.Transaction.create_or_replace_file t ~dir k v
      ) kvs

  let update_statuses tr s =
    list_iter_s (update_status tr) (Status.Set.elements s)

  let status (E ((module Tree), t)) commit context =
    let context = Datakit_path.of_steps_exn context in
    let dir =
      root (Commit.repo commit) / "commit" / Commit.id commit / "status"
      /@ context
    in
    Tree.exists_dir t dir >>*= fun exists_dir ->
    Log.debug (fun l -> l "status %a %b" Datakit_path.pp dir exists_dir);
    if not exists_dir then ok None
    else
      Tree.read_file t (dir / "state") >>*= fun state ->
      match Status_state.of_string (String.trim (Cstruct.to_string state)) with
      | None       -> error "%s: invalid state" @@ Cstruct.to_string state
      | Some state ->
        let read file =
          let some s = match String.trim s with "" -> None | s -> Some s in
          Tree.exists_file t file >>*= function
          | false -> ok None
          | true  ->
            Tree.read_file t file >>*= fun d ->
            ok (some @@ Cstruct.to_string d)
        in
        read (dir / "description") >>*= fun description ->
        read (dir / "target_url")  >>*= fun url ->
        let context = Datakit_path.unwrap context in
        Some { Status.state; commit; context; description; url }
        |> ok

  let statuses_of_commits tree commits =
    Log.debug (fun l -> l "statuses_of_commits");
    list_map_s (fun commit ->
        let dir = root (Commit.repo commit) / "commit" in
        Log.debug (fun l -> l "status_of_commit %a" Commit.pp commit);
        let dir = dir / Commit.id commit / "status" in
        walk (module Status.Set) tree dir ("state", status tree commit)
      ) (Commit.Set.elements commits)
    >>*= fun status ->
    ok (List.fold_left Status.Set.union Status.Set.empty status)

  let statuses ?commits:cs tree =
    Log.debug (fun l -> l "status");
    (match cs with None -> commits tree | Some c -> ok c) >>*= fun commits ->
    statuses_of_commits tree commits

  (* Refs *)

  let ref_ (E ((module Tree), t)) repo name =
    let path = Datakit_path.of_steps_exn name in
    let head = root repo / "ref" /@ path / "head" in
    Tree.exists_file t head >>*= fun exists_head ->
    Log.debug (fun l -> l "ref %a %b" Datakit_path.pp head exists_head);
    if not exists_head then ok None
    else
      Tree.read_file t head >>*= fun head ->
      let id = String.trim (Cstruct.to_string head) in
      let head = { Commit.repo; id } in
      ok (Some { Ref.head; name })

  let refs_of_repo tree repo =
    Log.debug (fun l -> l "refs_of_repo %a" Repo.pp repo);
    let E ((module Tree), t) = tree in
    let dir = root repo / "ref" in
    Tree.exists_dir t dir >>*= fun exists ->
    if not exists then ok Ref.Set.empty
    else walk (module Ref.Set) tree dir ("head", ref_ tree repo)

  let refs ?repos:rs tree =
    Log.debug (fun l -> l "refs");
    (match rs with None -> repos tree | Some rs -> ok rs) >>*= fun repos ->
    list_map_s (refs_of_repo tree) (Repo.Set.elements repos) >>*= fun rs ->
    ok (List.fold_left Ref.Set.union Ref.Set.empty rs)

  let update_ref tr (s, r) =
    Log.debug (fun l -> l "update_ref %a%a" Ref.pp_state s Ref.pp r);
    let path = Datakit_path.of_steps_exn (Ref.name r) in
    let dir = root (Ref.repo r) / "ref" /@ path in
    match s with
    | `Deleted -> safe_remove tr dir
    | `Created | `Updated ->
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let head = Cstruct.of_string (Ref.commit_id r ^ "\n") in
      DK.Transaction.create_or_replace_file tr ~dir "head" head

  let update_refs tr rs =
    list_iter_s (fun r -> update_ref tr (`Updated, r)) (Ref.Set.elements rs)

  let update_event t = function
    | Event.PR pr    -> update_pr t pr
    | Event.Status s -> update_status t s
    | Event.Ref r    -> update_ref t r
    | Event.Other o  ->
      Log.debug (fun l -> l "ignoring event: %s" @@ snd o);
      ok ()

  (* Diffs *)

  let diff (E ((module Tree), _)) c = Tree.diff c

  let apply_pr_diff t tree (r, id as x)  =
    pr tree r id >>*= function
    | None    -> Snapshot.remove_pr t x   |> ok
    | Some pr -> Snapshot.replace_pr pr t |> ok

  let apply_status_diff t tree (c, context as x) =
    status tree c context >>*= function
    | None   -> Snapshot.remove_status t x  |> ok
    | Some s -> Snapshot.replace_status s t |> ok

  let apply_ref_diff t tree (r, name as x) =
    ref_ tree r name >>*= function
    | None   -> Snapshot.remove_ref t x  |> ok
    | Some r -> Snapshot.replace_ref r t |> ok

  let apply_commit_diff t tree (r, id as x) =
    commit tree r id >>*= function
    | None   -> Snapshot.remove_commit t x  |> ok
    | Some c -> Snapshot.replace_commit c t |> ok

  let apply (t:Snapshot.t) (tree, diff) =
    let t = ref t in
    list_iter_s (fun ({ Diff.repo; id } as d) ->
        Log.debug (fun l -> l "apply %a" Diff.pp d);
        match id with
        | `PR pr ->
          apply_pr_diff !t tree (repo, pr) >>*= fun x ->
          t := x;
          ok ()
        | `Status (id, context) ->
          let commit = { Commit.repo; id } in
          apply_status_diff !t tree (commit, context) >>*= fun x ->
          apply_commit_diff x tree (repo, id) >>*= fun x ->
          t := x;
          ok ()
        | `Ref name ->
          apply_ref_diff !t tree (repo, name) >>*= fun x ->
          t := x;
          ok ()
        | `Unknown ->
          let repos = Repo.Set.add repo !t.Snapshot.repos in
          t := { !t with Snapshot.repos };
          ok ()
      ) (Diff.Set.elements diff)
    >>*= fun () ->
    ok !t

  (* Snapshot *)

  let snapshot_of_tree t =
    let E ((module Tree), tree) = t in
    Log.debug (fun l -> l "snapshot_of_tree");
    let root = Datakit_path.empty in
    Tree.exists_dir tree root >>*= fun is_dir ->
    if not is_dir then ok Snapshot.empty
    else
      repos t >>*= fun repos ->
      commits ~repos t >>*= fun commits ->
      prs ~repos t >>*= fun prs ->
      statuses ~commits t >>*= fun status ->
      refs ~repos t >|*= fun refs ->
      Snapshot.create ~repos ~status ~prs ~refs ~commits

  (* compute all the active hooks for a given DataKit commit *)
  let snapshot ?old tree =
    Log.debug (fun l ->
        let c = match old with None -> "*" | Some (c, _) -> DK.Commit.id c in
        l "snapshot old=%s"c
      );
    match old with
    | None        -> snapshot_of_tree tree
    | Some (c, s) ->
      diff tree c >>*= fun diff ->
      apply s (tree, diff) >>*= fun s ->
      ok s

end

module Sync (API: API) (DK: Datakit_S.CLIENT) = struct

  module Conv = Conv(DK)

  let error fmt = Fmt.kstrf (fun str -> DK.error "sync: %s" str) fmt

  (** Branches *)

  type branch = {
    snapshot: Snapshot.t;
    tr      : DK.Transaction.t;
    head    : DK.Commit.t;
    name    : string;
  }

  let pp_branch ppf t =
    Fmt.pf ppf "%a=%a" DK.Commit.pp t.head Snapshot.pp t.snapshot

  let compare_branch x y = Snapshot.compare x.snapshot y.snapshot

  (** State (t) *)

  (*               [priv]        [pub]
                      |            |
      GH --events-->  |            | <--commits-- Users
                      |            |
                      | --merge--> |
                      |            |
      GH --API GET--> |            | --API SET--> GH
                      |            |
                      | --merge--> |
                      |            |
  *)
  type state = {
    pub     : branch;       (* the public branch, where the user writes stuff *)
    priv    : branch; (* the private branch, where webhook events are written *)
  }

  let _compare x y =
    match compare_branch x.priv y.priv with
    | 0 -> compare_branch x.pub y.pub
    | i -> i

  let pp ppf t =
    Fmt.pf ppf "@[<2>pub:%a@]@;@[<2>priv:%a@]" pp_branch t.pub pp_branch t.priv

  let with_head branch fn =
    DK.Branch.head branch >>*= function
    | None   -> error "empty branch!"
    | Some c -> fn c

  let tr_head tr =
    DK.Transaction.parents tr >>*= function
    | []  -> error "no parents!"
    | [p] -> ok p
    | _   -> error "too many parents!"

  let is_open t =
    DK.Transaction.closed t.priv.tr = false
    && DK.Transaction.closed t.pub.tr = false

  let is_closed t =
    DK.Transaction.closed t.priv.tr && DK.Transaction.closed t.pub.tr

  let branch ?old b =
    DK.Branch.transaction b >>*= fun tr ->
    tr_head tr >>*= fun head ->
    Conv.snapshot ?old (Conv.tree_of_commit head) >>*= fun snapshot ->
    let name = DK.Branch.name b in
    ok { snapshot; tr; head; name }

  let state ~old ~pub ~priv =
    let () = match old with
      | None   -> Log.info (fun l -> l "Loading full state")
      | Some o -> assert (is_closed o)
    in
    let mk b = (b.head, b.snapshot) in
    let pub_o  = match old with None -> None | Some o -> Some (mk o.pub)  in
    let priv_o = match old with None -> None | Some o -> Some (mk o.priv) in
    branch ?old:pub_o pub   >>*= fun pub ->
    branch ?old:priv_o priv >|*= fun priv ->
    { pub; priv }

  (** Import from GitHub *)

  let status_of_commits token commits =
    let api_status token c =
      Log.info (fun l -> l "API.status %a" Commit.pp c);
      API.status token c >|= function
      | Error e   -> Error (c, e)
      | Ok status -> Ok (Status.Set.of_list status)
    in
    Lwt_list.map_p (api_status token) (Commit.Set.elements commits)
    >|= fun status ->
    List.fold_left (fun status -> function
        | Ok s         -> Status.Set.union status s
        | Error (c, e) ->
          Log.err (fun l -> l "API.status %a: %s" Commit.pp c e);
          status
      ) Status.Set.empty status

  let new_prs token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.prs %a" Repo.pp r);
        API.prs token r >|= function
        | Error e -> Error (r, e)
        | Ok prs  ->
          List.filter (fun pr -> pr.PR.state = `Open) prs
          |> PR.Set.of_list
          |> fun x -> Ok x
      ) repos_l
    >|= fun new_prs ->
    List.fold_left (fun new_prs -> function
        | Ok prs       -> PR.Set.union prs new_prs
        | Error (r, e) ->
          Log.err (fun l -> l "API.prs %a: %s" Repo.pp r e);
          new_prs
      ) PR.Set.empty new_prs

  let new_refs token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.refs %a" Repo.pp r);
        API.refs token r >|= function
        | Error e -> Error (r, e)
        | Ok refs -> Ok (Ref.Set.of_list refs)
      ) repos_l
    >|= fun new_refs ->
    List.fold_left (fun new_refs -> function
        | Ok refs      -> Ref.Set.union refs new_refs
        | Error (r, e) ->
          Log.err (fun l -> l "API.refs %a: %s" Repo.pp r e);
          new_refs
      ) Ref.Set.empty new_refs

  let to_clean t new_prs new_refs new_status new_commits =
    let prs =
      PR.Set.filter (fun pr ->
          not (PR.Set.exists (PR.same pr) new_prs)
        ) t.Snapshot.prs
    in
    let refs =
      Ref.Set.filter (fun r ->
          not (Ref.Set.exists (Ref.same r) new_refs)
        ) t.Snapshot.refs
    in
    let status =
      Status.Set.filter  (fun s ->
          not (Status.Set.exists (Status.same s) new_status)
        ) t.Snapshot.status
    in
    let commits =
      Commit.Set.filter (fun c ->
          not (Commit.Set.exists ((=) c) new_commits)
        ) t.Snapshot.commits
    in
    let repos = Repo.Set.empty in
    let t = { Snapshot.repos; prs; refs; commits; status } in
    if Snapshot.(compare empty t = 0) then None else Some t

  (* Import http://github.com/usr/repo state. *)
  let import_repos t ~token repos =
    Log.debug (fun l -> l "import_repo %a" Repo.Set.pp repos);
    new_prs token repos >>= fun new_prs ->
    new_refs token repos >>= fun new_refs ->
    let new_commits =
      Commit.Set.empty
      |> PR.Set.fold (fun pr acc -> Commit.Set.add (PR.commit pr) acc) new_prs
      |> Ref.Set.fold (fun r acc -> Commit.Set.add (Ref.commit r) acc) new_refs
    in
    status_of_commits token new_commits >>= fun new_status ->
    Log.debug (fun l ->
        l "[import]@;@[<2>new-prs:%a@]@;@[<2>new-refs:%a@]@;@[<2>new-status:%a@]]"
          PR.Set.pp new_prs Ref.Set.pp new_refs Status.Set.pp new_status);
    let clean = Snapshot.filter repos t in
    let prs = PR.Set.union clean.Snapshot.prs new_prs in
    let to_clean = to_clean t new_prs new_refs new_status new_commits in
    Log.debug (fun l ->
        l "[import]@;cleanup:%a" Fmt.(option Snapshot.pp) to_clean);
    let refs = Ref.Set.union clean.Snapshot.refs new_refs in
    let commits = Commit.Set.union clean.Snapshot.commits new_commits in
    let status = Status.Set.union clean.Snapshot.status new_status in
    let repos = Repo.Set.union clean.Snapshot.repos repos in
    ok ({ Snapshot.repos; prs; commits; refs; status }, to_clean)

  (* Read DataKit data and call the GitHub API to sync the world with
     what DataKit think it should be. *)
  let call_github_api ~dry_updates ~token ~old t =
    Log.debug (fun l -> l "call_github_api");
    let status = Status.Set.diff t.Snapshot.status old.Snapshot.status in
    let prs = PR.Set.diff t.Snapshot.prs old.Snapshot.prs in
    Lwt_list.iter_p (fun s ->
        let old =
          let same_context x =
            s.Status.context = x.Status.context &&
            s.Status.commit  = x.Status.commit
          in
          Status.Set.findf same_context old.Snapshot.status
        in
        Log.info
          (fun l ->
             l "API.set-status %a (was %a)"
               Status.pp s Fmt.(option ~none:(unit "<empty>") Status.pp) old);
        if dry_updates then Lwt.return_unit
        else
          API.set_status token s >|= function
          | Ok ()   -> ()
          | Error e ->
            Log.err (fun l -> l "API.set-status %a: %s" Status.pp s e)
      ) (Status.Set.elements status)
    >>= fun () ->
    Lwt_list.iter_p (fun pr ->
        Log.info (fun l -> l "API.set-pr %a" PR.pp pr);
        if not dry_updates then
          API.set_pr token pr >|= function
          | Ok ()   -> ()
          | Error e -> Log.err (fun l -> l "API.set-pr %a: %s" PR.pp pr e)
        else Lwt.return_unit
      ) (PR.Set.elements prs)
    >>= fun () ->
    ok ()

  (** Merge *)

  (* Merge the private branch back in the public branch. *)
  let merge t ~pub ~priv =
    state ~old:(Some t) ~pub ~priv >>*= fun t ->
    Log.debug (fun l -> l "[merge]@;%a" pp t);
    if compare_branch t.pub t.priv = 0 then
      DK.Transaction.abort t.pub.tr >>= ok
    else
      DK.Transaction.merge t.pub.tr t.priv.head >>*= fun (m, conflicts) ->
      (if conflicts = [] then ok ""
       else (
         (* usually that means a conflict between what the user
            request and the state of imported events from
            GitHub. *)
         let { DK.Transaction.ours; theirs; _ } = m in
         list_iter_s (fun path ->
             let dir, file =
               match List.rev @@ Datakit_path.unwrap path with
               | [] -> failwith "TODO"
               | base :: dir ->
                 Datakit_path.of_steps_exn (List.rev dir), base
             in
             DK.Tree.read_file ours path   >>= fun ours   ->
             DK.Tree.read_file theirs path >>= fun theirs ->
             match ours, theirs with
             | Error _ , Error _ -> DK.Transaction.remove t.pub.tr dir
             | Ok v    ,  _
             | Error _ , Ok v    ->
               DK.Transaction.create_or_replace_file t.pub.tr ~dir file v
           ) conflicts
         >>*= fun () ->
         ok @@ Fmt.strf "\n\nconflicts:@;@[%a@]"
           Fmt.(list ~sep:(unit "\n") Datakit_path.pp) conflicts)
      ) >>*= fun conflict_msg ->
      DK.Transaction.diff t.pub.tr t.pub.head >>*= function
      | []   -> DK.Transaction.abort t.pub.tr >>= ok
      | diff ->
        let diff = Diff.changes diff in
        let pp ppf diff =
          Fmt.(list ~sep:(unit "\n") Diff.pp) ppf (Diff.Set.elements diff)
        in
        let msg =
          Fmt.strf "Merging with %s\n\nChanges:\n%a%s"
            t.priv.name pp diff conflict_msg
        in
        Log.debug (fun l -> l "merge commit: %s" msg);
        DK.Transaction.commit t.pub.tr ~message:msg

  (* same as [merge], but check some useful invariants too. *)
  let merge t ~pub ~priv =
    assert (is_closed t);
    merge t ~pub ~priv >>*= fun () ->
    assert (is_closed t);
    state ~old:(Some t) ~priv ~pub

  (** Sync *)

  (* check that the public and private branch exist, and create them
     otherwise. As we will merge the private branch into the public
     one, we need to make sure they have a common ancestor. *)
  let init_sync ~priv ~pub =
    Log.debug (fun l -> l "init_sync");
    DK.Branch.head pub  >>*= fun pub_h ->
    DK.Branch.head priv >>*= fun priv_h ->
    match pub_h, priv_h with
    | None, None ->
      DK.Branch.with_transaction priv (fun tr ->
          let dir  = Datakit_path.empty in
          let data = Cstruct.of_string "### DataKit -- GitHub bridge\n" in
          DK.Transaction.create_or_replace_file tr ~dir "README.md" data
          >>= function
          | Ok ()   -> DK.Transaction.commit tr ~message:"Initial commit"
          | Error e ->
            DK.Transaction.abort tr >>= fun () ->
            Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
        ) >>*= fun () ->
      with_head priv (DK.Branch.fast_forward pub)
    | Some pub_c, None  -> DK.Branch.fast_forward priv pub_c
    | None, Some priv_c -> DK.Branch.fast_forward pub priv_c
    | Some _, Some _    -> ok ()

  let abort t =
    let close tr =
      if DK.Transaction.closed tr then Lwt.return_unit
      else DK.Transaction.abort tr
    in
    close t.priv.tr >>= fun () ->
    close t.pub.tr

  let remove_snapshot msg = function
    | None   -> ok None
    | Some t ->
      Log.debug (fun l -> l "[remove_snapshot]%s:@;%a" msg Snapshot.pp t);
      let root { Repo.user; repo } = Datakit_path.(empty / user / repo) in
      let { Snapshot.repos; prs; refs; commits; status } = t in
      let f tr =
        list_iter_s (fun r ->
            Conv.safe_remove tr (root r)
          ) (Repo.Set.elements repos)
        >>*= fun () ->
        list_iter_s (fun pr ->
            let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
            Conv.safe_remove tr dir
          ) (PR.Set.elements prs)
        >>*= fun () ->
        list_iter_s (fun c ->
            let dir = root (Commit.repo c) / "commit" / c.Commit.id in
            Conv.safe_remove tr dir
          ) (Commit.Set.elements commits)
        >>*= fun () ->
        list_iter_s (fun s ->
            let id = Status.commit_id s in
            let c  = Status.path s in
            let dir = root (Status.repo s) / "commit" / id / "status" /@ c in
            Conv.safe_remove tr dir
          ) (Status.Set.elements status)
        >>*= fun () ->
        list_iter_s (fun r ->
            let dir = root (Ref.repo r) / "ref" /@ Ref.path r in
            Conv.safe_remove tr dir
          ) (Ref.Set.elements refs)
      in
      ok (Some f)

  let cleanup msg c tr =
    remove_snapshot msg c >>*= function
    | None   -> ok ()
    | Some f -> f tr

  let sync_repos ~token ~pub ~priv t repos =
    import_repos ~token t.priv.snapshot repos >>*= fun (priv_s, c1) ->
    cleanup "import" c1 t.priv.tr >>*= fun () ->
    let priv_s, c2 = Snapshot.prune priv_s in
    cleanup "sync" c2 t.priv.tr >>*= fun () ->
    Conv.update_prs t.priv.tr priv_s.Snapshot.prs >>*= fun () ->
    Conv.update_statuses t.priv.tr priv_s.Snapshot.status >>*= fun () ->
    Conv.update_refs t.priv.tr priv_s.Snapshot.refs >>*= fun () ->
    DK.Transaction.diff t.priv.tr t.priv.head >>*= fun diff ->
    (if c1 = None && c2 = None && diff = [] then
       DK.Transaction.abort t.priv.tr >>= ok
     else
       let message = Fmt.strf "Sync with %a" Repo.Set.pp repos in
       DK.Transaction.commit t.priv.tr ~message)
    >>*= fun () ->
    DK.Transaction.abort t.pub.tr >>= fun () ->
    merge t ~pub ~priv

  type webhook = {
    watch: Repo.t -> unit Lwt.t;
    events: unit -> Event.t list;
  }

  let sync_webhooks t ~token ~webhook ~priv ~pub repos =
    match webhook with
    | None   -> ok t
    | Some w ->
      Log.debug (fun l -> l "[sync_webhook] repos: %a" Repo.Set.pp repos);
      (* register new webhooks *)
      Lwt_list.iter_p w.watch (Repo.Set.elements repos) >>= fun () ->
      (* apply the webhook events *)
      match w.events () with
      | []     -> ok t
      | events ->
        Log.debug (fun l ->
            l "[sync_webhook] events:@;%a" (Fmt.Dump.list Event.pp) events);
        let priv_s =
          List.fold_left (Snapshot.replace_event) t.priv.snapshot events
        in
        (* Need to resynchronsize build status for new commits *)
        let commits = List.fold_left (fun acc -> function
            | Event.PR pr ->
              if PR.state pr <> `Open then acc
              else Commit.Set.add (PR.commit pr) acc
            | Event.Ref (`Deleted, _) -> acc
            | Event.Ref (_, r) -> Commit.Set.add (Ref.commit r) acc
            | Event.Status _  | Event.Other _  -> acc
          ) Commit.Set.empty events
        in
        let new_commits = Commit.Set.diff commits priv_s.Snapshot.commits in
        status_of_commits token commits >>= fun new_status ->
        let status = Status.Set.union new_status priv_s.Snapshot.status in
        let commits = Commit.Set.union new_commits priv_s.Snapshot.commits in
        let priv_s = { priv_s with Snapshot.status; commits } in
        let events =
          events
          @ List.map (fun s -> Event.Status s) @@ Status.Set.elements new_status
        in
        list_iter_s (Conv.update_event t.priv.tr) events >>*= fun () ->
        let _, c = Snapshot.prune priv_s in
        cleanup "events" c t.priv.tr >>*= fun () ->
        let message =
          Fmt.strf "Importing webhooks:\n%a"
            Fmt.(list ~sep:(unit "\n") Event.pp) events
        in
        DK.Transaction.commit t.priv.tr ~message >>*= fun () ->
        DK.Transaction.abort t.pub.tr >>= fun () ->
        merge t ~pub ~priv

  let sync ~webhook ~token ~dry_updates ~pub ~priv ?old t repos =
    assert (is_open t);
    sync_webhooks t ~token ~webhook ~priv ~pub repos >>*= fun t ->
    assert (is_open t);
    sync_repos ~token ~pub ~priv t repos >>*= fun t ->
    assert (is_open t);
    let old = match old with
      | None   -> t.priv.snapshot
      | Some o -> assert (is_closed o); o.pub.snapshot
    in
    call_github_api ~dry_updates ~token ~old t.pub.snapshot >>*= fun () ->
    abort t >>= fun () ->
    ok t

  (* On startup, build the initial state by looking at the active
     repository in the public and private branch. Import the new
     repositories in the private branch, then merge it in the public
     branch. Finally call the GitHub API with the diff between the
     public and the private branch. *)
  let first_sync ~webhook ~token ~dry_updates ~pub ~priv =
    state ~old:None ~pub ~priv >>*= fun t ->
    Log.debug (fun l ->
        l "[first_sync] priv:%a pub=%a"
          DK.Commit.pp t.priv.head
          DK.Commit.pp t.pub.head
      );
    let repos =
      let r t = t.snapshot.Snapshot.repos in
      Repo.Set.union (r t.priv) (r t.pub)
    in
    begin
      if Repo.Set.is_empty repos then abort t >>= fun () -> ok t
      else sync ~webhook ~token ~dry_updates ~pub ~priv t repos
    end >|*= fun t ->
    assert (is_closed t);
    t

  let repos old t =
    let old = Snapshot.repos old.snapshot in
    let t   = Snapshot.repos t.snapshot in
    Repo.Set.(union (diff old t) (diff t old))

  (* The main synchonisation function: it is called on every change in
     the public or private branch. *)
  let sync_once ~webhook ~dry_updates ~token ~pub ~priv ~old t =
    Log.debug (fun l -> l "[sync_once]@;old:%a@;new:%a" pp old pp t);
    let repos = Repo.Set.union (repos old.pub t.pub) (repos old.priv t.priv) in
    sync ~webhook ~token ~dry_updates ~pub ~priv ~old t repos

  let sync_once ~webhook ~dry_updates ~token ~pub ~priv ~old =
    assert (is_closed old);
    state ~old:(Some old) ~pub ~priv >>*= fun t ->
    sync_once ~webhook ~dry_updates ~token ~pub ~priv ~old t >|*= fun t ->
    assert (is_closed t);
    t

  type t = State of state | Starting

  let empty = Starting

  let continue = function
    | Some s -> Lwt_switch.is_on s
    | None   -> true

  let process_webhook = function
    | None   -> None, fun () -> fst (Lwt.task ())
    | Some w ->
      let watch r = API.Webhook.watch w r in
      let events () =
        let e = API.Webhook.events w in
        API.Webhook.clear w;
        e
      in
      let run () = API.Webhook.run w in
      Some {watch; events}, run

  let run ~webhook ?switch ~dry_updates ~token ~priv ~pub t policy =
    let webhook, run_webhook = process_webhook webhook in
    let sync_once = function
      | Starting -> first_sync ~webhook ~dry_updates ~token ~priv ~pub
      | State t  -> sync_once ~webhook ~dry_updates ~token ~priv ~pub ~old:t
    in
    match policy with
    | `Once   -> sync_once t >>*= fun t -> ok (`Finish (State t))
    | `Repeat ->
      let t = ref t in
      let updates = ref false in
      let cond = Lwt_condition.create () in
      let pp ppf = function
        | Starting -> Fmt.string ppf "<starting>"
        | State t  ->
          let repos = Snapshot.repos t.priv.snapshot in
          Fmt.pf ppf "active repos: %a" Repo.Set.pp repos
      in
      let rec react () =
        if not (continue switch) then Lwt.return_unit
        else
          (if not !updates then Lwt_condition.wait cond else Lwt.return_unit)
          >>= fun () ->
          updates := false;
          Log.info (fun l -> l "Processing new entry -- %a" pp !t);
          Lwt.catch
            (fun () -> sync_once !t >|= function
               | Ok s    -> t := State s
               | Error e -> Log.err (fun l -> l "sync error: %a" DK.pp_error e))
            (fun e ->
               Log.err (fun l -> l "error: %s" (Printexc.to_string e));
               Lwt.return_unit)
          >>=
          react
      in
      let watch br =
        let notify _ =
          Log.info (fun l -> l "Change detected in %s" @@ DK.Branch.name br);
          updates := true;
          Lwt_condition.signal cond ();
          ok `Again
        in
        DK.Branch.wait_for_head ?switch br notify >>= function
        | Ok _    -> Lwt.return_unit
        | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
      in
      Lwt.choose [ react () ; watch priv; watch pub; run_webhook () ]
      >>= fun () ->
      ok (`Finish !t)

  let sync ?webhook ?switch ?(policy=`Repeat)
      ?(dry_updates=false) ~pub ~priv ~token t =
    Log.debug (fun l ->
        l "[sync] pub:%s priv:%s" (DK.Branch.name pub) (DK.Branch.name priv)
      );
    (init_sync ~priv ~pub >>*= fun () ->
     run ~webhook ?switch ~dry_updates ~token ~priv ~pub t policy >>*= function
     | `Finish l -> ok l
     | _ -> failwith "TODO")
    >>= function
    | Ok t    -> Lwt.return t
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e

end
