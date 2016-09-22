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

  let map f t = fold (fun x acc -> add (f x) acc) t empty

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

end

let pp_path = Fmt.(list ~sep:(unit "/") string)

module Repo = struct

  type t = { user: string; repo: string }
  let pp ppf t = Fmt.pf ppf "%s/%s" t.user t.repo
  let compare (x:t) (y:t) = Pervasives.compare x y
  type state = [`Monitored | `Ignored]

  let pp_state ppf = function
    | `Monitored -> Fmt.string ppf "+"
    | `Ignored   -> Fmt.string ppf "-"

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

let compare_fold fs x y =
  List.fold_left (fun acc f ->
      match acc with
      | 0 -> f x y
      | i -> i
    ) 0 (List.rev fs)

module Commit = struct

  type t = { repo: Repo.t; id : string }

  let pp ppf t = Fmt.pf ppf "{%a %s}" Repo.pp t.repo t.id
  let id t = t.id
  let repo t = t.repo
  let compare_repo x y = Repo.compare x.repo y.repo
  let compare_id x y = String.compare x.id y.id
  let same (x:t) (y:t) = x = y

  let compare = compare_fold [
      compare_repo;
      compare_id;
    ]

  module Set = struct
    include Set(struct
        type nonrec t = t
        let pp = pp
        let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
  end

end

module PR = struct

  type t = {
    head: Commit.t;
    number: int;
    state: [`Open | `Closed];
    title: string;
    base: string;
  }

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

  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_num x y = Pervasives.compare x.number y.number

  let compare = compare_fold [
      compare_repo;
      compare_num;
      Pervasives.compare;
    ]

  let pp ppf t =
    Fmt.pf ppf "{%a %d[%s] %s %a %S}"
      Repo.pp (repo t) t.number (commit_id t) t.base pp_state t.state t.title

  let number t = t.number
  let title t = t.title
  let state t = t.state
  let same x y = repo x = repo y && number x = number y

  module Set = struct
    include Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
    let commits t =
      fold (fun c acc -> Commit.Set.add (commit c) acc) t Commit.Set.empty
  end

end

module Status = struct

  type t = {
    commit: Commit.t;
    context: string list;
    url: string option;
    description: string option;
    state: Status_state.t;
  }

  let context t = match t.context with
    | [] -> ["default"]
    | l  -> l

  let path s = Datakit_path.of_steps_exn (context s)
  let repo t = t.commit.Commit.repo
  let commit t = t.commit
  let commit_id t = t.commit.Commit.id
  let same x y = commit x = commit y && context x = context y
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_commit_id x y = Pervasives.compare (commit_id x) (commit_id y)
  let compare_context x y = Pervasives.compare x.context y.context

  let compare = compare_fold [
      compare_repo;
      compare_commit_id;
      compare_context;
      Pervasives.compare
    ]

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

  module Set = struct
    include Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
    let commits t =
      fold (fun c acc -> Commit.Set.add (commit c) acc) t Commit.Set.empty
  end

end

module Ref = struct

  type t = {
    head: Commit.t;
    name: string list;
  }

  let repo t = t.head.Commit.repo
  let commit t = t.head
  let commit_id t = t.head.Commit.id
  let name t = t.name
  let same x y = repo x = repo y && name x = name y
  let path s = Datakit_path.of_steps_exn s.name
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_name x y = Pervasives.compare x.name y.name

  let compare = compare_fold [
      compare_repo;
      compare_name;
      Pervasives.compare;
    ]

  let pp ppf t =
    Fmt.pf ppf "{%a %a[%s]}" Repo.pp (repo t) pp_path t.name (commit_id t)

  module Set = struct
    include Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
      end)
    let repos t =
      fold (fun c acc -> Repo.Set.add (repo c) acc) t Repo.Set.empty
    let commits t =
      fold (fun c acc -> Commit.Set.add (commit c) acc) t Commit.Set.empty
  end

  type state = [`Created | `Updated | `Removed]

  let pp_state ppf = function
    | `Created -> Fmt.string ppf "+"
    | `Updated -> Fmt.string ppf "*"
    | `Removed -> Fmt.string ppf "-"

end

module Event = struct

  type t =
    | Repo of (Repo.state * Repo.t)
    | PR of PR.t
    | Status of Status.t
    | Ref of (Ref.state * Ref.t)
    | Other of (Repo.t * string)

  let repo' s r = Repo (s, r)
  let pr x = PR x
  let status x = Status x
  let ref x y = Ref (x, y)
  let other x y = Other (x, y)

  let pp ppf = function
    | Repo(s,r)-> Fmt.pf ppf "Repo: %a%a" Repo.pp_state s Repo.pp r
    | PR pr    -> Fmt.pf ppf "PR: %a" PR.pp pr
    | Status s -> Fmt.pf ppf "Status: %a" Status.pp s
    | Ref(s,r) -> Fmt.pf ppf "Ref: %a%a" Ref.pp_state s Ref.pp r
    | Other o  -> Fmt.pf ppf "Other: %s" @@ snd o

  let repo = function
    | Repo r   -> snd r
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
  val set_ref: token -> Ref.t -> unit result
  val remove_ref: token -> Repo.t -> string list -> unit result
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
    val wait: t -> unit Lwt.t
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

  let create ~repos ~commits ~status ~prs ~refs =
    { repos; commits; status; prs; refs }

  let compare_repos x y = Repo.Set.compare x.repos y.repos
  let compare_commits x y = Commit.Set.compare x.commits y.commits
  let compare_status x y = Status.Set.compare x.status y.status
  let compare_prs x y = PR.Set.compare x.prs y.prs
  let compare_refs x y = Ref.Set.compare x.refs y.refs

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
      Fmt.pf ppf "{@[<2>repos:%a@]@;@[<2>prs:%a@]@;@[<2>refs:%a@]@;\
                  @[<2>commits:%a@]@;@[<2>status:%a@]}"
        Repo.Set.pp t.repos PR.Set.pp t.prs Ref.Set.pp t.refs
        Commit.Set.pp t.commits Status.Set.pp t.status

  let remove_repo t repo =
    let keep f r = Repo.compare (f r) repo <> 0 in
    let repos = Repo.Set.remove repo t.repos in
    let prs = PR.Set.filter (keep PR.repo) t.prs in
    let refs = Ref.Set.filter (keep Ref.repo) t.refs in
    let commits = Commit.Set.filter (keep Commit.repo) t.commits in
    let status = Status.Set.filter (keep Status.repo) t.status in
    { repos; prs; refs; commits; status }

  let remove_repos t repos =
    Repo.Set.fold (fun r acc -> remove_repo acc r) repos t

  let keep_repos t repos =
    let keep f r = Repo.Set.mem (f r) repos in
    let repos = Repo.Set.diff t.repos repos in
    let prs = PR.Set.filter (keep PR.repo) t.prs in
    let refs = Ref.Set.filter (keep Ref.repo) t.refs in
    let commits = Commit.Set.filter (keep Commit.repo) t.commits in
    let status = Status.Set.filter (keep Status.repo) t.status in
    { repos; prs; refs; commits; status }

  let replace_repo t r = { t with repos = Repo.Set.add r t.repos }

  let remove_commit t (r, id) =
    let keep x = r <> Commit.repo x || id <> Commit.id x in
    { t with commits = Commit.Set.filter keep t.commits }

  let add_commit t c =
    let commits = Commit.Set.add c t.commits in
    { t with commits }

  let remove_pr t (r, id) =
    let keep pr = r  <> PR.repo pr || id <>  pr.PR.number in
    { t with prs = PR.Set.filter keep t.prs }

  let add_pr t pr =
    let prs     = PR.Set.add pr t.prs in
    let commits = Commit.Set.add (PR.commit pr) t.commits in
    { t with prs; commits }

  let replace_pr t pr =
    if not (Repo.Set.mem (PR.repo pr) t.repos) then t
    else
      let id = PR.repo pr, pr.PR.number in
      add_pr (remove_pr t id) pr

  let remove_status t (s, l) =
    let keep x = s <> Status.commit x || l <> x.Status.context in
    { t with status = Status.Set.filter keep t.status }

  let add_status t s =
    let status  = Status.Set.add s t.status in
    let commits = Commit.Set.add (Status.commit s) t.commits in
    { t with status; commits }

  let replace_status t s =
    if not (Repo.Set.mem (Status.repo s) t.repos) then t
    else
      let cc = s.Status.commit, s.Status.context in
      add_status (remove_status t cc) s

  let remove_ref t (r, l) =
    let keep x = r <> Ref.repo x || l <> x.Ref.name in
    { t with refs = Ref.Set.filter keep t.refs }

  let add_ref t r =
    let refs = Ref.Set.add r t.refs in
    { t with refs }

  let replace_ref t r =
    if not (Repo.Set.mem (Ref.repo r) t.repos) then t
    else
      let name = Ref.repo r, r.Ref.name in
      add_ref (remove_ref t name) r

  let replace_event t = function
    | Event.Repo (`Ignored,r) -> remove_repo t r
    | Event.Repo (_, r)       -> replace_repo t r
    | Event.PR pr             -> replace_pr t pr
    | Event.Ref (`Removed, r) -> remove_ref t (Ref.repo r, Ref.name r)
    | Event.Ref (_, r)        -> replace_ref t r
    | Event.Status s          -> replace_status t s
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
      assert (compare (keep_repos t t.repos) (keep_repos result t.repos)  = 0);
      result, None
    ) else
      result, Some cleanup

end

module Diff = struct

  type id = [
    | `Repo
    | `PR of int
    | `Commit of string
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
    | `Repo          -> Fmt.pf ppf "{%a}" Repo.pp t.repo
    | `Unknown       -> Fmt.pf ppf "{%a ?}" Repo.pp t.repo
    | `PR n          -> Fmt.pf ppf "{%a %d}" Repo.pp t.repo n
    | `Ref l         -> Fmt.pf ppf "{%a %a}" Repo.pp t.repo pp_path l
    | `Commit c      -> Fmt.pf ppf "{%a %s}" Repo.pp t.repo c
    | `Status (c, l) -> Fmt.pf ppf "{%a %s[%a]}" Repo.pp t.repo c pp_path l

  let compare: t -> t -> int = Pervasives.compare

  module Set = Set(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)

  let path_of_diff = function
    | `Added f | `Removed f | `Updated f -> Datakit_path.unwrap f

  let changes diff =
    let without_last l = List.rev (List.tl (List.rev l)) in
    List.fold_left (fun acc d ->
        let path = path_of_diff d in
        let t = match path with
          | [] | [_]             -> None
          | user :: repo :: path ->
            let repo = { Repo.user; repo } in
            match path with
            | [] | [".monitor"] -> Some { repo; id = `Repo }
            | "pr" :: id :: _   -> Some { repo; id = `PR (int_of_string id) }
            | "commit" :: [id]  -> Some { repo; id = `Commit id }
            | "commit" :: id :: "status" :: (_ :: _ :: _ as tl) ->
              Some { repo; id = `Status (id, without_last tl) }
            | "ref" :: ( _ :: _ :: _ as tl)  ->
              Some { repo; id = `Ref (without_last tl) }
            |  _ -> Some { repo; id = `Unknown }
        in
        match t with
        | None   -> acc
        | Some t -> Set.add t acc
      ) Set.empty diff

end

module Conv (DK: Datakit_S.CLIENT) = struct

  type nonrec 'a result = ('a, DK.error) result Lwt.t

  (* conversion between GitHub and DataKit states. *)

  module type TREE = sig
    include Datakit_S.READABLE_TREE with type 'a or_error := 'a DK.or_error
    val diff: DK.Commit.t -> Diff.Set.t result
  end

  type tree = E: (module TREE with type t = 'a) * 'a -> tree

  let safe_remove t path =
    DK.Transaction.remove t path >>= function
    | Error _ | Ok () -> ok ()

  let safe_read_dir (E ((module Tree), tree)) dir =
    Tree.read_dir tree dir >|= function
    | Error _ -> []
    | Ok dirs -> dirs

  let safe_exists_dir (E ((module Tree), tree)) dir =
    Tree.exists_dir tree dir >|= function
    | Error _ -> false
    | Ok b    -> b

  let safe_read_file (E ((module Tree), tree)) file =
    Tree.read_file tree file >|= function
    | Error _ -> None
    | Ok b    -> Some (String.trim (Cstruct.to_string b))


  let walk
      (type elt) (type t) (module Set: SET with type elt = elt and type t = t)
      tree root (file, fn) =
    let rec aux context =
      match Datakit_path.of_steps context with
      | Error e ->
        Log.err (fun l -> l "%s" e);
        Lwt.return Set.empty
      | Ok ctx  ->
        let dir = root /@ ctx in
        safe_read_dir tree dir >>= fun child ->
        Lwt_list.fold_left_s (fun acc c ->
            (* FIXME: not tail recurcsive *)
            aux (context @ [c]) >|= fun child ->
            Set.union child acc
          ) Set.empty child >>= fun child ->
        safe_read_file tree (dir / file) >>= fun file ->
        if file <> None then
          fn context >|= function
          | None   -> child
          | Some s -> Set.add s child
        else
          Lwt.return child
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


  let root r = empty / r.Repo.user / r.Repo.repo

  (* Repos *)

  let repo tree repo =
    safe_read_file tree (root repo / ".monitor") >|= function
    | None   ->
      Log.debug (fun l -> l "repo %a -> false" Repo.pp repo);
      None
    | Some _ ->
      Log.debug (fun l -> l "repo %a -> true" Repo.pp repo);
      Some repo

  let repos tree =
    let root = Datakit_path.empty in
    safe_read_dir tree root >>= fun users ->
    Lwt_list.fold_left_s (fun acc user ->
        safe_read_dir tree (root / user) >>= fun repos ->
        Lwt_list.fold_left_s (fun acc repo ->
            safe_read_file tree (root / user /repo / ".monitor") >|= function
            | None   -> acc
            | Some _ -> Repo.Set.add { Repo.user; repo } acc
          ) acc repos
      ) Repo.Set.empty users >|= fun repos ->
    Log.debug (fun l -> l "repos -> @;@[<2>%a@]" Repo.Set.pp repos);
    repos

  let update_repo tr s r =
    let dir = root r in
    match s with
    | `Ignored   -> ok ()
    | `Monitored ->
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let empty = Cstruct.of_string "" in
      DK.Transaction.create_or_replace_file tr ~dir ".monitor" empty

  (* PRs *)

  let update_pr t pr =
    let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
    Log.debug (fun l -> l "update_pr %s" @@ Datakit_path.to_hum dir);
    match pr.PR.state with
    | `Closed -> safe_remove t dir
    | `Open   ->
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let write k v =
        let v = Cstruct.of_string (v ^ "\n") in
        DK.Transaction.create_or_replace_file t ~dir k v
      in
      write "head"  (PR.commit_id pr)                >>*= fun () ->
      write "state" (PR.string_of_state pr.PR.state) >>*= fun () ->
      write "title" pr.PR.title                      >>*= fun () ->
      write "base"  pr.PR.base

  let pr tree repo number =
    let dir = root repo / "pr" / string_of_int number in
    Log.debug (fun l -> l "pr %a" Datakit_path.pp dir);
    safe_read_file tree (dir / "head")  >>= fun head ->
    safe_read_file tree (dir / "state") >>= fun state ->
    safe_read_file tree (dir / "title") >>= fun title ->
    safe_read_file tree (dir / "base")  >|= fun base ->
    match head, state, base with
    | None, _, _  ->
      Log.debug (fun l ->
          l "error: %a/pr/%d/head does not exist" Repo.pp repo number);
      None
    | _, None, _ ->
      Log.debug (fun l ->
          l "error: %a/pr/%d/state does not exist" Repo.pp repo number);
      None
    | _, _, None ->
      Log.debug (fun l ->
          l "error: %a/pr/%d/base does not exist" Repo.pp repo number);
      None
    | Some id, Some state, Some base ->
      let head = { Commit.repo; id } in
      let title = match title with None -> "" | Some t -> t in
      let state = match PR.state_of_string state with
        | Some s -> s
        | None    ->
          Log.err (fun l ->
              l "%s is not a valid PR state, picking `Closed instead"
                state);
          `Closed
      in
      Some { PR.head; number; state; title; base }

  let prs_of_repo tree repo =
    let dir = root repo / "pr"  in
    safe_read_dir tree dir >>= fun nums ->
    Lwt_list.fold_left_s (fun acc n ->
        pr tree repo (int_of_string n) >|= function
        | None   -> acc
        | Some p -> PR.Set.add p acc
      ) PR.Set.empty nums >|= fun prs ->
    Log.debug (fun l ->
        l "prs_of_repo %a -> @;@[<2>%a@]" Repo.pp repo PR.Set.pp prs);
    prs

  let maybe_repos tree = function
    | None -> repos tree
    | Some rs -> Lwt.return rs

  let prs ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.fold_left_s (fun acc r ->
        prs_of_repo tree r >|= fun prs ->
        PR.Set.union prs acc
      ) PR.Set.empty (Repo.Set.elements repos)
    >|= fun prs ->
    Log.debug (fun l -> l "prs -> @;@[<2>%a@]" PR.Set.pp prs);
    prs

  (* Commits *)

  let commit tree repo id =
    let dir = root repo / "commit" / id in
    safe_exists_dir tree dir >|= function
    | false ->
      Log.debug (fun l -> l "commit {%a %s} -> false" Repo.pp repo id);
      None
    | true  ->
      Log.debug (fun l -> l "commit {%a %s} -> true" Repo.pp repo id);
      Some { Commit.repo; id }

  let commits_of_repo tree repo =
    let dir = root repo / "commit" in
    safe_read_dir tree dir >|= fun commits ->
    List.fold_left (fun s id ->
        Commit.Set.add { Commit.repo; id } s
      ) Commit.Set.empty commits
    |> fun cs ->
    Log.debug
      (fun l -> l "commits_of_repo %a -> @;@[<2>%a@]" Repo.pp repo Commit.Set.pp cs);
    cs


  let commits ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.fold_left_s (fun acc r ->
        commits_of_repo tree r >|= fun commits ->
        Commit.Set.union commits acc
      ) Commit.Set.empty (Repo.Set.elements repos)
    >|= fun cs ->
    Log.debug (fun l -> l "commits -> @;@[<2>%a@]" Commit.Set.pp cs);
    cs

  (* Status *)

  let update_status t s =
    let dir = root (Status.repo s) / "commit" / (Status.commit_id s)
              / "status" /@ Status.path s
    in
    Log.debug (fun l -> l "update_status %a" Datakit_path.pp dir);
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

  let status tree commit context =
    let context = Datakit_path.of_steps_exn context in
    let dir =
      root (Commit.repo commit) / "commit" / Commit.id commit / "status"
      /@ context
    in
    safe_read_file tree (dir / "state") >>= fun state ->
    match state with
    | None     ->
      Log.debug (fun l -> l "status %a -> None" Datakit_path.pp dir);
      Lwt.return_none
    | Some str ->
      let state = match Status_state.of_string str with
        | Some s -> s
        | None   ->
          Log.err (fun l -> l "%s: invalid state, using `Failure instead" str);
          `Failure
      in
      Log.debug (fun l -> l "status %a -> %a"
                    Datakit_path.pp context Status_state.pp state);
      safe_read_file tree (dir / "description") >>= fun description ->
      safe_read_file tree (dir / "target_url")  >|= fun url ->
      let context = Datakit_path.unwrap context in
      Some { Status.state; commit; context; description; url }

  let statuses_of_commits tree commits =
    Lwt_list.fold_left_s (fun acc commit ->
        let dir = root (Commit.repo commit) / "commit" in
        let dir = dir / Commit.id commit / "status" in
        walk (module Status.Set) tree dir ("state", status tree commit)
        >|= fun status ->
        Status.Set.union status acc
      ) Status.Set.empty (Commit.Set.elements commits)
    >|= fun status ->
    Log.debug (fun l -> l "statuses_of_commits %a -> @;@[<2>%a@]"
                  Commit.Set.pp commits Status.Set.pp status);
    status

  let maybe_commits tree = function
    | None   -> commits tree
    | Some c -> Lwt.return c

  let statuses ?commits:cs tree =
    maybe_commits tree cs >>= fun commits ->
    statuses_of_commits tree commits >|= fun status ->
    Log.debug (fun l -> l "statuses -> @;@[<2>%a@]" Status.Set.pp status);
    status

  (* Refs *)

  let ref_ tree repo name =
    let path = Datakit_path.of_steps_exn name in
    let head = root repo / "ref" /@ path / "head" in
    safe_read_file tree head >|= function
    | None    ->
      Log.debug (fun l -> l "ref_ %a:%a -> None" Repo.pp repo pp_path name);
      None
    | Some id ->
      Log.debug (fun l -> l "ref_ %a:%a -> %s" Repo.pp repo pp_path name id);
      let head = { Commit.repo; id } in
      Some { Ref.head; name }

  let refs_of_repo tree repo =
    let dir = root repo / "ref" in
    walk (module Ref.Set) tree dir ("head", ref_ tree repo) >|= fun refs ->
    Log.debug (fun l ->
        l "refs_of_repo %a -> @;@[<2>%a@]" Repo.pp repo Ref.Set.pp refs);
    refs

  let refs ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.fold_left_s (fun acc r ->
        refs_of_repo tree r >|= fun refs ->
        Ref.Set.union acc refs
      ) Ref.Set.empty (Repo.Set.elements repos)
    >|= fun refs ->
    Log.debug (fun l -> l "refs -> @;@[<2>%a@]" Ref.Set.pp refs);
    refs

  let update_ref tr s r =
    let path = Datakit_path.of_steps_exn (Ref.name r) in
    Log.debug (fun l -> l "update_ref %a" Datakit_path.pp path);
    let dir = root (Ref.repo r) / "ref" /@ path in
    match s with
    | `Removed -> safe_remove tr dir
    | `Created | `Updated ->
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let head = Cstruct.of_string (Ref.commit_id r ^ "\n") in
      DK.Transaction.create_or_replace_file tr ~dir "head" head

  let update_event t = function
    | Event.Repo (s, r) -> update_repo t s r
    | Event.PR pr       -> update_pr t pr
    | Event.Status s    -> update_status t s
    | Event.Ref (s, r)  -> update_ref t s r
    | Event.Other o     ->
      Log.debug (fun l  -> l "ignoring event: %s" @@ snd o);
      ok ()

  (* Snapshot *)

  let snapshot_of_repos tree repos =
    commits ~repos tree >>= fun commits ->
    prs ~repos tree >>= fun prs ->
    statuses ~commits tree >>= fun status ->
    refs ~repos tree >|= fun refs ->
    Snapshot.create ~repos ~status ~prs ~refs ~commits

  let snapshot_of_tree tree =
    repos tree >>= fun repos ->
    snapshot_of_repos tree repos

  (* Diffs *)

  let safe_diff (E ((module Tree), _)) c =
    Tree.diff c >|= function
    | Error _ -> Diff.Set.empty
    | Ok d    -> d

  let apply_repo_diff t tree r =
    repo tree r >>= function
    | None   -> Snapshot.remove_repo t r |> Lwt.return
    | Some r ->
      snapshot_of_repos tree (Repo.Set.singleton r) >|= fun s ->
      Snapshot.union s t

  let apply_commit_diff t tree (r, id as x) =
    commit tree r id >|= function
    | None   -> Snapshot.remove_commit t x
    | Some c -> Snapshot.add_commit t c

  let apply_pr_diff t tree (r, id as x)  =
    pr tree r id >|= function
    | None    -> Snapshot.remove_pr t x
    | Some pr -> Snapshot.replace_pr t pr

  let apply_status_diff t tree (c, context as x) =
    status tree c context >|= function
    | None   -> Snapshot.remove_status t x
    | Some s -> Snapshot.replace_status t s

  let apply_ref_diff t tree (r, name as x) =
    ref_ tree r name >|= function
    | None   -> Snapshot.remove_ref t x
    | Some r -> Snapshot.replace_ref t r

  let apply init (tree, diff) =
    Log.debug (fun l -> l "apply");
    if Diff.Set.is_empty diff then Lwt.return init
    else Lwt_list.fold_left_s (fun acc { Diff.repo; id } ->
        match id with
        | `Repo      -> apply_repo_diff acc tree repo
        | `PR pr     -> apply_pr_diff acc tree (repo, pr)
        | `Ref name  -> apply_ref_diff acc tree (repo, name)
        | `Commit id -> apply_commit_diff acc tree (repo, id)
        | `Status (id, context) ->
          let commit = { Commit.repo; id } in
          apply_status_diff acc tree (commit, context) >>= fun acc ->
          apply_commit_diff acc tree (repo, id)
        | `Unknown ->
          let repos = Repo.Set.add repo acc.Snapshot.repos in
          Lwt.return { acc with Snapshot.repos };
      ) init (Diff.Set.elements diff)
      >|= fun t ->
      Log.debug (fun l -> l "apply @[<2>(%a)@]@;@[<2>(%a)@]@;@[<2>->(%a)@]"
                    Diff.Set.pp diff Snapshot.pp init Snapshot.pp t);
      t
  (* compute all the active hooks for a given DataKit commit *)
  let snapshot msg ?old tree =
    Log.debug (fun l ->
        let c = match old with None -> "*" | Some (c, _) -> DK.Commit.id c in
        l "snapshot %s old=%s" msg c
      );
    match old with
    | None        -> snapshot_of_tree tree
    | Some (c, s) ->
      safe_diff tree c >>= fun diff ->
      apply s (tree, diff) >|= fun s ->
      s

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
    pub    : branch;        (* the public branch, where the user writes stuff *)
    priv   : branch;  (* the private branch, where webhook events are written *)
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

  let branch msg ?old b =
    DK.Branch.transaction b >>*= fun tr ->
    tr_head tr >>*= fun head ->
    Conv.snapshot msg ?old (Conv.tree_of_commit head) >>= fun snapshot ->
    let name = DK.Branch.name b in
    ok { snapshot; tr; head; name }

  let state msg ~old ~pub ~priv =
    let () = match old with
      | None   -> Log.info (fun l -> l "Loading full state")
      | Some o ->
        if not (is_closed o) then (
          Log.err (fun l -> l "%s should be closed!" msg);
          assert false)
    in
    let mk b = (b.head, b.snapshot) in
    let pub_o  = match old with None -> None | Some o -> Some (mk o.pub)  in
    let priv_o = match old with None -> None | Some o -> Some (mk o.priv) in
    branch (msg ^ "-pub")  ?old:pub_o pub   >>*= fun pub ->
    branch (msg ^ "-priv") ?old:priv_o priv >|*= fun priv ->
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

  type cleanup = {
    remove: Snapshot.t option;
    update: Snapshot.t option;
  }

  let cleanup t repos new_prs new_refs new_status new_commits =
    let mk repos keep_pr keep_ref keep_status keep_commit =
      let prs = PR.Set.filter keep_pr t.Snapshot.prs in
      let refs = Ref.Set.filter keep_ref t.Snapshot.refs in
      let status = Status.Set.filter keep_status t.Snapshot.status in
      let commits = Commit.Set.filter keep_commit t.Snapshot.commits in
      let t = { Snapshot.repos; prs; refs; commits; status } in
      if Snapshot.(compare empty t = 0) then None else Some t
    in
    let remove =
      let keep_pr x =
        Repo.Set.mem (PR.repo x) repos
        && not (PR.Set.exists (PR.same x) new_prs)
      in
      let keep_ref x =
        Repo.Set.mem (Ref.repo x) repos
        && not (Ref.Set.exists (Ref.same x) new_refs)
      in
      let keep_status x =
        Repo.Set.mem (Status.repo x) repos
        && not (Status.Set.exists (Status.same x) new_status)
      in
      let keep_commit x =
        Repo.Set.mem (Commit.repo x) repos
        && not (Commit.Set.exists (Commit.same x) new_commits)
      in
      mk Repo.Set.empty keep_pr keep_ref keep_status keep_commit
    in
    let update =
      let keep_pr x =
        Repo.Set.mem (PR.repo x) repos
        && PR.Set.exists
          (fun y -> PR.same x y && PR.compare x y <> 0) new_prs
      in
      let keep_ref x =
        Repo.Set.mem (Ref.repo x) repos
        && Ref.Set.exists
          (fun y -> Ref.same x y && Ref.compare x y <> 0) new_refs
      in
      let keep_status x =
        Repo.Set.mem (Status.repo x) repos
        && Status.Set.exists
          (fun y -> Status.same x y && Status.compare x y <> 0) new_status
      in
      let keep_commit _ = false in
      mk repos keep_pr keep_ref keep_status keep_commit
    in
    { remove; update }

  (* Import http://github.com/usr/repo state. *)
  let import_repos t ~token repos =
    Log.debug (fun l -> l "import_repo %a" Repo.Set.pp repos);
    new_prs token repos >>= fun new_prs ->
    new_refs token repos >>= fun new_refs ->
    let new_commits =
      Commit.Set.union (PR.Set.commits new_prs) (Ref.Set.commits new_refs)
    in
    status_of_commits token new_commits >>= fun new_status ->
    Log.debug (fun l ->
        l "[import] %a@;@[<2>new-prs:%a@]@;@[<2>new-refs:%a@]@;\
           @[<2>new-status:%a@]]"
          Repo.Set.pp repos
          PR.Set.pp new_prs Ref.Set.pp new_refs Status.Set.pp new_status);
    let cleanup = cleanup t repos new_prs new_refs new_status new_commits in
    let base = Snapshot.remove_repos t repos in
    let repos = Repo.Set.union t.Snapshot.repos repos in
    let prs = PR.Set.union base.Snapshot.prs new_prs in
    let refs = Ref.Set.union base.Snapshot.refs new_refs in
    let commits = Commit.Set.union base.Snapshot.commits new_commits in
    let status = Status.Set.union base.Snapshot.status new_status in
    ok ({ Snapshot.repos; prs; commits; refs; status }, cleanup)

  let api_set_pr ~dry_updates ~token pr =
    Log.info (fun l -> l "API.set-pr %a" PR.pp pr);
    if dry_updates then Lwt.return_unit
    else
      API.set_pr token pr >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-pr %a: %s" PR.pp pr e)

  let api_remove_ref ~dry_updates ~token r =
    let repo = Ref.repo r in
    let name = Ref.name r in
    let pp ppf _ = Fmt.pf ppf "{%a %a}" Repo.pp repo pp_path name in
    Log.info (fun l -> l "API.remove-ref %a" pp r);
    if dry_updates then Lwt.return_unit
    else
      API.remove_ref token repo name >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.remove-ref %a: %s" pp r e)

  let api_set_ref ~dry_updates ~token r =
    Log.info (fun l -> l "API.set-ref %a" Ref.pp r);
    if dry_updates then Lwt.return_unit
    else
      API.set_ref token r >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-ref %a: %s" Ref.pp r e)

  let api_set_status ~dry_updates ~token s =
    Log.info (fun l -> l "API.set-status %a" Status.pp s);
    if dry_updates then Lwt.return_unit
    else
      API.set_status token s >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-status %a: %s" Status.pp s e)

  (* Read DataKit data and call the GitHub API to sync the world with
     what DataKit think it should be. *)
  let call_github_api ~dry_updates ~token ~old t =
    let pub = t.pub.snapshot in
    Log.debug (fun l ->
        l "call_github_api@;@[<2>old=%a@]@;@[<2>pub=%a@]"
          Snapshot.pp old Snapshot.pp pub);
    let closed_prs =
      PR.Set.diff old.Snapshot.prs pub.Snapshot.prs
      |> PR.Set.filter
        (fun pr -> not (PR.Set.exists (PR.same pr) pub.Snapshot.prs))
      |> PR.Set.map (fun pr -> { pr with PR.state = `Closed })
    in

    let prs =
      PR.Set.diff pub.Snapshot.prs old.Snapshot.prs
      |> PR.Set.union closed_prs
    in
    Lwt_list.iter_p (api_set_pr ~dry_updates ~token) (PR.Set.elements prs)
    >>= fun () ->
    let closed_refs =
      Ref.Set.diff old.Snapshot.refs pub.Snapshot.refs
      |> Ref.Set.filter
        (fun r -> not (Ref.Set.exists (Ref.same r) pub.Snapshot.refs))
    in
    Lwt_list.iter_p (api_remove_ref ~dry_updates ~token)
      (Ref.Set.elements closed_refs)
    >>= fun () ->
    let refs = Ref.Set.diff pub.Snapshot.refs old.Snapshot.refs in
    Lwt_list.iter_p (api_set_ref ~dry_updates ~token) (Ref.Set.elements refs)
    >>= fun () ->
    (* NOTE: ideally we would also remove status, but the GitHub API doesn't
       support removing status so we just ignore *)
    let status = Status.Set.diff pub.Snapshot.status old.Snapshot.status in
    Lwt_list.iter_p (api_set_status ~dry_updates ~token)
      (Status.Set.elements status)
    >>= fun () ->
    ok t

  (** Merge *)

  let abort t =
    let close tr =
      if DK.Transaction.closed tr then Lwt.return_unit
      else DK.Transaction.abort tr
    in
    close t.priv.tr >>= fun () ->
    close t.pub.tr

  (* Merge the private branch back in the public branch. *)
  let merge t ~pub ~priv =
    assert (is_closed t);
    state "start-merge" ~old:(Some t) ~pub ~priv >>*= fun t ->
    Log.debug (fun l -> l "[merge]@;%a" pp t);
    if compare_branch t.pub t.priv = 0 then
      ok t
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
      | []   -> ok t
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
        DK.Transaction.commit t.pub.tr ~message:msg >>*= fun () ->
        abort t >>= fun () ->
        state "end-merge" ~old:(Some t) ~priv ~pub

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

  let remove_snapshot msg = function
    | None   -> ok None
    | Some t ->
      let f tr =
        Log.debug
          (fun l -> l "[remove_snapshot] (from %s):@;%a" msg Snapshot.pp t);
        let root { Repo.user; repo } = Datakit_path.(empty / user / repo) in
        let { Snapshot.prs; refs; commits; status; _ } = t in
        list_iter_s (fun pr ->
            let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
            Conv.safe_remove tr dir
          ) (PR.Set.elements prs)
        >>*= fun () ->
        list_iter_s (fun r ->
            let dir = root (Ref.repo r) / "ref" /@ Ref.path r in
            Conv.safe_remove tr dir
          ) (Ref.Set.elements refs)
        >>*= fun () ->
        list_iter_s (fun s ->
            let id = Status.commit_id s in
            let c  = Status.path s in
            let dir = root (Status.repo s) / "commit" / id / "status" /@ c in
            Conv.safe_remove tr dir
          ) (Status.Set.elements status)
        >>*= fun () ->
        list_iter_s (fun c ->
            let dir = root (Commit.repo c) / "commit" / c.Commit.id in
            Conv.safe_remove tr dir
          ) (Commit.Set.elements commits)
      in
      ok (Some f)

  let update_snapshot msg = function
    | None   -> ok None
    | Some t ->
      let f tr =
        Log.debug
          (fun l -> l "[update_snapshot] (from %s):@;%a" msg Snapshot.pp t);
        let { Snapshot.repos; prs; refs; status; _ } = t in
        list_iter_s (Conv.update_repo tr `Monitored) (Repo.Set.elements repos)
        >>*= fun () ->
        list_iter_s (Conv.update_pr tr) (PR.Set.elements prs)
        >>*= fun () ->
        list_iter_s (Conv.update_ref tr `Updated) (Ref.Set.elements refs)
        >>*= fun () ->
        list_iter_s (Conv.update_status tr) (Status.Set.elements status)
      in
      ok (Some f)

  let cleanup msg { remove; update } tr =
    let clean () =
      remove_snapshot msg remove >>*= function
      | None   -> ok ()
      | Some f -> f tr
    in
    let update () =
      update_snapshot msg update >>*= function
      | None   -> ok ()
      | Some f -> f tr
    in
    clean () >>*= fun () ->
    update ()

  let sync_repos ~token ~pub ~priv t repos =
    import_repos ~token t.priv.snapshot repos >>*= fun (priv_s, c) ->
    cleanup "import" c t.priv.tr >>*= fun () ->
    let priv_s, remove = Snapshot.prune priv_s in
    cleanup "sync" { remove; update = Some priv_s } t.priv.tr >>*= fun () ->
    DK.Transaction.diff t.priv.tr t.priv.head >>*= fun diff ->
    (if c.remove = None && c.update = None && remove = None && diff = [] then
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
      Lwt_list.iter_p (fun r ->
          Log.info (fun l -> l "API.add-webhook %a" Repo.pp r);
          w.watch r
        ) (Repo.Set.elements repos) >>= fun () ->
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
            | Event.Ref (`Removed, _) -> acc
            | Event.Ref (_, r) -> Commit.Set.add (Ref.commit r) acc
            | Event.Repo _ | Event.Status _  | Event.Other _  -> acc
          ) Commit.Set.empty events
        in
        let new_commits = Commit.Set.diff commits priv_s.Snapshot.commits in
        status_of_commits token commits >>= fun new_status ->
        let status = Status.Set.union new_status priv_s.Snapshot.status in
        let commits = Commit.Set.union new_commits priv_s.Snapshot.commits in
        let priv_s = { priv_s with Snapshot.status; commits } in
        let events =
          events @ List.map Event.status @@ Status.Set.elements new_status
        in
        list_iter_s (Conv.update_event t.priv.tr) events >>*= fun () ->
        let _, remove = Snapshot.prune priv_s in
        cleanup "events" { remove; update = None } t.priv.tr >>*= fun () ->
        let message =
          Fmt.strf "Importing webhooks:\n%a"
            Fmt.(list ~sep:(unit "\n") Event.pp) events
        in
        DK.Transaction.commit t.priv.tr ~message >>*= fun () ->
        DK.Transaction.abort t.pub.tr >>= fun () ->
        merge t ~pub ~priv

  let sync ~webhook ~token ~pub ~priv t repos =
    assert (is_open t);
    sync_webhooks t ~token ~webhook ~priv ~pub repos >>*= fun t ->
    assert (is_open t);
    sync_repos ~token ~pub ~priv t repos >>*= fun t ->
    assert (is_open t);
    abort t >>= fun () ->
    ok t

  (* On startup, build the initial state by looking at the active
     repository in the public and private branch. Import the new
     repositories in the private branch, then merge it in the public
     branch. Finally call the GitHub API with the diff between the
     public and the private branch. *)
  let first_sync ~webhook ~token ~dry_updates ~pub ~priv =
    state "first-sync" ~old:None ~pub ~priv >>*= fun t ->
    Log.debug (fun l ->
        l "[first_sync]@;@[<2>priv:%a@]@;@[<2>pub=%a@]"
          pp_branch t.priv
          pp_branch t.pub
      );
    let repos =
      let r t = t.snapshot.Snapshot.repos in
      Repo.Set.union (r t.priv) (r t.pub)
    in
    begin
      if Repo.Set.is_empty repos then abort t >>= fun () -> ok t
      else sync ~webhook ~token ~pub ~priv t repos
    end >>*= fun t ->
    assert (is_closed t);
    let old = t in
    state "api-calls" ~old:(Some t) ~pub ~priv >>*= fun t ->
    abort t >>= fun () ->
    call_github_api ~dry_updates ~token ~old:old.priv.snapshot t >>*= fun t ->
    ok t

  let repos old t =
    let old = Snapshot.repos old.snapshot in
    let t   = Snapshot.repos t.snapshot in
    Repo.Set.(union (diff old t) (diff t old))

  (* The main synchonisation function: it is called on every change in
     the public or private branch. *)
  let sync_once ~webhook ~dry_updates ~token ~pub ~priv ~old =
    assert (is_closed old);
    state "sync-once" ~old:(Some old) ~pub ~priv >>*= fun t ->
    Log.debug (fun l -> l "[sync_once]@;old:%a@;new:%a" pp old pp t);
    call_github_api ~dry_updates ~token ~old:old.pub.snapshot t >>*= fun t ->
    let repos = Repo.Set.union (repos old.pub t.pub) (repos old.priv t.priv) in
    sync ~webhook ~token ~pub ~priv t repos >>*= fun t ->
    assert (is_closed t);
    ok t

  type t = State of state | Starting

  let empty = Starting

  let continue = function
    | Some s -> Lwt_switch.is_on s
    | None   -> true

  let process_webhook = function
    | None   -> None, fun _ -> fst (Lwt.task ())
    | Some w ->
      let watch r = API.Webhook.watch w r in
      let events () =
        let e = API.Webhook.events w in
        API.Webhook.clear w;
        e
      in
      let rec wait s =
        API.Webhook.wait w >>= fun () ->
        s ();
        wait s
      in
      let run s =
        Lwt.pick [
          API.Webhook.run w;
          wait s
        ]
      in
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
      let notify () =
        Log.info (fun l -> l "New webhooks detected");
        updates := true;
        Lwt_condition.signal cond ()
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
      Lwt.choose [ react () ; watch priv; watch pub; run_webhook notify ]
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
