open Astring

let src = Logs.Src.create "dkt-github" ~doc:"Github API abstraction for DataKit"
module Log = (val Logs.src_log src : Logs.LOG)

module type ELT = sig
  include Set.OrderedType
  val pp: t Fmt.t
end

module type SET = sig
  include Asetmap.Set.S
  val pp: t Fmt.t
end

module type MAP = sig
  include Asetmap.Map.S
  val pp: 'a Fmt.t -> 'a t Fmt.t
end

let pp_set (type a) k (module S: SET with type t = a) ppf (v:a) =
  if S.is_empty v then Fmt.string ppf "" else
    Fmt.pf ppf "@[<2>%s:@;%a@;@]" k S.pp v

let pp_field k pp ppf v = Fmt.pf ppf "@[<2>%s:@;%a@]" k pp v

let validate_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' | '_' | '.' -> ()
  | c -> invalid_arg (Fmt.strf "invalid character %c in reference name" c)

let trim_and_validate s =
  let s = String.trim s in
  String.iter validate_char s;
  s

module Set (E: ELT) = struct

  include Asetmap.Set.Make(E)

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

module Map (K: ELT) = struct
  include Asetmap.Map.Make(K)
  let pp v ppf t = Fmt.(list ~sep:(unit "@;") (pair K.pp v)) ppf (bindings t)
end

let pp_path = Fmt.(list ~sep:(unit "/") string)

module User = struct
  module X = struct
    type t = { name: string }
    let pp ppf t = Fmt.string ppf t.name
    let compare x y = String.compare x.name y.name
  end
  include X
  let v name = { name = trim_and_validate name }
  let name t = t.name
  module Set = Set (X)
  module Map = Map (X)
end

module Repo = struct

  type t = { user: User.t; repo: string }

  let v ~user ~repo =
   let repo = trim_and_validate repo in
   { user; repo }

  let of_string s = match String.cuts ~sep:"/" s with
    | [user; repo] ->
      let user = User.v user in
      (try Some (v ~user ~repo) with Invalid_argument _ -> None)
    | _ -> None

  let pp ppf t = Fmt.pf ppf "%a/%s" User.pp t.user t.repo
  let compare (x:t) (y:t) = Pervasives.compare x y
  type state = [`Monitored | `Ignored]

  let pp_state ppf = function
    | `Monitored -> Fmt.string ppf "+"
    | `Ignored   -> Fmt.string ppf "-"

  module X = struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end
  module Set = Set(X)
  module Map = Map(X)
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

  let compare (x:t) (y:t) = Pervasives.compare x y

end

let compare_fold fs x y =
  List.fold_left (fun acc f ->
      match acc with
      | 0 -> f x y
      | i -> i
    ) 0 (List.rev fs)

module Commit = struct

  type t = { repo: Repo.t; hash : string }

  let v repo hash = {repo; hash = String.trim hash }
  let pp ppf t = Fmt.pf ppf "{%a %s}" Repo.pp t.repo t.hash
  let pp_hash f h = Fmt.string f (String.with_range ~len:6 h)
  let hash t = t.hash
  let repo t = t.repo
  let compare_repo x y = Repo.compare x.repo y.repo
  let compare_hash x y = String.compare x.hash y.hash
  let equal (x:t) (y:t) = x = y

  let compare = compare_fold [
      compare_repo;
      compare_hash;
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

module Comment = struct
  type t = { id: int; user: User.t; body: string }
  let v ~id ~user ~body = { id; user; body }
  let id t = t.id
  let user t = t.user
  let body t = t.body
  let pp ppf t =
    let body = String.(with_range ~len:80 t.body) in
    Fmt.pf ppf "@[%d %a: %S@]" t.id User.pp t.user body
end

module PR = struct

  type t = {
    head: Commit.t;
    number: int;
    state: [`Open | `Closed];
    title: string;
    base: string;
    owner: string;
    comments: Comment.t array;
  }

  let v ?(state=`Open) ~title ?(base="master") ~owner ~comments head number =
    { state; title = String.trim title; base = String.trim base; head;
      number; owner; comments }

  type id = Repo.t * int

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
  let id t = repo t, t.number
  let commit t = t.head
  let commit_hash t = t.head.Commit.hash
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_num x y = Pervasives.compare x.number y.number
  let number t = t.number
  let title t = t.title
  let owner t = t.owner
  let comments t = t.comments
  let state t = t.state
  let close t = { t with state = `Closed }
  let same_id x y = repo x = repo y && number x = number y

  let compare = compare_fold [
      compare_repo;
      compare_num;
      Pervasives.compare;
    ]

  let pp ppf t =
    Fmt.pf ppf "{%a %d[%s] %s %s %a %S %a}"
      Repo.pp (repo t) t.number (commit_hash t) t.base t.owner pp_state
      t.state t.title Fmt.(Dump.array Comment.pp) t.comments

  let pp_id ppf (r, n) = Fmt.pf ppf "{%a %d}" Repo.pp r n

  let compare_id =
    let compare_repo x y = Repo.compare (fst x) (fst y) in
    let compare_num x y = Pervasives.compare (snd x) (snd y) in
    compare_fold [ compare_repo; compare_num ]

  module Id = struct
    type t = id
    let pp = pp_id
    let compare = compare_id
  end
  module IdSet = Set (Id)
  module Index = Map(Id)

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

  let index s =
    Set.fold (fun t acc ->
        let repo = repo t in
        let id = id t in
        let idx = match Repo.Map.find repo acc with
          | None     -> Index.singleton id t
          | Some idx -> Index.add id t idx
        in
        Repo.Map.add repo idx acc
      ) s Repo.Map.empty

end

module Status = struct

  type context = string list

  type t = {
    commit: Commit.t;
    context: string list;
    url: Uri.t option;
    description: string option;
    state: Status_state.t;
  }

  type id = Commit.t * string list

  let context t = match t.context with
    | [] -> ["default"]
    | l  -> l

  let id t = t.commit, t.context
  let repo t = t.commit.Commit.repo
  let commit t = t.commit
  let description t = t.description
  let state t = t.state
  let url t = t.url
  let pp_context = pp_path
  let commit_hash t = t.commit.Commit.hash
  let same_id x y = commit x = commit y && context x = context y
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_commit_hash x y =
    Pervasives.compare (commit_hash x) (commit_hash y)
  let compare_context x y = Pervasives.compare x.context y.context
  let compare_descr x y = Pervasives.compare x.description y.description
  let compare_state x y = Status_state.compare x.state y.state

  let compare_uri x y = match x.url, y.url with
    | None  , None   -> 0
    | Some x, Some y -> Uri.compare x y
    | Some _, None   -> 1
    | None  , Some _ -> -1

  (* To avoid:
     Github: GitHub API error: 422 Unprocessable Entity (WebDAV) (RFC 4918)
       -- Validation Failed
     Resource type: Status
     Field: description
     Code: custom
     Message: description is too long (maximum is 140 characters) *)
  let truncate_and_trim = function
    | None   -> None
    | Some s ->
      let s =
        if String.length s <= 140 then s else String.with_range s ~len:140
      in
      Some (String.trim s)

  let v ?description ?url commit context state =
    { description = truncate_and_trim description;
      url; commit; context; state }

  let compare = compare_fold [
      compare_repo;
      compare_commit_hash;
      compare_context;
      compare_uri;
      compare_descr;
      compare_state;
    ]

  let pp_opt k ppf v = match v with
    | None   -> ()
    | Some v -> Fmt.pf ppf " %s=%s" k v

  let map f = function None -> None | Some v -> Some (f v)

  let pp ppf t =
    Fmt.pf ppf "{%a %s:%a[%a]%a%a}"
      Repo.pp (repo t) (commit_hash t)
      pp_path t.context
      Status_state.pp t.state
      (pp_opt "url") (map Uri.to_string t.url)
      (pp_opt "descr") t.description

  let pp_id ppf (c, s) = Fmt.pf ppf "{%a %a}" Commit.pp c pp_path s

  let compare_id =
    let compare_commit x y = Commit.compare (fst x) (fst y) in
    let compare_context x y = Pervasives.compare (snd x) (snd y) in
    compare_fold [ compare_commit; compare_context ]

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

  module Index = Map(struct
      type t = id
      let compare = compare_id
      let pp = pp_id
    end)

  let index s =
    Set.fold (fun t acc ->
        let repo = repo t in
        let id = id t in
        let idx = match Repo.Map.find repo acc with
          | None     -> Index.singleton id t
          | Some idx -> Index.add id t idx
        in
        Repo.Map.add repo idx acc
      ) s Repo.Map.empty

end

module Ref = struct

  type name = string list

  type t = {
    head: Commit.t;
    name: string list;
  }

  type id = Repo.t * string list

  let v head name =
    let name = List.map trim_and_validate name in
    { head; name }

  let repo t = t.head.Commit.repo
  let id t = repo t, t.name
  let commit t = t.head
  let commit_hash t = t.head.Commit.hash
  let name t = t.name
  let compare_repo x y = Repo.compare (repo x) (repo y)
  let compare_name x y = Pervasives.compare x.name y.name
  let same_id x y = Repo.compare (repo x) (repo y) = 0 && name x = name y

  let compare = compare_fold [
      compare_repo;
      compare_name;
      Pervasives.compare;
    ]

  let pp_name = pp_path

  let pp ppf t =
    Fmt.pf ppf "{%a %a[%s]}" Repo.pp (repo t) pp_path t.name (commit_hash t)

  let pp_id ppf (r, p) = Fmt.pf ppf "{%a %a}" Repo.pp r pp_path p

  let compare_id =
    let compare_repo x y = Repo.compare (fst x) (fst y) in
    let compare_context x y = Pervasives.compare (snd x) (snd y) in
    compare_fold [ compare_repo; compare_context ]

  module Id = struct
    type t = id
    let pp = pp_id
    let compare = compare_id
  end
  module IdSet = Set (Id)
  module Index = Map(Id)

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

  let index s =
    Set.fold (fun t acc ->
        let repo = repo t in
        let id = id t in
        let idx = match Repo.Map.find repo acc with
          | None     -> Index.singleton id t
          | Some idx -> Index.add id t idx
        in
        Repo.Map.add repo idx acc
      ) s Repo.Map.empty

  type event = [`Created of t | `Updated of t  | `Removed of id]

  let pp_event ppf = function
    | `Created t  -> Fmt.pf ppf "+%a" pp t
    | `Updated t  -> Fmt.pf ppf "*%a" pp t
    | `Removed id -> Fmt.pf ppf "-%a" pp_id id

end

module Event = struct

  type t =
    | Repo of (Repo.state * Repo.t)
    | PR of PR.t
    | Status of Status.t
    | Ref of Ref.event
    | Other of (Repo.t * string)

  let of_repo s r = Repo (s, r)
  let of_pr x = PR x
  let of_status x = Status x
  let of_ref x = Ref x
  let of_other x y = Other (x, y)

  let pp ppf = function
    | Repo(s,r) -> Fmt.pf ppf "Repo: %a%a" Repo.pp_state s Repo.pp r
    | PR pr     -> Fmt.pf ppf "PR: %a" PR.pp pr
    | Status s  -> Fmt.pf ppf "Status: %a" Status.pp s
    | Ref r     -> Fmt.pf ppf "Ref: %a" Ref.pp_event r
    | Other o   -> Fmt.pf ppf "Other: %s" @@ snd o

  let repo = function
    | Repo r   -> snd r
    | PR pr    -> PR.repo pr
    | Status s -> Status.repo s
    | Ref (`Removed r) -> fst r
    | Ref (`Created r
          |`Updated r) -> Ref.repo r

    | Other o  -> fst o

end

module Elt = struct

  type t = [
    | `Repo of Repo.t
    | `Commit of Commit.t
    | `PR of PR.t
    | `Status of Status.t
    | `Ref of Ref.t
  ]

  let pp ppf = function
    | `Repo r   -> Repo.pp ppf r
    | `Commit c -> Commit.pp ppf c
    | `PR pr    -> PR.pp ppf pr
    | `Status s -> Status.pp ppf s
    | `Ref r    -> Ref.pp ppf r

  let compare x y = match x, y with
    | `Repo x  , `Repo y   -> Repo.compare x y
    | `Commit x, `Commit y -> Commit.compare x y
    | `PR x    , `PR y     -> PR.compare x y
    | `Status x, `Status y -> Status.compare x y
    | `Ref x   , `Ref y    -> Ref.compare x y
    | `Repo _  , _ -> 1
    | _, `Repo _   -> -1
    | `Commit _, _ -> 1
    | _, `Commit _ -> -1
    | `PR _, _     -> 1
    | _, `PR _     -> -1
    | `Status _, _ -> 1
    | _, `Status _ -> -1

  type id = [
    | `Repo of Repo.t
    | `Commit of Commit.t
    | `PR of PR.id
    | `Status of Status.id
    | `Ref of Ref.id
  ]

  let id = function
    | `Repo r   -> `Repo r
    | `Commit c -> `Commit c
    | `PR pr    -> `PR (PR.id pr)
    | `Status s -> `Status (Status.id s)
    | `Ref r    -> `Ref (Ref.id r)

  let pp_id ppf = function
    | `Repo r   -> Repo.pp ppf r
    | `Commit c -> Commit.pp ppf c
    | `PR pr    -> PR.pp_id ppf pr
    | `Status s -> Status.pp_id ppf s
    | `Ref r    -> Ref.pp_id ppf r

  let compare_id x y = match x, y with
    | `Repo x  , `Repo y   -> Repo.compare x y
    | `Commit x, `Commit y -> Commit.compare x y
    | `PR x    , `PR y     -> PR.compare_id x y
    | `Status x, `Status y -> Status.compare_id x y
    | `Ref x   , `Ref y    -> Ref.compare_id x y
    | `Repo _  , _ -> 1
    | _, `Repo _   -> -1
    | `Commit _, _ -> 1
    | _, `Commit _ -> -1
    | `PR _, _     -> 1
    | _, `PR _     -> -1
    | `Status _, _ -> 1
    | _, `Status _ -> -1

  module IdSet = struct
    include Set(struct
        type t = id
        let pp = pp_id
        let compare = compare_id
      end)

    let repos s =
      fold
        (fun e acc -> match e with `Repo r -> Repo.Set.add r acc | _ -> acc)
        s Repo.Set.empty

    let prs s =
      fold
        (fun e acc -> match e with `PR pr -> PR.IdSet.add pr acc | _ -> acc)
        s PR.IdSet.empty

    let refs s =
      fold
        (fun e acc -> match e with `Ref r -> Ref.IdSet.add r acc | _ -> acc)
        s Ref.IdSet.empty

    let of_repos s = Repo.Set.fold (fun r -> add (`Repo r)) s empty
    let of_prs s = PR.Set.fold (fun p -> add (`PR (PR.id p))) s empty
    let of_refs s = Ref.Set.fold (fun r -> add (`Ref (Ref.id r))) s empty

  end

  module Set = struct
    include Set(struct
        type nonrec t = t
        let pp = pp
        let compare = compare
      end)
    let ids t = elements t |> List.map id |> IdSet.of_list

    let prs t =
      elements t
      |> List.fold_left (fun acc -> function
          | `PR pr -> PR.Set.add pr acc
          | _      -> acc
        ) PR.Set.empty

    let refs t =
      elements t
      |> List.fold_left (fun acc -> function
          | `Ref r -> Ref.Set.add r acc
          | _      -> acc
        ) Ref.Set.empty

    let status t =
      elements t
      |> List.fold_left (fun acc -> function
          | `Status s -> Status.Set.add s acc
          | _         -> acc
        ) Status.Set.empty

  end

end

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

  let is_empty s =
    Repo.Set.is_empty s.repos &&
    Commit.Set.is_empty s.commits &&
    Status.Set.is_empty s.status &&
    PR.Set.is_empty s.prs &&
    Ref.Set.is_empty s.refs

  let elts t =
    let repos = Repo.Set.elements t.repos |> List.map (fun x -> `Repo x) in
    let commits =
      Commit.Set.elements t.commits |> List.map (fun x -> `Commit x)
    in
    let status =
      Status.Set.elements t.status |> List.map (fun x -> `Status x)
    in
    let prs = PR.Set.elements t.prs |> List.map (fun x -> `PR x) in
    let refs = Ref.Set.elements t.refs |> List.map (fun x -> `Ref x) in
    let (++) = Elt.Set.union in
    let (l) = Elt.Set.of_list in
    l repos ++ l commits ++ l status ++ l prs ++ l refs

  let pp ppf t =
    if compare t empty = 0 then Fmt.string ppf "{}"
    else
      Fmt.pf ppf "{%a%a%a%a%a}"
        (pp_set "repos"   (module Repo.Set)) t.repos
        (pp_set "prs"     (module PR.Set)) t.prs
        (pp_set "refs"    (module Ref.Set)) t.refs
        (pp_set "commits" (module Commit.Set)) t.commits
        (pp_set "status"  (module Status.Set)) t.status

  let union x y = {
    repos   = Repo.Set.union x.repos y.repos;
    commits = Commit.Set.union x.commits y.commits;
    status  = Status.Set.union x.status y.status;
    prs     = PR.Set.union x.prs y.prs;
    refs    = Ref.Set.union x.refs y.refs;
  }

  let v ~repos ~commits ~status ~prs ~refs =
    let repos =
      let (++) = Repo.Set.union in
      repos ++ Commit.Set.repos commits ++ Status.Set.repos status
      ++ PR.Set.repos prs ++ Ref.Set.repos refs
    in
    let commits =
      let (++) = Commit.Set.union in
      commits ++ Status.Set.commits status ++ PR.Set.commits prs
      ++ Ref.Set.commits refs
    in
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

  type keep = { f: 'a . ('a -> Repo.t) -> 'a -> bool }

  let with_repo r t = { t with repos = Repo.Set.add r t.repos }

  let without_repo_f keep t =
    let repos = Repo.Set.filter (keep.f (fun x -> x)) t.repos in
    let prs = PR.Set.filter (keep.f PR.repo) t.prs in
    let refs = Ref.Set.filter (keep.f Ref.repo) t.refs in
    let commits = Commit.Set.filter (keep.f Commit.repo) t.commits in
    let status = Status.Set.filter (keep.f Status.repo) t.status in
    { repos; prs; refs; commits; status }

  let without_repo repo =
    without_repo_f { f = fun f r -> Repo.compare (f r) repo <> 0 }

  let without_repos repos =
    without_repo_f { f = fun f r -> not (Repo.Set.mem (f r) repos) }

  let without_commit { Commit.repo; hash } t =
    let keep x = repo <> Commit.repo x || hash <> Commit.hash x in
    { t with commits = Commit.Set.filter keep t.commits }

  let with_commit c t =
    { t with commits = Commit.Set.add c t.commits;
             repos   = Repo.Set.add (Commit.repo c) t.repos }

  let without_pr (r, id) t =
    let keep pr = r  <> PR.repo pr || id <>  pr.PR.number in
    { t with prs = PR.Set.filter keep t.prs }

  let add_pr pr t =
    { t with prs     = PR.Set.add pr t.prs;
             commits = Commit.Set.add (PR.commit pr) t.commits;
             repos   = Repo.Set.add (PR.repo pr) t.repos }

  let with_pr pr t =
    let id = PR.repo pr, pr.PR.number in
    add_pr pr (without_pr id t)

  let without_status (s, l) t =
    let keep x = s <> Status.commit x || l <> x.Status.context in
    { t with status = Status.Set.filter keep t.status }

  let add_status t s =
    { t with status  = Status.Set.add s t.status;
             commits = Commit.Set.add (Status.commit s) t.commits;
             repos   = Repo.Set.add (Status.repo s) t.repos }

  let with_status s t = add_status (without_status (Status.id s) t) s

  let without_ref (r, l) t =
    let keep x = r <> Ref.repo x || l <> x.Ref.name in
    { t with refs = Ref.Set.filter keep t.refs }

  let add_ref r t =
    { t with refs    = Ref.Set.add r t.refs;
             commits = Commit.Set.add (Ref.commit r) t.commits;
             repos   = Repo.Set.add (Ref.repo r) t.repos }

  let with_ref r t = add_ref r (without_ref (Ref.id r) t)

  let with_elt e t = match e with
    | `Repo r   -> with_repo r t
    | `Commit c -> with_commit c t
    | `PR pr    -> with_pr pr t
    | `Ref r    -> with_ref r t
    | `Status s -> with_status s t

  let without_elt e t = match e with
    | `Repo r   -> without_repo r t
    | `Commit c -> without_commit c t
    | `PR pr    -> without_pr pr t
    | `Ref r    -> without_ref r t
    | `Status s -> without_status s t

  let with_elts = Elt.Set.fold with_elt
  let without_elts = Elt.IdSet.fold without_elt

  let find (id:Elt.id) t =
    match
      elts t
      |> Elt.Set.elements
      |> List.find (fun e -> Elt.compare_id (Elt.id e) id = 0)
    with
    | exception Not_found -> None
    | e -> Some (e:Elt.t)

  let with_event = function
    | Event.Repo (`Ignored,r) -> without_repo r
    | Event.Repo (_, r)       -> with_repo r
    | Event.PR pr             -> with_pr pr
    | Event.Ref (`Removed  r) -> without_ref r
    | Event.Ref (`Created r
                |`Updated r)  -> with_ref r
    | Event.Status s          -> with_status s
    | Event.Other _           -> fun t -> t

  let with_events es t = List.fold_left (fun acc e -> with_event e acc) t es

  type diff = { remove: Elt.IdSet.t; update: Elt.Set.t }

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
      Log.debug (fun l -> l "[prune]+status:@;%a" Status.Set.pp open_status);
      Log.debug (fun l -> l "[prune]-status:@;%a" Status.Set.pp closed_status);
      let repos   = Repo.Set.singleton repo in
      let status  = open_status in
      let prs     = open_prs in
      let commits = open_commits in
      { repos; status; prs; refs; commits }
    in
    Repo.Set.fold (fun r acc -> union acc (aux r)) t.repos empty

  let pp_diff ppf t =
    Fmt.pf ppf "@[[%a@;%a]@]"
      (pp_field "update" Elt.Set.pp) t.update
      (pp_field "remove" Elt.IdSet.pp) t.remove

  let diff x y =
    let mk t repos skip_pr skip_ref skip_status skip_commit =
      let neg f x = not (f x) in
      let prs = PR.Set.filter (neg skip_pr) t.prs in
      let refs = Ref.Set.filter (neg skip_ref) t.refs in
      let status = Status.Set.filter (neg skip_status) t.status in
      let commits = Commit.Set.filter (neg skip_commit) t.commits in
      { repos; prs; refs; commits; status }
    in
    let remove =
      let repos = Repo.Set.diff y.repos x.repos in
      let skip_pr pr = PR.Set.exists (PR.same_id pr) x.prs in
      let skip_ref r = Ref.Set.exists (Ref.same_id r) x.refs in
      let skip_status s = Status.Set.exists (Status.same_id s) x.status in
      let skip_commit c = Commit.Set.exists (Commit.equal c) x.commits in
      mk y repos skip_pr skip_ref skip_status skip_commit
      |> elts |> Elt.Set.ids
    in
    let update =
      let repos = Repo.Set.diff x.repos y.repos in
      let skip_commit c = Commit.Set.mem c y.commits in
      let skip_pr pr = PR.Set.mem pr y.prs in
      let skip_ref r = Ref.Set.mem r y.refs in
      let skip_status s = Status.Set.mem s y.status in
      mk x repos skip_pr skip_ref skip_status skip_commit
      |> elts
    in
    let r = { remove; update } in
    Log.debug (fun l -> l "Snapshot.diff@;x=%a@;y=%a@;r=%a" pp x pp y pp_diff r);
    r

end

module Diff = struct

  open Snapshot

  type t = Snapshot.diff
  let empty = { update = Elt.Set.empty; remove = Elt.IdSet.empty }

  let update t = t.update
  let remove t = t.remove

  let compare x y =
    match Elt.Set.compare x.update y.update with
    | 0 -> Elt.IdSet.compare x.remove y.remove
    | i -> i

  let pp = Snapshot.pp_diff

  let commit_message t =
    let updates = Elt.Set.cardinal t.update in
    let removes = Elt.IdSet.cardinal t.remove in
    if updates = 0 && removes = 0 then Fmt.strf "No changes!"
    else if updates = 0 && removes = 1 then
      Fmt.strf "1 item removed@;@;@[<2>%a@]" Elt.IdSet.pp t.remove
    else if updates = 0 && removes > 1 then
      Fmt.strf "%d items removed@;@;@[<2>%a@]" removes Elt.IdSet.pp t.remove
    else if removes = 0 && updates = 1 then
      Fmt.strf "1 item updated@;@;@[<2>%a@]" Elt.Set.pp t.update
    else if removes = 0 && updates > 1 then
      Fmt.strf "%d items updated@;@;@[<2>%a@]" updates Elt.Set.pp t.update
    else
      Fmt.strf "%d items modified@;@;@[Updated@;<2>%a@]@;@;\
                @[Removed@;<2>%a@]"
        (updates+removes) Elt.Set.pp t.update Elt.IdSet.pp t.remove

  let is_empty t = Elt.IdSet.is_empty t.remove && Elt.Set.is_empty t.update
  let with_update s t = { t with update = Elt.Set.add s t.update }
  let with_remove s t = { t with remove = Elt.IdSet.add s t.remove }

  let apply d t =
    t
    |> Snapshot.without_elts d.remove
    |> Snapshot.with_elts d.update

end

module Capabilities = struct

  type op = [`Read | `Write | `Excl ]

  type resource = [
    | `Repo of string list
    | `PR
    | `Commit
    | `Status of string list
    | `Ref
    | `Webhook
  ]

  exception Error of string * string

  let pp_resource ppf = function
    | `Repo []   -> Fmt.string ppf "repo"
    | `Repo p    -> Fmt.pf ppf "repo[%a]" pp_path p
    | `PR        -> Fmt.string ppf "pr"
    | `Commit    -> Fmt.string ppf "commit"
    | `Status [] -> Fmt.string ppf "status"
    | `Status p  -> Fmt.pf ppf "status[%a]" pp_path p
    | `Ref       -> Fmt.string ppf "ref"
    | `Webhook   -> Fmt.string ppf "webhook"

  let parse_kv s =
    match String.cut ~sep:"[" s with
    | Some (s, c) ->
      (* remove trailing ']' *)
      let c = String.with_range ~len:(String.length c - 1) c in
      let c = String.cuts ~sep:"/" c in
      Some (s, c)
    | _ ->
      None

  let parse_resource = function
    | "pr"      -> `PR
    | "commit"  -> `Commit
    | "repo"    -> `Repo []
    | "status"  -> `Status []
    | "ref"     -> `Ref
    | "webhook" -> `Webhook
    | "*"       -> `Default
    | s         ->
      match parse_kv s with
      | Some ("status", l) -> `Status l
      | Some ("repo"  , l) -> `Repo l
      | _                  -> raise (Error (s, "invalid resource"))

  let pp_op ppf = function
    | `Read  -> Fmt.string ppf "read"
    | `Write -> Fmt.string ppf "write"
    | `Excl  -> Fmt.string ppf "excl"

  module X = struct

    type t = { read: bool; write: bool; excl: bool }
    let none = { read = false; write = false; excl = false }
    let all = { read = true; write = true; excl = false }

    let allow t = function
      | `Read  -> { t with read  = true }
      | `Write -> { t with write = true }
      | `Excl  -> { t with excl  = true }

    let disallow t = function
      | `Read  -> { t with read  = false }
      | `Write -> { t with write = false }
      | `Excl  -> { t with excl  = false }

    let pp_aux ppf (f, s) = match f with
      | true  -> Fmt.string ppf s
      | false -> Fmt.string ppf ""

    let pp_r ppf t = pp_aux ppf (t.read , "r")
    let pp_w ppf t = pp_aux ppf (t.write, "w")
    let pp_l ppf t = pp_aux ppf (t.excl , "x")

    let pp ppf t = Fmt.pf ppf "%a%a%a" pp_l t pp_r t pp_w t

    let parse s =
      String.fold_left (fun acc -> function
          | 'x' -> { acc with excl  = true }
          | 'r' -> { acc with read  = true }
          | 'w' -> { acc with write = true }
          | _   -> raise (Error (s, "invalid capacities"))
        ) none s

    let check t op =
      let r = match op with
      | `Read  -> t.read  || t.excl
      | `Write -> t.write || t.excl
      | `Excl  -> t.excl
      in
      Log.debug (fun l -> l "X.check %a %a %b" pp t pp_op op r);
      r

  end

  type t = {
    default: X.t;
    extra  : (resource * X.t) list;
  }

  let sort_extra extra = List.sort (fun (x, _) (y, _) -> compare x y) extra

  let equal x y =
    x.default = y.default &&
    List.length x.extra = List.length y.extra &&
    List.for_all2
      (fun (r1, x1) (r2, x2) -> r1 = r2 && x1 = x2)
      (sort_extra x.extra) (sort_extra y.extra)

  let v ?(extra=[]) default = { default; extra = sort_extra extra }

  let none = v X.none
  let all = v X.all

  let pp ppf t =
    let pp_one = Fmt.(pair ~sep:(unit ":")) pp_resource X.pp in
    let pp = Fmt.(list ~sep:(unit ",")) pp_one in
    let extra =
      if t.default <> X.none then t.extra
      else List.filter (fun (_, v) -> v <> X.none)  t.extra
    in
    if extra = [] then
      Fmt.pf ppf "*:%a" X.pp t.default
    else if t.default = X.none then
      Fmt.pf ppf "%a" pp extra
    else
      Fmt.pf ppf "*:%a,%a" X.pp t.default pp extra

  let parse_resource_ops s = match String.cut ~sep:":" s with
    | None        -> raise (Error (s, "missing ':'"))
    | Some (r, c) -> parse_resource r, X.parse c

  let parse s =
    try
      let caps =
        String.cuts ~sep:"," s
        |> List.map parse_resource_ops
      in
      let default =
        try List.find (fun (r, _) -> r = `Default) caps |> snd
        with Not_found -> X.none
      in
      let extra = List.fold_left (fun acc -> function
          | `Default, _       -> acc
          | #resource as r, x -> (r, x) :: acc
        ) [] caps
      in
      `Ok (v default ~extra)
    with Error (s, msg) ->
      Fmt.kstrf (fun e -> `Error e) "%s: %s" s msg

  let apply f t op = function
    | `Default       -> { t with default = f t.default op }
    | #resource as r ->
      try
        let x = List.assoc r t.extra in
        let x = f x op in
        let extra = List.filter (fun (s, _) -> s <> r) t.extra in
        v t.default ~extra:((r, x) :: extra)
      with Not_found ->
        let x = f X.none op in
        v t.default ~extra:((r, x) :: t.extra)

  let allow = apply X.allow
  let disallow = apply X.disallow

  let rec starts_with ~prefix l = match prefix, l with
    | []  , _    -> true
    | _   , []   -> false
    | a::b, c::d -> a=c && starts_with ~prefix:b d

  let find_longuest_prefix l default extra =
      List.fold_left (fun (n, _ as acc) (k, v) -> match k with
        | prefix when starts_with ~prefix l && List.length prefix > n
          -> (List.length prefix, Some v)
        | _ -> acc
      ) (0, None) extra
      |> function
      | _, None   -> default
      | _, Some s -> s

  let statuses t =
    List.fold_left (fun acc -> function
        | (`Status s, v) -> (s, v) :: acc
        | _ -> acc
      ) [] t.extra

  let repos t =
    List.fold_left (fun acc -> function
        | (`Repo r, v) -> (r, v) :: acc
        | _ -> acc
      ) [] t.extra

  let x t = function
    | `Status l -> find_longuest_prefix l t.default (statuses t)
    | `Repo r   -> find_longuest_prefix r t.default (repos t)
    | r         -> try List.assoc r t.extra with Not_found -> t.default

  let check t op (r:resource) =
    let allowed = X.check (x t r) op in
    if not allowed then
      Log.info (fun l ->
          l "%a: %a is denied (current policy is %a)"
            pp_resource r pp_op op pp t
        );
    allowed

  let filter_aux t op = function
    | `Commit _ -> X.check (x t `Commit) op
    | `PR _     -> X.check (x t `PR) op
    | `Ref _    -> X.check (x t `Ref) op
    | `Repo r   ->
      let { Repo.user; repo } = r in
      X.check (x t (`Repo [repo; User.name user])) op

  let filter_elt t op (e:Elt.t) = match e with
    | `Commit _ | `PR _ | `Ref _ | `Repo _ as x -> filter_aux t op x
    | `Status s -> X.check (x t (`Status (Status.context s))) op

  let filter_elt_id t op (e:Elt.id) = match e with
    | `Commit _ | `PR _ | `Ref _ | `Repo _ as x -> filter_aux t op x
    | `Status s -> X.check (x t (`Status (snd s))) op

  let filter_diff t op diff =
    let update = Elt.Set.filter (filter_elt t op) diff.Snapshot.update in
    let remove =
      Elt.IdSet.filter (filter_elt_id t op) diff.Snapshot.remove
    in
    { Snapshot.update; remove }

end

module type API = sig
  type token
  type 'a result = ('a, string) Result.result Lwt.t
  val user_exists: token -> user:User.t -> bool result
  val repo_exists: token -> Repo.t -> bool result
  val repos: token -> user:User.t -> Repo.t list result
  val status: token -> Commit.t -> Status.t list result
  val set_status: token -> Status.t -> unit result
  val set_ref: token -> Ref.t -> unit result
  val remove_ref: token -> Repo.t -> string list -> unit result
  val set_pr: token -> PR.t -> unit result
  val prs: token -> Repo.t -> PR.t list result
  val pr: token -> PR.id -> PR.t option result
  val refs: token -> Repo.t -> Ref.t list result
  val ref: token -> Ref.id -> Ref.t option result
  val events: token -> Repo.t -> Event.t list result
  module Webhook: sig
    type t
    val v: token -> Uri.t -> t
    val run: t -> unit Lwt.t
    val repos: t -> Repo.Set.t
    val watch: t -> Repo.t -> unit Lwt.t
    val events: t -> Event.t list Lwt.t
    val wait: t -> unit Lwt.t
    val clear: t -> unit
  end
end
