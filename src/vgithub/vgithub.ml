open Result
open Astring

let src = Logs.Src.create "vgithub" ~doc:"Virtual Github API"
module Log = (val Logs.src_log src : Logs.LOG)

let err_invalid_status s = Vfs.error "%S: invalid status" s


module type ELT = sig
  include Set.OrderedType
  val pp: t Fmt.t
end

module Set (E: ELT) = struct

  include Set.Make(E)

  let pp ppf t = Fmt.(list ~sep:(unit "@ ") E.pp) ppf (elements t)

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

module PR = struct

  type t = {
    user: string;
    repo: string;
    number: int;
    state: [`Open | `Closed];
    head: string; (* SHA1 *)
    title: string;
  }

  let compare = Pervasives.compare

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

  let pp ppf t =
    Fmt.pf ppf "[%s/%s#%d, state: %a, head: %s, title: %S]"
      t.user t.repo t.number pp_state t.state t.head t.title

  let repo { user; repo; _ } = { Repo.user; repo }

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

end

module Status = struct

  type t = {
    user: string;
    repo: string;
    commit: string;
    context: string list;
    url: string option;
    description: string option;
    state: Status_state.t;
  }

  let compare = Pervasives.compare

  let pp ppf t =
    let pp_opt k ppf v = match v with
      | None   -> ()
      | Some v -> Fmt.pf ppf " %s=%s," k v
    in

    Fmt.pf ppf "[%s/%s:%s:%a,%a%a %a]"
      t.user t.repo t.commit
      Fmt.(list ~sep:(unit "/") string) t.context
      (pp_opt "url") t.url
      (pp_opt "description") t.description
      Status_state.pp t.state

  let path t = match t.context with
    | [] -> ["default"]
    | l  -> l

  let repo { user; repo; _ } = { Repo.user; repo }

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)

end

module Event = struct

  type t =
    | PR of PR.t
    | Status of Status.t
    | Other of string

  let pp ppf = function
    | PR pr    -> Fmt.pf ppf "PR: %a" PR.pp pr
    | Status s -> Fmt.pf ppf "Status: %a" Status.pp s
    | Other s  -> Fmt.pf ppf "Other: %s" s

  module Set = Set(struct
      type nonrec t = t
      let pp = pp
      let compare = compare
    end)
end

module type API = sig
  type token
  val user_exists: token -> user:string -> bool Lwt.t
  val repo_exists: token -> user:string -> repo:string -> bool Lwt.t
  val repos: token -> user:string -> string list Lwt.t
  val status: token -> user:string -> repo:string -> commit:string ->
    Status.t list Lwt.t
  val set_status: token -> Status.t -> unit Lwt.t
  val set_pr: token -> PR.t -> unit Lwt.t
  val prs: token -> user:string -> repo:string -> PR.t list Lwt.t
  val events: token -> user:string -> repo:string -> Event.t list Lwt.t
end

module Make (API: API) = struct

  open Lwt.Infix

  type t = {
    token: API.token;
    user: string;
    repo: string;
  }

  (* /github.com/${USER}/${REPO}/commit/${SHA1}/status/${S} *)
  let commit_status_dir t ?(extra_dirs=fun () -> []) s =
    Logs.debug (fun l ->
        l "commit_status_file %s/%s %a" t.user t.repo Status.pp s
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
      API.set_status t.token new_status;
    in
    let state = Vfs.File.command ~init (fun str ->
        match Status_state.of_string str with
        | None   -> err_invalid_status str
        | Some s ->
          if s = !current_state then Vfs.ok (str ^ "\n")
          else (
            current_state := s;
            set_status () >>= fun () ->
            Vfs.ok (Status_state.to_string s ^ "\n");
          )
      ) in
    let descr = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_descr then Vfs.ok (str ^ "\n")
        else (
          current_descr := Some str;
          set_status () >>= fun () ->
          Vfs.ok (str ^ "\n")
        )
      ) in
    let url = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_url then Vfs.ok (str ^ "\n")
        else (
          current_url := Some str;
          set_status () >>= fun () ->
          Vfs.ok (str ^ "\n")
        )
      ) in
    let dir = [
      Vfs.Inode.file "state"  state;
      Vfs.Inode.file "descr"  descr;
      Vfs.Inode.file "url"    url;
    ] in
    Vfs.Dir.of_list (fun () -> Vfs.ok @@ dir @ extra_dirs ())

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
  let commit_status_root t commit =
    Log.debug (fun l -> l "commit_status_root %s/%s %s" t.user t.repo commit);
    let status =
      ref @@ lazy (API.status t.token ~user:t.user ~repo:t.repo ~commit)
    in
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
      | None   -> Vfs.Dir.of_list (fun () -> Vfs.ok @@ childs ())
      | Some s -> commit_status_dir t ~extra_dirs:childs s
    in
    let ls () =
      Lazy.force !status >>= fun s -> s
      |> List.map (fun s -> Status.path s, s)
      |> sort_by_hd
      |> List.map (fun (name, childs) -> Vfs.Inode.dir name @@ inodes childs)
      |> Vfs.ok
    in
    let lookup name =
      Log.debug (fun l -> l "lookup %s" name);
      try
        Lazy.force !status >>= fun s -> s
        |> List.map (fun s -> Status.path s, s)
        |> sort_by_hd
        |> List.assoc name
        |> inodes
        |> Vfs.Inode.dir name
        |> Vfs.ok
      with Not_found ->
        Vfs.File.err_no_entry
    in
    let mkdir name =
      Log.debug (fun l -> l "mkdir %s" name);
      let new_status = {
        user = t.user; repo = t.repo; commit;
        Status.context = [name];
        url = None;
        description = None;
        state = `Pending;
      } in
      API.set_status t.token new_status >>= fun () ->
      status := lazy (API.status t.token ~user:t.user ~repo:t.repo ~commit);
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
      let status = Vfs.Inode.dir "status" @@ commit_status_root t commit in
      Vfs.Inode.dir commit @@ Vfs.Dir.of_list (fun () -> Vfs.ok [status])
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
    let dirs () = Vfs.ok [
      Vfs.Inode.file "head"  @@ pr_head t pr;
    ] in
    Vfs.Dir.of_list dirs

  (* /github.com/${USER}/${REPO}/pr *)
  let pr_root t =
    Logs.debug (fun l -> l "pr_root %s/%s" t.user t.repo);
    let prs () =
      API.prs t.token ~user:t.user ~repo:t.repo >>= fun prs ->
      List.map (fun pr ->
          Vfs.Inode.dir (string_of_int pr.PR.number) @@ pr_dir t pr
        ) prs
      |> Vfs.ok
    in
    Vfs.Dir.of_list prs

  (* /github.com/${USER}/${REPO}/events *)
  let repo_events t =
    let open Lwt.Infix in
    Logs.debug (fun l -> l "repo_events %s/%s" t.user t.repo);
    let data () =
      let buf = Buffer.create 1024 in
      let ppf = Format.formatter_of_buffer buf in
      API.events t.token ~user:t.user ~repo:t.repo >|= fun events ->
      List.iter (Fmt.pf ppf "%a\n" Event.pp) events;
      Buffer.contents buf
    in
    let length () = Lwt.return 0 in
    Vfs.File.status ~length data

  (* /github.com/${USER}/${REPO} *)
  let repo_dir t =
    Logs.debug (fun l -> l "repo_root %s/%s" t.user t.repo);
    API.repo_exists t.token ~user:t.user ~repo:t.repo >|= fun repo_exists ->
    if not repo_exists then None
    else
      let files = Vfs.ok [
        Vfs.Inode.file "events" @@ repo_events t;
        Vfs.Inode.dir  "pr"     @@ pr_root t;
        Vfs.Inode.dir  "commit" @@ commit_root t;
      ] in
      let dir = Vfs.Dir.of_list (fun () -> files) in
      Some (Vfs.Inode.dir t.repo dir)

  (* /github.com/${USER}/ *)
  let user_dir ~token ~user =
    Logs.debug (fun l -> l "user_root %s/" user);
    API.user_exists token ~user >>= fun exists_user ->
    if not exists_user then Vfs.Dir.err_no_entry
    else
      let ls () =
        API.repos token ~user >>= fun r ->
        Lwt_list.rev_map_p (fun repo -> repo_dir { token; user; repo }) r
        >>= fun r ->
        List.fold_left (fun acc -> function
            | None   -> acc
            | Some x -> x :: acc)
          [] r
        |> Vfs.ok
      in
      let remove _ = Vfs.Dir.err_read_only in
      let lookup repo =
        repo_dir { token; user; repo } >>= function
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

open Lwt.Infix
open Datakit_path.Infix

let ( >>*= ) x f =
  x >>= function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

let ok x = Lwt.return (Ok x)

(* our 9p server seems to dead-lock when more than 100 fids are
   used. In doubt, try to limit the number of 9p operations to
   50... *)
let pool9p = Lwt_pool.create 50 (fun () -> Lwt.return_unit)

let list_iter_p f l =
  Lwt_list.map_p f l >|= fun l ->
  List.fold_left (fun acc x -> match acc, x with
      | Ok (), Ok ()            -> Ok ()
      | Error e, _ | _, Error e -> Error e
    ) (Ok ()) (List.rev l)

let list_iter_s f l =
  Lwt_list.map_s f l >|= fun l ->
  List.fold_left (fun acc x -> match acc, x with
      | Ok (), Ok ()            -> Ok ()
      | Error e, _ | _, Error e -> Error e
    ) (Ok ()) (List.rev l)

let list_map_p ?pool f l =
  let f x = match pool with
    | None   -> f x
    | Some p -> Lwt_pool.use p (fun () -> f x)
  in
  Lwt_list.map_p f l >|= fun l ->
  List.fold_left (fun acc x -> match acc, x with
      | Ok acc, Ok x            -> Ok (x :: acc)
      | Error e, _ | _, Error e -> Error e
    ) (Ok []) (List.rev l)

let list_map_s f l =
  Lwt_list.map_s f l >|= fun l ->
  List.fold_left (fun acc x -> match acc, x with
      | Ok acc, Ok x            -> Ok (x :: acc)
      | Error e, _ | _, Error e -> Error e
    ) (Ok []) (List.rev l)

module Snapshot = struct

  type t = {
    repos : Repo.Set.t;
    status: Status.Set.t;
    prs   : PR.Set.t;
  }

  let repos t = t.repos
  let status t = t.status
  let prs t = t.prs

  let empty =
    { repos = Repo.Set.empty; status = Status.Set.empty; prs = PR.Set.empty }

  let union x y = {
    repos  = Repo.Set.union x.repos y.repos;
    status = Status.Set.union x.status y.status;
    prs    = PR.Set.union x.prs y.prs;
  }

  let create ?(repos=Repo.Set.empty) ~status ~prs () =
    let repos =
      Status.Set.fold (fun x s -> Repo.Set.add (Status.repo x) s) status repos
    in
    let repos =
      PR.Set.fold (fun x s -> Repo.Set.add (PR.repo x) s) prs repos
    in
    { repos; status; prs }

  let add_repo t r = { t with repos = Repo.Set.add r t.repos }
  let add_pr t pr = { t with prs = PR.Set.add pr t.prs }
  let add_status t ss = { t with status = Status.Set.union ss t.status }

  let compare x y =
    match Repo.Set.compare x.repos y.repos with
    | 0 ->
      begin match Status.Set.compare x.status y.status with
        | 0 -> PR.Set.compare x.prs y.prs
        | i -> i
      end
    | i -> i

  let pp ppf t =
    Fmt.pf ppf "@[repos: %a@, status: %a@, prs: %a@]"
      Repo.Set.pp t.repos Status.Set.pp t.status PR.Set.pp t.prs

end

module Diff = struct

  type t = {
    user: string;
    repo: string;
    id  : [ `PR of int | `Status of string * string list | `Unknown ]
  }

  let pp_id ppf = function
    | `PR n          -> Fmt.pf ppf "#%d" n
    | `Status (s, l) -> Fmt.pf ppf "%s:%a" s Fmt.(list ~sep:(unit "/") string) l
    | `Unknown       -> Fmt.pf ppf "?"

  let pp ppf t =
    match t.id with
    | `Unknown       -> Fmt.pf ppf "? %s/%s" t.user t.repo
    | `PR n          -> Fmt.pf ppf "PR %s/%s#%d" t.user t.repo n
    | `Status (s, l) ->
      Fmt.pf ppf "status %s/%s:%s:%a"
        t.user t.repo s Fmt.(list ~sep:(unit "/") string) l

  let compare = Pervasives.compare

  module Set = Set(struct
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)

  (** PR diffs *)

  let remove_pr t (r, id) =
    let keep pr =
      r.Repo.user <> pr.PR.user ||
      r.Repo.repo <> pr.PR.repo ||
      id          <>  pr.PR.number
    in
    { t with Snapshot.prs = PR.Set.filter keep t.Snapshot.prs }

  let replace_pr t pr =
    let id = PR.repo pr, pr.PR.number in
    let t = remove_pr t id in
    let repos = Repo.Set.add (PR.repo pr) t.Snapshot.repos in
    let prs   = PR.Set.add pr t.Snapshot.prs in
    { t with Snapshot.repos; prs }

  let path_of_diff = function
    | `Added f | `Removed f | `Updated f -> Datakit_path.unwrap f

  (** Status diffs *)

  let remove_status t (r, s, l) =
    let keep x =
      r.Repo.user <> x.Status.user ||
      r.Repo.repo <> x.Status.repo ||
      s           <> x.Status.commit ||
      l           <> x.Status.context
    in
    { t with Snapshot.status = Status.Set.filter keep t.Snapshot.status }

  let replace_status t s =
    let cc = Status.repo s, s.Status.commit, s.Status.context in
    let t = remove_status t cc in
    let repos  = Repo.Set.add (Status.repo s) t.Snapshot.repos in
    let status = Status.Set.add s t.Snapshot.status in
    { t with Snapshot.repos; status }

  (** Repositories diff *)

  let repos diff =
    List.fold_left (fun acc d ->
        match path_of_diff d with
        | user :: repo :: _ -> Repo.Set.add { Repo.user; repo } acc
        | _ -> acc
      ) Repo.Set.empty diff

  let changes diff =
    List.fold_left (fun acc d ->
        let t = match path_of_diff d with
          | user :: repo :: "pr" :: id :: _ ->
            Some { user; repo; id = `PR (int_of_string id) }
          | user :: repo :: "commit" :: id :: "status" :: (_ :: _ :: _ as tl) ->
            let context = List.rev (List.tl (List.rev tl)) in
            Some { user; repo; id = `Status (id, context) }
          | user :: repo :: _ -> Some { user; repo; id = `Unknown }
          | _ -> None
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

  let error fmt = Fmt.kstrf (fun str -> DK.error "conv: %s" str) fmt

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

  let repos_of_commit c = repos (tree_of_commit c)

  (* PRs *)

  let update_pr t pr =
    let dir =
      empty / pr.PR.user / pr.PR.repo / "pr" / string_of_int pr.PR.number
    in
    Log.debug (fun l -> l "update_pr %s" @@ Datakit_path.to_hum dir);
    match pr.PR.state with
    | `Closed ->
      DK.Transaction.exists t dir >>*= fun exists ->
      if exists then DK.Transaction.remove t dir else ok ()
    | `Open   ->
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let head = Cstruct.of_string (pr.PR.head ^ "\n")in
      let state = Cstruct.of_string (PR.string_of_state pr.PR.state ^ "\n") in
      let title = Cstruct.of_string (pr.PR.title ^ "\n") in
      DK.Transaction.create_or_replace_file t ~dir "head" head >>*= fun () ->
      DK.Transaction.create_or_replace_file t ~dir "state" state >>*= fun () ->
      DK.Transaction.create_or_replace_file t ~dir "title" title

  let update_prs tr prs = list_iter_p (update_pr tr) (PR.Set.elements prs)

  let pr (E ((module Tree), t)) ~user ~repo number =
    let dir = empty / user / repo / "pr" / string_of_int number in
    Log.debug (fun l -> l "read_pr %s" @@ Datakit_path.to_hum dir);
    Tree.exists_file t (dir / "head")  >>*= fun exists_head ->
    Tree.exists_file t (dir / "state") >>*= fun exists_state ->
    Tree.exists_file t (dir / "title") >>*= fun exists_title ->
    if not exists_head then
      Log.debug (fun l -> l "error: pr/%d/head does not exist" number);
    if not exists_state then
      Log.debug (fun l -> l "error: pr/%d/state does not exist" number);
    if not exists_head || not exists_state then ok None
    else (
      Tree.read_file t (dir / "head") >>*= fun head ->
      Tree.read_file t (dir / "state") >>*= fun state ->
      (if not exists_title then ok (Cstruct.of_string "")
       else Tree.read_file t (dir / "title"))
      >>*= fun title ->
      let parse s = String.trim (Cstruct.to_string s) in
      let state = parse state in
      let head = parse head in
      let title = parse title in
      match PR.state_of_string state with
      | None       -> error "%s is not a valid PR state" state
      | Some state ->
        ok (Some { PR.user = user; repo; number; state; head; title })
    )

  let prs_of_repo tree { Repo.user; repo } =
    Log.debug (fun l -> l "prs_of_repo %s/%s" user repo);
    let E ((module Tree), t) = tree in
    let dir = empty / user / repo / "pr"  in
    Log.debug (fun l -> l "read_prs %s" @@ Datakit_path.to_hum dir);
    Tree.exists_dir t dir >>*= fun exists ->
    if not exists then ok PR.Set.empty
    else
      Tree.read_dir t dir >>*=
      list_map_p ~pool:pool9p
        (fun num -> pr ~user ~repo tree (int_of_string num))
      >>*= function l ->
      List.fold_left
        (fun acc pr -> match pr with None -> acc | Some x -> PR.Set.add x acc)
        PR.Set.empty l
      |> ok

  let prs ?repos:rs tree =
    Log.debug (fun l -> l "prs");
    (match rs with None -> repos tree | Some rs -> ok rs) >>*= fun repos ->
    list_map_p ~pool:pool9p (prs_of_repo tree) (Repo.Set.elements repos)
    >>*= fun prs ->
    ok (List.fold_left PR.Set.union PR.Set.empty prs)

  (* Status *)

  let update_status t s =
    let dir =
      empty / s.Status.user / s.Status.repo / "commit" / s.Status.commit
      / "status" /@ Datakit_path.of_steps_exn (Status.path s)
    in
    Log.debug (fun l -> l "update_status %s" @@ Datakit_path.to_hum dir);
    DK.Transaction.make_dirs t dir >>*= fun () ->
    let kvs = [
      "description", s.Status.description;
      "state"      , Some (Status_state.to_string s.Status.state);
      "target_url" , s.Status.url;
    ] in
    list_iter_p (fun (k, v) -> match v with
        | None   ->
          DK.Transaction.exists_file t (dir / k) >>*= fun exists ->
          if not exists then ok () else DK.Transaction.remove t (dir / k)
        | Some v ->
          let v = Cstruct.of_string (v ^ "\n") in
          DK.Transaction.create_or_replace_file t ~dir k v
      ) kvs

  let update_statuses tr s =
    list_iter_p (update_status tr) (Status.Set.elements s)

  let status (E ((module Tree), t)) ~user ~repo ~commit ~context =
    let context = Datakit_path.of_steps_exn context in
    let dir = empty / user / repo / "commit" / commit / "status" /@ context in
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
        Some { Status.user; repo; state; commit; context; description; url }
        |> ok

  let statuses_of_repo tree { Repo.user; repo } =
    Log.debug (fun l -> l "status_of_repo");
    let E ((module Tree), t) = tree in
    let dir = empty / user / repo / "commit" in
    Tree.exists_dir t dir >>*= fun exists ->
    if not exists then ok Status.Set.empty
    else
      Tree.read_dir t dir >>*=
      list_map_s (fun commit ->
          Log.debug (fun l ->
              l "status_of_repo %a %s" Datakit_path.pp dir commit);
          let dir = dir / commit / "status" in
          let rec aux context =
            let ctx = match Datakit_path.of_steps context with
              | Ok x    -> ok x
              | Error e -> error "%s" e
            in
            ctx >>*= fun ctx ->
            let dir = dir /@ ctx in
            Tree.exists_dir t dir >>*= fun exists ->
            if not exists then ok Status.Set.empty
            else
              Tree.read_dir t dir >>*= fun child ->
              list_map_p ~pool:pool9p
                (fun c -> aux (context @ [c])) child
              >>*= fun child ->
              let child = List.fold_left Status.Set.union Status.Set.empty child in
              Tree.exists_file t (dir / "state") >>*= fun exists ->
              if exists then
                status ~user ~repo tree ~commit ~context >>*= function
                | None   -> ok child
                | Some s -> ok (Status.Set.add s child)
              else
                ok child
          in
          aux []
        )
      >>*= fun status ->
      ok (List.fold_left Status.Set.union Status.Set.empty status)

  let statuses ?repos:rs tree =
    Log.debug (fun l -> l "status");
    (match rs with None -> repos tree | Some rs -> ok rs) >>*= fun repos ->
    list_map_p ~pool:pool9p (statuses_of_repo tree) (Repo.Set.elements repos)
    >>*= fun ss ->
    ok (List.fold_left Status.Set.union Status.Set.empty ss)

  (* Diffs *)

  let diff (E ((module Tree), _)) c = Tree.diff c

  let apply_pr_diff t tree (r, id as s)  =
    pr ~user:r.Repo.user ~repo:r.Repo.repo tree id >>*= function
    | None    -> Diff.remove_pr t s |> ok
    | Some pr -> Diff.replace_pr t pr |> ok

  let apply_status_diff t tree (r, commit, context as s)=
    status tree ~user:r.Repo.user ~repo:r.Repo.repo ~commit ~context
    >>*= function
    | None   -> Diff.remove_status t s |> ok
    | Some s -> Diff.replace_status t s |> ok

  let apply t (tree, diff) =
    let t = ref t in
    list_iter_s (fun { Diff.repo; user; id } ->
        Log.debug (fun l -> l "apply %s/%s %a" repo user Diff.pp_id id);
        let repo = { Repo.repo; user } in
        match id with
        | `PR pr ->
          apply_pr_diff !t tree (repo, pr) >>*= fun x ->
          t := x;
          ok ()
        | `Status (commit, context) ->
          apply_status_diff !t tree (repo, commit, context) >>*= fun x ->
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
      prs ~repos t >>*= fun prs ->
      statuses ~repos t >>*= fun status ->
      ok { Snapshot.repos; status; prs }

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
    Fmt.pf ppf "@[%a: %a@]" DK.Commit.pp t.head Snapshot.pp t.snapshot

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

  let pp ppf t =
    Fmt.pf ppf "@[[pub: %a@, priv: %a@]@]" pp_branch t.pub pp_branch t.priv

  let with_head branch fn =
    DK.Branch.head branch >>*= function
    | None   -> error "empty branch!"
    | Some c -> fn c

  let tr_head tr =
    DK.Transaction.parents tr >>*= function
    | []  -> error "no parents!"
    | [p] -> ok p
    | _   -> error "too many parents!"

  let branch ?old b =
    DK.Branch.transaction b >>*= fun tr ->
    tr_head tr >>*= fun head ->
    Conv.snapshot ?old (Conv.tree_of_commit head) >>*= fun snapshot ->
    let name = DK.Branch.name b in
    ok { snapshot; tr; head; name }

  let state ~old ~pub ~priv =
    let mk b = (b.head, b.snapshot) in
    let pub_o  = match old with None -> None | Some o -> Some (mk o.pub)  in
    let priv_o = match old with None -> None | Some o -> Some (mk o.priv) in
    branch ?old:pub_o pub   >>*= fun pub ->
    branch ?old:priv_o priv >>*= fun priv ->
    ok { pub; priv }

  (** Import from GitHub *)

  (* Import http://github.com/usr/repo state. *)
  let import_repos t ~token repos =
    Log.debug (fun l -> l "import_repo %a" Repo.Set.pp repos);
    let repos = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        let { Repo.user; repo } = r in
        Log.debug (fun l -> l "API.prs %a" Repo.pp r);
        API.prs token ~user ~repo
      ) repos
    >>= fun prs ->
    let prs = List.flatten prs in
    Lwt_list.map_p (fun pr ->
        let { PR.user; repo; _ } = pr in
        if pr.PR.state = `Closed then Lwt.return_none
        else (
          Log.debug (fun l -> l "API.status %s/%s:%s" user repo pr.PR.head);
          API.status token ~user ~repo ~commit:pr.PR.head >|= fun s ->
          Some (pr, s)
        )
      ) prs
    >>= fun ss ->
    let t = List.fold_left Snapshot.add_repo t repos in
    List.fold_left (fun acc -> function
        | None         -> acc
        | Some (pr, s) ->
          let acc = Snapshot.add_pr acc pr in
          let acc = Snapshot.add_status acc  (Status.Set.of_list s) in
          acc
      ) t ss
    |> ok

  (** Prune *)

  (* [prune t] is [t] with all the closed PRs pruned as well as a
     cleanup function which can be used to cleanup an existing
     file-system projection of [t]. *)
  let prune t =
    Log.debug (fun l -> l "prune");
    let status = Status.Set.index t.Snapshot.status Status.repo in
    let prs = PR.Set.index t.Snapshot.prs PR.repo in
    let aux repo =
      let status = try Hashtbl.find status repo with Not_found -> [] in
      let prs = try Hashtbl.find prs repo with Not_found -> [] in
      Log.debug (fun l -> l "prune %a" Repo.pp repo);
      Log.debug (fun l ->
          l "status:@ %a@ prs:@ %a"
            Fmt.(Dump.list Status.pp) status Fmt.(Dump.list PR.pp) prs
        );
      (* 1. Prune closed PRs. *)
      let open_prs, closed_prs =
        List.fold_left (fun (open_prs, closed_prs) pr ->
            match pr.PR.state with
            | `Open   -> PR.Set.add pr open_prs, closed_prs
            | `Closed -> open_prs, PR.Set.add pr closed_prs
          ) (PR.Set.empty, PR.Set.empty) prs
      in
      let is_open s =
        PR.Set.exists (fun pr ->
            pr.PR.head = s.Status.commit && pr.PR.state = `Open
          ) open_prs
      in
      Log.debug (fun l -> l "open_prs:%a" PR.Set.pp open_prs);
      Log.debug (fun l -> l "closed_prs:%a" PR.Set.pp closed_prs);
      (* 2. Prune commits which doesn't belong to an open PR. *)
      let open_status, closed_status =
        List.fold_left (fun (open_status, closed_status) s ->
            match is_open s with
            | false ->
              open_status, Status.Set.add s closed_status
            | true  ->
              Status.Set.add s open_status, closed_status
          ) (Status.Set.empty, Status.Set.empty) status
      in
      Log.debug (fun l -> l "open_status:%a" Status.Set.pp open_status);
      Log.debug (fun l -> l "closed_status:%a" Status.Set.pp closed_status);
      let t = {
        Snapshot.repos = Repo.Set.singleton repo ;
        status         = open_status;
        prs            = open_prs
      } in
      let cleanup =
        if PR.Set.is_empty closed_prs && Status.Set.is_empty closed_status then
          None
        else
          let root = Datakit_path.empty / repo.Repo.user / repo.Repo.repo in
          let f tr =
            list_iter_p (fun pr ->
                let dir = root / "pr" / string_of_int pr.PR.number in
                DK.Transaction.remove tr dir
              ) (PR.Set.elements closed_prs)
            >>*= fun () ->
            list_iter_p (fun s ->
                DK.Transaction.remove tr (root / "commit" / s.Status.commit)
              ) (Status.Set.elements closed_status)
          in
          Some f
      in
      ok (t, cleanup)
    in
    (* status cannot be removed, so simply monitor updates in
       [new_status]. *)
    let result = ref Snapshot.empty in
    let cleanup = ref None in
    list_iter_s (fun r ->
        aux r >>*= fun (x, c) ->
        result := Snapshot.union !result x;
        let () = match c, !cleanup with
          | None  , None   -> ()
          | None  , Some c
          | Some c, None   -> cleanup := Some c
          | Some x, Some y -> cleanup := Some (fun t -> x t >>*= fun () -> y t)
        in
        ok ()
      ) (Repo.Set.elements t.Snapshot.repos)
    >>*= fun () ->
    ok (!result, !cleanup)

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
          (fun l -> l "API.set-status %a (was %a)"
              Status.pp s Fmt.(option Status.pp) old);
        if not dry_updates then API.set_status token s else  Lwt.return_unit
      ) (Status.Set.elements status)
    >>= fun () ->
    Lwt_list.iter_p (fun pr ->
        Log.info (fun l -> l "API.set-pr %a" PR.pp pr);
        if not dry_updates then API.set_pr token pr else Lwt.return_unit
      ) (PR.Set.elements prs)
    >>= fun () ->
    ok ()

  (** Merge *)

  (* Merge the private branch back in the public branch. *)
  let merge t =
    Log.debug (fun l ->
        l "merge@,@[pub:%a@,priv:%a@]" pp_branch t.pub pp_branch t.priv
      );
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
         list_iter_p (fun path ->
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
         ok @@ Fmt.strf "\n\nconflicts:@,@[%a@]"
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
          Fmt.strf "Merging with %s\n\n%a%s" t.priv.name pp diff conflict_msg
        in
        Log.debug (fun l -> l "merge commit: %s" msg);
        DK.Transaction.commit t.pub.tr ~message:msg

  (* same as [merge], but check some useful invariants too. *)
  let merge t =
    assert (DK.Transaction.closed t.priv.tr);
    assert (not (DK.Transaction.closed t.pub.tr));
    merge t >>*= fun () ->
    assert (DK.Transaction.closed t.priv.tr);
    assert (DK.Transaction.closed t.pub.tr);
    ok ()

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
    DK.Transaction.abort t.priv.tr >>= fun () ->
    DK.Transaction.abort t.pub.tr

  (* On startup, build the initial state by looking at the active
     repository in the public and private branch. Import the new
     repositories in the private branch, then merge it in the public
     branch. Finally call the GitHub API with the diff between the
     public and the private branch. *)
  let first_sync ~token ~dry_updates ~pub ~priv =
    with_head pub (fun pub_c ->
        DK.Branch.with_transaction priv (fun priv_tr ->
            tr_head priv_tr >>*= fun priv_c ->
            Log.debug (fun l ->
                l "first_sync priv=%a pub=%a"
                  DK.Commit.pp priv_c DK.Commit.pp pub_c);
            Conv.repos_of_commit priv_c >>*= fun priv_r ->
            Conv.repos_of_commit pub_c  >>*= fun pub_r  ->
            let repos = Repo.Set.union priv_r pub_r in
            if Repo.Set.is_empty repos then DK.Transaction.abort priv_tr >>= ok
            else
              import_repos ~token Snapshot.empty repos >>*= fun t ->
              Conv.update_prs priv_tr t.Snapshot.prs >>*= fun () ->
              Conv.update_statuses priv_tr t.Snapshot.status >>*= fun () ->
              Conv.diff Conv.(tree_of_transaction priv_tr) priv_c >>*= fun d ->
              if Diff.Set.is_empty d then DK.Transaction.abort priv_tr >>= ok
              else
                let message = Fmt.strf "Resync for %a" Repo.Set.pp repos in
                DK.Transaction.commit priv_tr ~message))
    >>*= fun () ->
    state ~old:None ~pub ~priv >>*= fun t ->
    DK.Transaction.abort t.priv.tr >>= fun () ->
    Log.debug (fun l -> l "first_sync: initial state %a" pp t);
    merge t >>*= fun () ->
    state ~old:(Some t) ~pub ~priv >>*= fun t ->
    Log.debug (fun l -> l "first_sync: after merge %a" pp t);
    call_github_api ~token ~dry_updates ~old:t.priv.snapshot t.pub.snapshot
    >>*= fun () ->
    abort t >>= fun () ->
    ok t

  let repos old t =
    let old = Snapshot.repos old.snapshot in
    let t   = Snapshot.repos t.snapshot in
    Repo.Set.(union (diff old t) (diff t old))

  (* The main synchonisation function: it is called on every change in
     the public or private branch. *)
  let sync_once ~dry_updates ~token ~pub ~priv ~old t =
    Log.debug (fun l -> l "sync_once:@,@[old:%a@,new:%a@]" pp old pp t);
    (* Start by calling GitHub API calls that the user requested. *)
    call_github_api ~dry_updates ~token ~old:old.pub.snapshot t.pub.snapshot
    >>*= fun () ->
    let repos = Repo.Set.union (repos old.pub t.pub) (repos old.priv t.priv) in
    let merge t =
      prune t.priv.snapshot >>*= fun (_, cleanups) ->
      match cleanups with
      | None ->
        Log.debug (fun l -> l "nothing to prune");
        (* nothing to prune *)
        DK.Transaction.abort t.priv.tr >>= fun () ->
        merge t
      | Some fn ->
        (* the private branch needs some cleanup *)
        fn t.priv.tr >>*= fun () ->
        let message = Fmt.strf "Pruning %s" t.priv.name in
        DK.Transaction.abort t.pub.tr >>= fun () ->
        DK.Transaction.commit t.priv.tr ~message
    in
    if Repo.Set.is_empty repos then merge t >>*= fun () -> ok t
    else
      (* we have new repositories to watch! import them in the private
         branch. *)
      import_repos t.priv.snapshot ~token repos >>*= fun s ->
      Conv.update_prs t.priv.tr s.Snapshot.prs >>*= fun () ->
      Conv.update_statuses t.priv.tr s.Snapshot.status >>*= fun () ->
      DK.Transaction.diff t.priv.tr t.priv.head >>*= fun diff ->
      DK.Transaction.abort t.pub.tr >>= fun () ->
      if diff = [] then DK.Transaction.abort t.priv.tr >>= fun () -> ok t
      else
        let message = Fmt.strf "Import %a from GitHub" Repo.Set.pp repos in
        DK.Transaction.commit t.priv.tr ~message >>*= fun () ->
        state ~old:(Some t) ~pub ~priv >>*= fun t ->
        merge t >>*= fun () ->
        ok t

  let sync_once ~dry_updates ~token ~pub ~priv old  =
    assert (DK.Transaction.closed old.priv.tr);
    assert (DK.Transaction.closed old.pub.tr);
    state ~old:(Some old) ~pub ~priv >>*= fun t ->
    sync_once ~dry_updates ~token ~pub ~priv ~old t >>*= fun t ->
    assert (DK.Transaction.closed t.priv.tr);
    assert (DK.Transaction.closed t.pub.tr);
    ok t

  type t = State of state | Starting

  let empty = Starting

  let continue = function
    | Some s -> Lwt_switch.is_on s
    | None   -> true

  let run ?switch ~dry_updates ~token ~priv ~pub t policy =
    let sync_once = function
      | Starting -> first_sync ~dry_updates ~token ~priv ~pub
      | State t  -> sync_once ~dry_updates ~token ~priv ~pub t
    in
    match policy with
    | `Once   -> sync_once t >>*= fun t -> ok (`Finish (State t))
    | `Repeat ->
      let t = ref t in
      let updates = ref false in
      let cond = Lwt_condition.create () in
      let rec react () =
        if not (continue switch) then Lwt.return_unit
        else
          (if not !updates then Lwt_condition.wait cond else Lwt.return_unit)
          >>= fun () ->
          updates := false;
          sync_once !t >>= function
          | Ok s    -> t := State s; react ()
          | Error e ->
            Log.err (fun l -> l "sync error: %a" DK.pp_error e);
            react ()
      in
      let watch br =
        let notify _ =
          updates := true;
          Lwt_condition.signal cond ();
          ok `Again
        in
        DK.Branch.wait_for_head ?switch br notify >>= function
        | Ok _    -> Lwt.return_unit
        | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
      in
      Lwt.join [ react () ; watch priv; watch pub ] >>= fun () ->
      ok (`Finish !t)

  let sync ?switch ?(policy=`Repeat) ?(dry_updates=false) ~pub ~priv ~token t =
    Log.debug (fun l ->
        l "sync pub:%s priv:%s" (DK.Branch.name pub) (DK.Branch.name priv)
      );
    (init_sync ~priv ~pub >>*= fun () ->
     run ?switch ~dry_updates ~token ~priv ~pub t policy >>*= function
     | `Finish l -> ok l
     | _ -> failwith "TODO")
    >>= function
    | Ok t    -> Lwt.return t
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e

end
