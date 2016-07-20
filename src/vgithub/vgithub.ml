open Result
open Astring

let src = Logs.Src.create "vgithub" ~doc:"Virtual Github API"
module Log = (val Logs.src_log src : Logs.LOG)

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

  type t = {
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
    Fmt.pf ppf "[number: %d, state: %a, head: %s, title: %s]"
      t.number pp_state t.state t.head t.title

end

module Status = struct

  type t = {
    context: string option;
    url: string option;
    description: string option;
    state: Status_state.t;
    commit: string;
  }

  let compare = Pervasives.compare

  let pp ppf t =
    let pp_opt k ppf v = match v with
      | None   -> ()
      | Some v -> Fmt.pf ppf "%s: %s, " k v
    in
    Fmt.pf ppf "[commit:%s, %a%a%a state: %a]"
      t.commit
      (pp_opt "context") t.context
      (pp_opt "url") t.url
      (pp_opt "description") t.description
      Status_state.pp t.state

  let path t = match t.context with
    | None   -> ["default"]
    | Some c -> String.cuts ~empty:false ~sep:"/" c

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

end

module type API = sig
  type token
  val user_exists: token -> user:string -> bool Lwt.t
  val repo_exists: token -> user:string -> repo:string -> bool Lwt.t
  val repos: token -> user:string -> string list Lwt.t
  val status: token -> user:string -> repo:string -> commit:string ->
    Status.t list Lwt.t
  val set_status: token -> user:string -> repo:string -> Status.t -> unit Lwt.t
  val set_pr: token -> user:string -> repo:string -> PR.t -> unit Lwt.t
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
      API.set_status t.token ~user:t.user ~repo:t.repo new_status;
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
        Status.context = Some name;
        url = None;
        description = None;
        state = `Pending;
        commit;
      } in
      API.set_status t.token ~user:t.user ~repo:t.repo new_status >>= fun () ->
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

module Sync (API: API) (DK: Datakit_S.CLIENT) = struct

  open Lwt.Infix
  open Datakit_path.Infix

  let ( >>*= ) x f =
    x >>= function
    | Ok x -> f x
    | Error _ as e -> Lwt.return e

  let ok x = Lwt.return (Ok x)
  let error fmt = Fmt.kstrf (fun str -> DK.error "%s" str) fmt

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

  let list_map_p f l =
    Lwt_list.map_p f l >|= fun l ->
    List.fold_left (fun acc x -> match acc, x with
        | Ok acc, Ok x            -> Ok (x :: acc)
        | Error e, _ | _, Error e -> Error e
      ) (Ok []) (List.rev l)

  module type TREE = Datakit_S.READABLE_TREE with
    type 'a or_error := 'a DK.or_error

  type tree = E: (module TREE with type t = 'a) * 'a -> tree

  module Conv = struct

    (* conversion between GitHub and DataKit states. *)

    (* PRs *)

    let update_pr ~root t pr =
      let dir = root / "pr" / string_of_int pr.PR.number in
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

    let read_pr ~root (E ((module Tree), t)) number =
      let dir = root / "pr" / string_of_int number in
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
        | Some state -> ok (Some { PR.number; state; head; title })
      )

    let read_prs ~root tree =
      let E ((module Tree), t) = tree in
      let dir = root / "pr"  in
      Log.debug (fun l -> l "read_prs %s" @@ Datakit_path.to_hum dir);
      Tree.exists_dir t dir >>*= fun exists ->
      if not exists then ok []
      else
        Tree.read_dir t dir >>*=
        list_map_p (fun num -> read_pr ~root tree (int_of_string num))
        >>*= fun l ->
        List.fold_left
          (fun acc pr -> match pr with None -> acc | Some x -> x :: acc)
          [] (List.rev l)
        |> ok

    (* Status *)

    let update_status ~root t s =
      let dir =
        root / "commit" / s.Status.commit / "status" /@
        Datakit_path.of_steps_exn (Status.path s)
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

    let read_status ~root (E ((module Tree), t)) ~commit ~context =
      let context_path = Datakit_path.of_steps_exn context in
      let dir = root / "commit" / commit / "status" /@ context_path in
      Log.debug (fun l -> l "read_status %a" Datakit_path.pp dir);
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
        let context = Some (Datakit_path.to_hum context_path) in
        ok { Status.state; commit; context; description; url; }

    let read_statuses ~root tree =
      let E ((module Tree), t) = tree in
      Log.debug (fun l -> l "read_statuses");
      let dir = root / "commit" in
      Log.debug (fun l -> l "read_statuses %a" Datakit_path.pp dir);
      Tree.exists_dir t dir >>*= fun exists ->
      if not exists then ok []
      else
        Tree.read_dir t dir >>*=
        list_map_p (fun commit ->
            let dir = dir / commit / "status" in
            let rec aux context =
              Log.debug
                (fun l -> l "read_status context=%a" Fmt.(Dump.list string) context);
              let dir = dir /@ Datakit_path.of_steps_exn context in
              Tree.exists_dir t dir >>*= fun exists ->
              if not exists then ok []
              else
                Tree.read_dir t dir >>*= fun child ->
                list_map_p (fun c -> aux (context @ [c])) child >>*= fun child ->
                let child = List.flatten child in
                Tree.exists_file t (dir / "state") >>*= fun exists ->
                if exists then
                  read_status ~root tree ~commit ~context >>*= fun s ->
                  ok (s :: child)
                else
                  ok child
            in
            aux []
          )
        >>*= fun status ->
        ok (List.flatten status)

  end

  type 'a user_repo = {
    user: string;
    repo: string;
    data: 'a;
  }

  let pp_user_repo pp_a ppf t =
    Fmt.pf ppf "@[%s/%s: %a@]" t.user t.repo pp_a t.data

  let compare_user_repo cmp x y =
    match compare (x.user, x.repo) (y.user, y.repo) with
    | 0 -> cmp x.data y.data
    | i -> i

  module type ELT = sig
    include Set.OrderedType
    val pp: t Fmt.t
  end

  module UserRepoSet (E: ELT) = struct
    include Set.Make(struct
        type t = E.t user_repo
        let compare = compare_user_repo E.compare
      end)
    let sdiff x y = union (diff x y) (diff y x)
    let pp ppf t = Fmt.(Dump.list @@ pp_user_repo E.pp) ppf (elements t)

    let bindings t =
      let tbl = Hashtbl.create (cardinal t) in
      iter (fun { user; repo; data } ->
          let v =
            try Hashtbl.find tbl (user, repo)
            with Not_found -> []
          in
          Hashtbl.replace tbl (user, repo) (data :: v)
        ) t;
      tbl

  end

  module XStatusSet = UserRepoSet(Status)
  module XPRSet = UserRepoSet(PR)
  module XRepoSet = UserRepoSet(struct
      type t = unit
      let compare = compare
      let pp ppf () = Fmt.string ppf ""
    end)

  type snapshot = {
    repos : XRepoSet.t;
    status: XStatusSet.t;
    prs   : XPRSet.t;
  }

  let compare_snapshot x y =
    match XRepoSet.compare x.repos y.repos with
    | 0 ->
      begin match XStatusSet.compare x.status y.status with
        | 0 -> XPRSet.compare x.prs y.prs
        | i -> i
      end
    | i -> i

  let pp_snapshot ppf t =
    Fmt.pf ppf "@[repos: %a@, status: %a@, prs: %a@]"
      XRepoSet.pp t.repos XStatusSet.pp t.status XPRSet.pp t.prs

  let empty_snapshot =
    { repos = XRepoSet.empty; status = XStatusSet.empty; prs = XPRSet.empty }

  type t = {
    pub: snapshot; priv: snapshot;
    merged: bool;     (* becomes true after the first merge of priv into pub. *)
  }

  let pp ppf t =
    Fmt.pf ppf "@[pub: %a@, priv: %a@, merged: %b]"
      pp_snapshot t.pub pp_snapshot t.priv t.merged

  let empty = { pub = empty_snapshot; priv = empty_snapshot; merged = false }

  let of_tree (E ((module Tree), tree)) =
    Log.debug (fun l -> l "of_tree");
    let root = Datakit_path.empty in
    Tree.exists_dir tree root >>*= fun is_dir ->
    if not is_dir then ok empty_snapshot
    else
    Tree.read_dir tree root >>*= fun users ->
    List.fold_left (fun acc user ->
        Tree.exists_dir tree (root / user) >>*= fun is_dir ->
        if not is_dir then acc
        else
          Tree.read_dir tree (root / user) >>*= fun repos ->
          List.fold_left (fun acc repo ->
              acc >>*= fun acc ->
              let tree = E ((module Tree), tree) in
              let root = root / user / repo in
              Conv.read_statuses ~root tree >>*= fun status ->
              let status =
                status
                |> List.map (fun data -> { user; repo; data })
                |> XStatusSet.of_list
                |> XStatusSet.union acc.status
              in
              Conv.read_prs ~root tree >>*= fun prs ->
              let prs =
                List.map (fun data -> { user; repo; data }) prs
                |> XPRSet.of_list
                |> XPRSet.union acc.prs
              in
              let repos = XRepoSet.add { user; repo; data = () } acc.repos in
              ok { repos; status; prs }
            ) acc repos
      ) (ok @@ empty_snapshot) users

  (* compute all the active hooks for a given DataKit commit *)
  let of_commit c =
    Log.debug (fun l -> l "of_commit %s" @@ DK.Commit.id c);
    of_tree (E ((module DK.Tree), DK.Commit.tree c))

  (* Read events from the GitHub API and overwrite DataKit state with
     them, in chronological order. *)
  let sync_datakit token ~user ~repo tr =
    Log.debug (fun l -> l "sync_datakit %s/%s" user repo);
    let root = Datakit_path.empty / user / repo in
    API.events token ~user ~repo >>= fun events ->
    list_iter_s (function
        | Event.PR pr    -> Conv.update_pr ~root tr pr
        | Event.Status s -> Conv.update_status ~root tr s
        | _               -> ok ()
      ) events >>*= fun () ->
    (* NOTE: it seems that GitHub doesn't store status events so we
       need to do load them ourself ... *)
    DK.Transaction.exists_dir tr (root / "commit") >>*= fun exists_c ->
    (if not exists_c then ok () else DK.Transaction.remove tr (root / "commit"))
    >>*= fun () ->
    let tree = E ((module DK.Transaction), tr) in
    Conv.read_prs ~root tree >>*= fun prs ->
    list_iter_p (fun pr ->
        if pr.PR.state = `Closed then ok ()
        else (
          API.status token ~user ~repo ~commit:pr.PR.head >>= fun s ->
          list_iter_p (Conv.update_status ~root tr) s
        )
      ) prs

  (* Read the GitHub events for the repositories appearing in [diff]
     and populate [branch] with the result of applying all of the into
     the state. *)
  (* NOTE: quite slow (because of the call to API.events), so use it
     with care *)
  let import_github_events ~token branch diff =
    Log.debug (fun l -> l "import_github_events %a" XRepoSet.pp diff);
    if XRepoSet.is_empty diff then ok ()
    else DK.Branch.with_transaction branch (fun tr ->
        list_iter_p (fun { user; repo; _ } ->
            sync_datakit token ~user ~repo tr >>= function
            | Ok ()   -> ok ()
            | Error e ->
              error "Error while syncing %s/%s: %a" user repo DK.pp_error e
          ) (XRepoSet.elements diff)
        >>= function
        | Ok () ->
          let message = Fmt.strf "Syncing with events %a" XRepoSet.pp diff in
          DK.Transaction.commit tr ~message
        | Error e ->
          DK.Transaction.abort tr >>= fun () ->
          error "%a" DK.pp_error e
      )

  (* Read DataKit data and call the GitHub API to sync the world with
     what DataKit think it should be.
     Also clean-up DataKit invariants such as GC-ing commit entries. *)
  (* TODO: handle pr_diffs too *)
  let call_github_api ~dry_updates ~token ~old t =
    let status = XStatusSet.diff t.status old.status |> XStatusSet.elements in
    let prs = XPRSet.diff t.prs old.prs |> XPRSet.elements in
    Lwt_list.iter_p (fun { user; repo; data } ->
        Log.debug (fun l -> l "call-github-api: status %s/%s" user repo);
        if not dry_updates then API.set_status token ~user ~repo data
        else (
          Log.app
            (fun l -> l "API.set-status %s/%s %a" user repo Status.pp data);
          Lwt.return_unit)
      ) status
    >>= fun () ->
    Lwt_list.iter_p (fun { user; repo; data } ->
        Log.debug (fun l -> l "call-github-api: pr %s/%s" user repo);
        if not dry_updates then API.set_pr token ~user ~repo data
        else (
          Log.app (fun l -> l "API.set-pr %s/%s %a" user repo PR.pp data);
          Lwt.return_unit)
      ) prs
    >>= ok

  let prune t branch =
    let status = XStatusSet.bindings t.status in
    let prs = XPRSet.bindings t.prs in
    let aux { user; repo; _ } =
      let status = try Hashtbl.find status (user, repo) with Not_found -> [] in
      let prs = try Hashtbl.find prs (user, repo) with Not_found -> [] in
      Log.debug (fun l -> l "prune user=%s repo=%s" user repo);
      let root = Datakit_path.empty / user / repo in
      Log.debug (fun l ->
          l "status:@ %a@ prs:@ %a"
            Fmt.(Dump.list Status.pp) status Fmt.(Dump.list PR.pp) prs
        );
      (* 1. Prune closed PRs. *)
      let open_prs =
        List.fold_left (fun acc pr ->
            if pr.PR.state = `Open then String.Set.add pr.PR.head acc else acc
          ) String.Set.empty prs
      in
      Log.debug (fun l -> l "open_prs:%a" String.Set.dump open_prs);
      let is_open commit = String.Set.mem commit open_prs in
      let closed_prs = List.filter (fun pr -> pr.PR.state = `Closed) prs in
      Log.debug (fun l -> l "closed_prs:%a" Fmt.(Dump.list PR.pp) closed_prs);
      (* 2. Prune commits which doesn't belong to an open PR. *)
      let closed_commits =
        List.fold_left (fun acc s ->
            if is_open s.Status.commit then acc
            else String.Set.add s.Status.commit acc
          ) String.Set.empty status
      in
      Log.debug (fun l -> l "closed_commits:%a" String.Set.dump closed_commits);
      if String.Set.is_empty closed_commits && closed_prs = [] then ok ()
      else DK.Branch.with_transaction branch (fun tr ->
          list_iter_p (fun pr ->
              DK.Transaction.remove tr (root / "pr" / string_of_int pr.PR.number)
            ) closed_prs
          >>*= fun () ->
          list_iter_p (fun commit ->
              DK.Transaction.remove tr (root / "commit" / commit)
            ) (String.Set.elements closed_commits)
          >>= function
          | Ok ()   -> DK.Transaction.commit tr
                         ~message:"Pruning closed PRs and their commits."
          | Error e -> DK.Transaction.abort tr >|= fun () -> Error e
        )
    in
    (* status cannot be removed, so simply monitor updates in
       [new_status]. *)
    list_iter_p aux (XRepoSet.elements t.repos)

  let with_head branch fn =
    DK.Branch.head branch >>*= function
    | None   -> error "empty branch!"
    | Some c -> fn c

  let sync ?switch ?(policy=`Repeat) ?(dry_updates=false) ~pub ~priv ~token t =
    Log.debug (fun l ->
        l "sync pub:%s priv:%s" (DK.Branch.name pub) (DK.Branch.name priv)
      );
    let user_repo_diff old_t c =
      of_commit c >>*= fun current_t ->
      let diff = XRepoSet.sdiff old_t.repos current_t.repos in
      Log.debug (fun l -> l "user-repo-diff: %a" XRepoSet.pp diff);
      ok diff
    in
    let call_github_api old_t ~priv_c ~pub_c =
      of_commit pub_c >>*= fun pub_t ->
      if not t.merged then
        (* if [t.priv] is not yet merged into [t.pub], this means that
           [t.pub] might contain some outdated information. In that
           case, skip the user updates as it is unsafe to call GitHub
           API calls.  *)
        of_commit priv_c >>*= fun priv_t ->
        call_github_api ~dry_updates ~token ~old:priv_t pub_t
      else
        call_github_api ~dry_updates ~token ~old:old_t.pub pub_t
    in
    let prune current =
      of_commit current >>*= fun last_t ->
      prune last_t priv >>*= fun () ->
      ok ()
    in
    let merge old_t c =
      DK.Branch.with_transaction pub (fun tr ->
          DK.Transaction.merge tr c >>*= fun (_, conflicts) ->
          if conflicts <> [] then failwith "TODO";
          let msg = Fmt.strf "Merging with %s" @@ DK.Branch.name priv in
          of_commit c >>*= fun priv ->
          of_tree (E ((module DK.Transaction), tr)) >>*= fun pub ->
          (if compare_snapshot old_t.priv priv = 0 then
             DK.Transaction.abort tr >>= ok
           else
             DK.Transaction.commit tr ~message:msg)
          >>*= fun () ->
          ok { pub; priv; merged = true }
        )
    in
    let init () =
      Log.debug (fun l -> l "init");
      (DK.Branch.head priv >>*= function
        | Some _ -> ok ()
        | None   ->
          DK.Branch.with_transaction priv (fun tr ->
              let dir  = Datakit_path.empty in
              let data = Cstruct.of_string "### DataKit -- GitHub bridge" in
              DK.Transaction.create_or_replace_file tr ~dir "README.md" data
              >>= function
              | Ok ()   -> DK.Transaction.commit tr ~message:"Initial commit"
              | Error e ->
                DK.Transaction.abort tr >>= fun () ->
                Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
            ))
      >>*= fun () ->
      DK.Branch.head pub >>*= function
      | Some _ -> ok ()
      | None   -> with_head priv (DK.Branch.fast_forward pub)
    in
    let once t =
      Log.debug (fun l -> l "once %a" pp t);
      with_head priv (fun priv_c ->
          with_head pub (fun pub_c ->
              call_github_api t ~priv_c ~pub_c >>*= fun () ->
              user_repo_diff t.pub pub_c       >>*= fun pub_d ->
              user_repo_diff t.priv priv_c     >>*= fun priv_d ->
              import_github_events ~token priv (XRepoSet.union pub_d priv_d)
            ))
      >>*= fun () ->
      with_head priv prune     >>*= fun () ->
      with_head priv (merge t)
    in
    let run () =
      once t >>*= fun t ->
      match policy with
      | `Once   -> ok (`Finish t)
      | `Repeat ->
        let last = ref t in
        let cond = Lwt_condition.create () in
        let signal = function
          | None    -> ok `Again
          | Some _ -> Lwt_condition.signal cond (); ok `Again
        in
        let rec react () =
          Lwt_condition.wait cond >>= fun () ->
          once !last >>= function
          | Ok l    -> last := l; react ()
          | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
        in
        let watch br =
          DK.Branch.wait_for_head ?switch br signal >>= function
          | Ok _    -> Lwt.return_unit
          | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e
        in
        Lwt.join [
          watch priv;
          watch pub;
          react ();
        ] >>= fun () ->
        ok (`Finish !last)
    in
    (init () >>*= fun () ->
     run  () >>*= function
     | `Finish l -> ok l
     | _ -> failwith "TODO")
    >>= function
    | Ok t    -> Lwt.return t
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e

end
