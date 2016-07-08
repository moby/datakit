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

  let pp ppf t =
    Fmt.pf ppf "[number: %d, state: %a, head: %s]"
      t.number pp_state t.state t.head

  module Set = Set.Make(struct type r = t type t = r let compare = compare end)

end

module Status = struct

  type t = {
    context: string option;
    url: string option;
    description: string option;
    state: Status_state.t;
    commit: string;
  }

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

  module Set = struct
    include Set.Make(struct type r = t type t = r let compare = compare end)
    let of_list = List.fold_left (fun s e -> add e s) empty
    let pp ppf s = Fmt.(Dump.list pp) ppf (elements s)
  end

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
            set_status ();
            Vfs.ok (Status_state.to_string s ^ "\n");
          )
      ) in
    let descr = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_descr then Vfs.ok (str ^ "\n")
        else (
          current_descr := Some str;
          set_status ();
          Vfs.ok (str ^ "\n")
        )
      ) in
    let url = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_url then Vfs.ok (str ^ "\n")
        else (
          current_url := Some str;
          set_status ();
          Vfs.ok (str ^ "\n")
        )
      ) in
    let dir = [
      Vfs.Inode.file "state"  state;
      Vfs.Inode.file "descr"  descr;
      Vfs.Inode.file "url"    url;
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
      | None   -> Vfs.Dir.of_list childs
      | Some s -> commit_status_dir t ~extra_dirs:childs s
    in
    let ls () =
      Lazy.force !status
      |> List.map (fun s -> Status.path s, s)
      |> sort_by_hd
      |> List.map (fun (name, childs) -> Vfs.Inode.dir name @@ inodes childs)
      |> Vfs.ok
    in
    let lookup name =
      Log.debug (fun l -> l "lookup %s" name);
      try
        Lazy.force !status
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
      API.set_status t.token ~user:t.user ~repo:t.repo new_status;
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
  open Datakit_path.Infix

  let ( >>*= ) x f =
    x >>= function
    | Ok x -> f x
    | Error _ as e -> Lwt.return e

  let ok x = Lwt.return (Ok x)
  let error fmt = Fmt.kstrf (fun str -> DK.error "%s" str) fmt

  let list_iter f l =
    List.fold_left (fun acc x ->
        acc >>*= fun () ->
        f x
      ) (ok ()) l

  let list_map f l =
    List.fold_left (fun acc x ->
        acc >>*= fun acc ->
        f x >>*= fun y   ->
        ok (y :: acc)
      ) (ok []) (List.rev l)

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
        DK.Transaction.create_or_replace_file t ~dir "head" head >>*= fun () ->
        DK.Transaction.create_or_replace_file t ~dir "state" state

    let read_pr ~root t number =
      let dir = root / "pr" / string_of_int number in
      Log.debug (fun l -> l "read_pr %s" @@ Datakit_path.to_hum dir);
      DK.Tree.read_file t (dir / "head")  >>*= fun head ->
      DK.Tree.read_file t (dir / "state") >>*= fun state ->
      let parse s = String.trim (Cstruct.to_string s) in
      let state = parse state in
      let head = parse head in
      match PR.state_of_string state with
      | None       -> error "%s is not a valid PR state" state
      | Some state -> ok { PR.number; state; head }

    let read_prs ~root t =
      let dir = root / "pr"  in
      Log.debug (fun l -> l "read_prs %s" @@ Datakit_path.to_hum dir);
      DK.Tree.exists_dir t dir >>*= fun exists ->
      if not exists then ok []
      else
        DK.Tree.read_dir t dir >>*=
        list_map (fun num -> read_pr ~root t (int_of_string num))

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
      list_iter (fun (k, v) -> match v with
          | None   ->
            DK.Transaction.exists_file t (dir / k) >>*= fun exists ->
            if not exists then ok () else DK.Transaction.remove t (dir / k)
          | Some v ->
            let v = Cstruct.of_string (v ^ "\n") in
            DK.Transaction.create_or_replace_file t ~dir k v
        ) kvs

    let read_status ~root t ~commit ~context =
      let context_path = Datakit_path.of_steps_exn context in
      let dir = root / "commit" / commit / "status" /@ context_path in
      Log.debug (fun l -> l "read_status %a" Datakit_path.pp dir);
      DK.Tree.read_file t (dir / "state") >>*= fun state ->
      match Status_state.of_string (String.trim (Cstruct.to_string state)) with
      | None       -> error "%s: invalid state" @@ Cstruct.to_string state
      | Some state ->
        let read file =
          let some s = match String.trim s with "" -> None | s -> Some s in
          DK.Tree.exists_file t file >>*= function
          | false -> ok None
          | true  ->
            DK.Tree.read_file t file >>*= fun d ->
            ok (some @@ Cstruct.to_string d)
        in
        read (dir / "description") >>*= fun description ->
        read (dir / "target_url")  >>*= fun url ->
        let context = Some (Datakit_path.to_hum context_path) in
        ok { Status.state; commit; context; description; url; }

    let read_statuses ~root t =
      let dir = root / "commit" in
      Log.debug (fun l -> l "read_statuses %a" Datakit_path.pp dir);
      DK.Tree.exists_dir t dir >>*= fun exists ->
      if not exists then ok []
      else
        DK.Tree.read_dir t dir >>*=
        list_map (fun commit ->
            let dir = dir / commit / "status" in
            let rec aux context =
              Log.debug
                (fun l -> l "read_status context=%a" Fmt.(Dump.list string) context);
              let dir = dir /@ Datakit_path.of_steps_exn context in
              DK.Tree.exists_dir t dir >>*= fun exists ->
              if not exists then ok []
              else
                DK.Tree.read_dir t dir >>*= fun child ->
                list_map (fun c -> aux (context @ [c])) child >>*= fun child ->
                let child = List.flatten child in
                DK.Tree.exists_file t (dir / "state") >>*= fun exists ->
                if exists then read_status ~root t ~commit ~context >>*= fun s ->
                  ok (s :: child)
                else
                  ok child
            in
            aux []
          )
        >>*= fun status ->
        ok (List.flatten status)

  end

  (* FIXME: should be simply DK.Tree.t and/or a Tree ID so that it
     should be much more efficient to compare. *)
  type files = Cstruct.t Datakit_path.Map.t

  let compare_files = Datakit_path.Map.compare Cstruct.compare

  let read_files tree root =
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

  type hook = {
    user: string;
    repo: string;
    files: files;                              (* the files under /user/repo/ *)
  }

  let pp_hook ppf t = Fmt.pf ppf "%s/%s" t.user t.repo

  let compare_user_repo x y = compare (x.user, x.repo) (y.user, y.repo)

  module UserRepoSet = struct (* discard tree *)
    include Set.Make(struct type t = hook let compare = compare_user_repo end)
    let sdiff x y = union (diff x y) (diff y x)
    let pp ppf t = Fmt.(list pp_hook) ppf (elements t)
  end

  let compare_hook x y =
    match compare_user_repo x y with
    | 0 -> compare_files x.files y.files
    | i -> i

  module HookSet = Set.Make(struct type t = hook let compare = compare_hook end)

  type t = {
    tree      : DK.Tree.t option;
    user_repos: UserRepoSet.t;            (* active hooks, computed from tree *)
    hooks     : HookSet.t;         (* user_repo + FS tree, computed from tree *)
  }

  let empty =
    { hooks = HookSet.empty; user_repos = UserRepoSet.empty; tree = None }

  (* compute all the active hooks for a given DataKit commit *)
  let of_commit c =
    let tree = DK.Commit.tree c in
    let root = Datakit_path.empty in
    DK.Tree.read_dir tree root >>*= fun users ->
    List.fold_left (fun acc user ->
        DK.Tree.read_dir tree (root / user) >>*= fun repos ->
        List.fold_left (fun acc repo ->
            acc >>*= fun acc ->
            read_files tree (root / user / repo) >>*= fun files ->
            let hooks = HookSet.add { user; repo; files } acc.hooks in
            let user_repos =
              UserRepoSet.add { user; repo; files } acc.user_repos
            in
            ok { tree = Some tree; hooks; user_repos }
          ) acc repos
      ) (ok @@ empty) users

  (* Read events from the GitHub API and overwrite DataKit state with
     them, in chronological order. *)
  let sync_datakit token ~user ~repo tr =
    let root = Datakit_path.empty / user / repo in
    API.events token ~user ~repo >>= fun events ->
    list_iter (function
        | Event.PR pr    -> Conv.update_pr ~root tr pr
        | Event.Status s -> Conv.update_status ~root tr s
        | _               -> ok ()
      ) events

  (* Read the GitHub events for the repositories appearing in [diff]
     and populate [branch] with the result of applying all of the into
     the state. *)
  (* NOTE: quite slow (because of the call to API.events), so use it
     with care *)
  let import_github_events ~token branch diff =
    Log.debug (fun l -> l "import_github_events %a" UserRepoSet.pp diff);
    if UserRepoSet.is_empty diff then ok ()
    else DK.Branch.with_transaction branch (fun tr ->
        Lwt_list.iter_p (fun { user; repo; _ } ->
            sync_datakit token ~user ~repo tr >|= function
            | Ok ()   -> ()
            | Error e ->
              Log.err (fun l ->
                  l "Error while syncing %s/%s: %a" user repo DK.pp_error e
                );
          ) (UserRepoSet.elements diff)
        >>= fun () ->
        let message = Fmt.strf "Syncing with events %a" UserRepoSet.pp diff in
        DK.Transaction.commit tr ~message
      )

  let conv_read_statuses_opt ~root = function
    | None   -> ok []
    | Some t -> Conv.read_statuses ~root t

  let conv_read_prs_opt ~root = function
    | None   -> ok []
    | Some t -> Conv.read_prs ~root t

  (* Read DataKit data and call the GitHub API to sync the world with
     what DataKit think it should be.
     Also clean-up DataKit invariants such as GC-ing commit entries. *)
  (* TODO: handle pr_diffs too *)
  let call_github_api ~token ~old t =
    let aux { user; repo; _ } =
      let root = Datakit_path.empty / user / repo in
      conv_read_statuses_opt ~root old.tree >>*= fun old_status ->
      conv_read_statuses_opt ~root t.tree   >>*= fun new_status ->
      (* status cannot be removed, so simply monitor updates in [new_status]. *)
      let diff = Status.Set.(diff (of_list new_status) (of_list old_status)) in
      Log.debug
        (fun l -> l "call_github_api %s/%s: %a" user repo Status.Set.pp diff);
      List.iter (API.set_status token ~user ~repo) (Status.Set.elements diff);
      ok ()
    in
    let hooks = (* search for changes in subtrees. *)
      HookSet.elements t.hooks
      |> UserRepoSet.of_list
      |> UserRepoSet.elements
    in
    list_iter aux hooks

  let prune t branch =
    let aux { user; repo; _ } =
      Log.debug (fun l -> l "prune user=%s repo=%s" user repo);
      let root = Datakit_path.empty / user / repo in
      conv_read_statuses_opt ~root t.tree >>*= fun status ->
      conv_read_prs_opt ~root t.tree      >>*= fun prs ->
      Log.debug (fun l ->
          l "status:@ %a@ prs:@ %a"
            Fmt.(list Status.pp) status Fmt.(list PR.pp) prs
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
      Log.debug (fun l -> l "closed_prs:%a" Fmt.(list PR.pp) closed_prs);
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
          list_iter (fun pr ->
              DK.Transaction.remove tr (root / "pr" / string_of_int pr.PR.number)
            ) closed_prs
          >>*= fun () ->
          list_iter (fun commit ->
              DK.Transaction.remove tr (root / "commit" / commit)
            ) (String.Set.elements closed_commits)
          >>*= fun () ->
          DK.Transaction.commit tr
            ~message:"Pruning closed PRs and their commits."
        )
    in
    (* status cannot be removed, so simply monitor updates in
       [new_status]. *)
    list_iter aux (UserRepoSet.elements t.user_repos)

  let with_head branch fn =
    DK.Branch.head branch >>*= function
    | None   -> error "empty branch!"
    | Some c -> fn c

  let sync ?switch ?(policy=`Repeat) t branch ~writes token =
    Log.debug (fun l -> l "sync %s" @@ DK.Branch.name branch);
    let import_events last_t current =
      of_commit current >>*= fun current_t ->
      let diff = UserRepoSet.sdiff last_t.user_repos current_t.user_repos in
      Log.debug (fun l -> l "user-repo-diff: %a" UserRepoSet.pp diff);
      import_github_events ~token branch diff >>*= fun () ->
      ok ()
    in
    let github_calls last_t current =
      of_commit current >>*= fun current_t ->
      call_github_api ~token ~old:last_t current_t >>*= fun () ->
      ok ()
    in
    let prune current =
      of_commit current >>*= fun last_t ->
      prune last_t branch >>*= fun () ->
      ok ()
    in
    let merge_writes c =
      DK.Branch.with_transaction writes (fun tr ->
          DK.Transaction.merge tr c >>*= fun (_, conflicts) ->
          if conflicts <> [] then failwith "TODO";
          let msg = Fmt.strf "Merging with %s" @@ DK.Branch.name branch in
          DK.Transaction.commit tr ~message:msg
        ) >>*= fun _ ->
      ok ()
    in
    let run () =
      with_head branch merge_writes >>*= fun () ->
      (* 1. handle user requests since last iteration. *)
      with_head writes (github_calls t) >>*= fun () ->
      (* 2. import GitHub events for new repositories. *)
      with_head branch (import_events t) >>*= fun () ->
      (* 3. prune *)
      with_head branch prune >>*= fun () ->

      match policy with
      | `Once   -> ok (`Finish ())
      | `Repeat ->
        let last = ref t in
        DK.Branch.wait_for_head ?switch branch (function
            | None   -> ok `Again
            | Some c ->
              (* Note: we use the same commit [c] for import and
                 prune, so might need multiple calls to stabilise. *)
              merge_writes c >>*= fun () ->
              github_calls !last c >>*= fun () ->
              import_events !last c >>*= fun () ->
              prune c >>*= fun () ->
              of_commit c >>*= fun l ->
              last := l;
              ok `Again
          )
    in
    (run () >>*= fun _ -> (* FIXME: racy *) with_head branch of_commit)
    >>= function
    | Ok t    -> Lwt.return t
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" DK.pp_error e

end
