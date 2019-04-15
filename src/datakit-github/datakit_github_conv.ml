open Lwt.Infix
open Datakit_client.Path.Infix
open Datakit_github
open Result
open Datakit_client

let src = Logs.Src.create "dkt-github" ~doc:"Github to Git bridge"

module Log = (val Logs.src_log src : Logs.LOG)

let ( >>*= ) x f = x >>= function Ok x -> f x | Error _ as e -> Lwt.return e

let pp_path = Fmt.(list ~sep:(unit "/") string)

let mapo f = function None -> None | Some x -> Some (f x)

let failf fmt = Fmt.kstrf failwith fmt

module Make (DK : S) = struct
  type tree = DK.Tree.t

  (* conversion between GitHub and DataKit states. *)

  let path s = Path.of_steps_exn s

  (* TODO: Lots of these functions used to ignore errors silently. This can lead
     to bugs in the users of the library (e.g. we lost our 9p connection but
     we think instead that the file we wanted doesn't exist). For now, I've
     converted it to log errors in these cases but continue with the old
     behaviour. Assuming we don't see these errors being logged, we can
     change the code to raise exceptions instead. *)

  let remove_if_exists t path =
    DK.Transaction.remove t path >|= function
    | Error `Does_not_exist | Ok () -> ()
    | Error e -> failf "remove_if_exists(%a): %a" Path.pp path DK.pp_error e

  let read_dir_if_exists t dir =
    DK.Tree.read_dir t dir >|= function
    | Ok dirs -> dirs
    | Error (`Does_not_exist | `Not_dir) -> []
    | Error e -> failf "safe_read_dir(%a): %a" Path.pp dir DK.pp_error e

  let exists_dir t dir =
    DK.Tree.exists_dir t dir >|= function
    | Ok b -> b
    | Error `Not_dir ->
        false (* Some parent doesn't exist or isn't a directory *)
    | Error e -> failf "exists_dir(%a): %a" Path.pp dir DK.pp_error e

  let exists_file t file =
    DK.Tree.exists_file t file >|= function
    | Ok b -> b
    | Error `Not_dir ->
        false (* Some parent doesn't exist or isn't a directory *)
    | Error e -> failf "exists_file(%a): %a" Path.pp file DK.pp_error e

  let read_file_if_exists ?(trim = true) t file =
    DK.Tree.read_file t file >|= function
    | Ok b ->
        let b = Cstruct.to_string b in
        Some (if trim then String.trim b else b)
    | Error (`Does_not_exist | `Not_dir) -> None
    | Error e -> failf "read_file(%a): %a" Path.pp file DK.pp_error e

  let create_file tr file contents =
    match Path.basename file with
    | None -> failf "%a is not a file" Path.pp file
    | Some _ -> (
        let dir = Path.dirname file in
        ( DK.Transaction.make_dirs tr dir >>*= fun () ->
          DK.Transaction.create_or_replace_file tr file contents )
        >|= function
        | Ok () -> ()
        | Error e ->
            failf "Got %a while creating %a" DK.pp_error e Path.pp file )

  let tr_diff tr c =
    DK.Transaction.diff tr c >|= function
    | Ok d -> d
    | Error e -> failf "tr_diff: %a" DK.pp_error e

  let lift_errors name f =
    f >>= function
    | Error e -> Lwt.fail_with @@ Fmt.strf "%s: %a" name DK.pp_error e
    | Ok x -> Lwt.return x

  let path_of_diff = function
    | `Added f | `Removed f | `Updated f -> Path.unwrap f

  let safe_tree c =
    DK.Commit.tree c >>= function
    | Error e -> Fmt.kstrf Lwt.fail_with "%a" DK.pp_error e
    | Ok tree -> Lwt.return tree

  type dirty = Elt.IdSet.t

  let changes diff =
    let rdecons l =
      match List.rev l with [] -> assert false | h :: t -> (h, List.rev t)
    in
    List.fold_left
      (fun (acc, dirty) d ->
        let path = path_of_diff d in
        let added = match d with `Removed _ -> false | _ -> true in
        let t =
          match path with
          | [] | [ _ ] -> None
          | user :: repo :: path -> (
              let user = User.v user in
              let repo = Repo.v ~user ~repo in
              let pr repo id = `PR (repo, int_of_string id) in
              match path with
              | [] | [ ".monitor" ] -> Some (`Repo repo)
              | [ ".dirty" ] when added -> Some (`Dirty (`Repo repo))
              | [ "pr"; id; ".dirty" ] when added -> Some (`Dirty (pr repo id))
              | "pr" :: id :: _ -> Some (pr repo id)
              | [ "commit"; id ] -> Some (`Commit (Commit.v repo id))
              | "commit" :: id :: "status" :: (_ :: _ :: _ as tl) ->
                  let _, last = rdecons tl in
                  Some (`Status (Commit.v repo id, last))
              | "ref" :: (_ :: _ :: _ as tl) ->
                  let f, last = rdecons tl in
                  let r = `Ref (repo, last) in
                  if f = ".dirty" then Some (`Dirty r) else Some r
              | _ -> None )
        in
        match t with
        | None -> (acc, dirty)
        | Some (`Dirty d) -> (acc, Elt.IdSet.add d dirty)
        | Some (#Elt.id as e) -> (Elt.IdSet.add e acc, dirty) )
      (Elt.IdSet.empty, Elt.IdSet.empty)
      diff

  let safe_diff x y =
    DK.Commit.diff x y >|= function
    | Error e ->
        Log.err (fun f -> f "safe_diff: %a" DK.pp_error e);
        (Elt.IdSet.empty, Elt.IdSet.empty)
    | Ok d -> changes d

  let walk (type elt t) (module Set : SET with type elt = elt and type t = t)
      tree root (file, fn) =
    let rec aux acc = function
      | [] -> Lwt.return acc
      | context :: todo -> (
        match Path.of_steps context with
        | Error e ->
            Log.err (fun l -> l "%s" e);
            aux acc todo
        | Ok ctx -> (
            let dir = root /@ ctx in
            read_dir_if_exists tree dir >>= fun childs ->
            let todo = List.map (fun c -> context @ [ c ]) childs @ todo in
            exists_file tree (dir / file) >>= function
            | false -> aux acc todo
            | true -> (
                fn (Path.unwrap ctx) >>= function
                | None -> aux acc todo
                | Some e -> aux (Set.add e acc) todo ) ) )
    in
    aux Set.empty [ [] ]

  let empty = Path.empty

  let root r = empty / User.name r.Repo.user / r.Repo.repo

  (* Repos *)

  let repo tree repo =
    read_file_if_exists tree (root repo / ".monitor") >|= function
    | None ->
        Log.debug (fun l -> l "repo %a -> false" Repo.pp repo);
        None
    | Some _ ->
        Log.debug (fun l -> l "repo %a -> true" Repo.pp repo);
        Some repo

  let reduce_repos = List.fold_left Repo.Set.union Repo.Set.empty

  let repos tree =
    let root = Path.empty in
    read_dir_if_exists tree root >>= fun users ->
    Lwt_list.map_p
      (fun user ->
        read_dir_if_exists tree (root / user) >>= fun repos ->
        Lwt_list.map_p
          (fun repo ->
            read_file_if_exists tree (root / user / repo / ".monitor")
            >|= function
            | None -> Repo.Set.empty
            | Some _ ->
                let user = User.v user in
                let repo = Repo.v ~user ~repo in
                Repo.Set.singleton repo )
          repos
        >|= reduce_repos )
      users
    >|= fun repos ->
    let repos = reduce_repos repos in
    Log.debug (fun l -> l "repos -> @;@[<2>%a@]" Repo.Set.pp repos);
    repos

  let update_repo_aux tr s r =
    let dir = root r in
    match s with
    | `Ignored -> remove_if_exists tr (root r / ".monitor")
    | `Monitored ->
        let remove =
          DK.Transaction.make_dirs tr dir >>*= fun () ->
          let empty = Cstruct.of_string "" in
          DK.Transaction.create_or_replace_file tr (dir / ".monitor") empty
        in
        lift_errors "update_repo" remove

  let update_repo tr r = update_repo_aux tr `Monitored r

  let remove_repo tr r = update_repo_aux tr `Ignored r

  let update_commit tr c =
    let dir = root (Commit.repo c) / "commit" in
    lift_errors "update_commit" @@ DK.Transaction.make_dirs tr dir

  (* PRs *)

  let update_pr t pr =
    let dir = root (PR.repo pr) / "pr" / string_of_int pr.PR.number in
    Log.debug (fun l -> l "update_pr %s" @@ Path.to_hum dir);
    let update =
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let write ?prefix ?(newline = true) k v =
        let v = Cstruct.of_string (if newline then v ^ "\n" else v) in
        let dir = match prefix with None -> dir | Some p -> dir /@ p in
        DK.Transaction.create_or_replace_file t (dir / k) v
      in
      write "head" (PR.commit_hash pr) >>*= fun () ->
      write "state" (PR.string_of_state pr.PR.state) >>*= fun () ->
      write "title" pr.PR.title >>*= fun () ->
      write "owner" (User.name pr.PR.owner) >>*= fun () ->
      write "base" pr.PR.base >>*= fun () ->
      remove_if_exists t (dir / "comments") >>= fun () ->
      Lwt_list.mapi_p
        (fun id c ->
          let prefix = Path.empty / "comments" / string_of_int id in
          DK.Transaction.make_dirs t (dir /@ prefix) >>*= fun () ->
          let user = User.name c.Comment.user in
          write ~prefix "id" (string_of_int c.Comment.id) >>*= fun () ->
          write ~prefix "user" user >>*= fun () ->
          write ~newline:false ~prefix "body" c.Comment.body )
        (Array.to_list pr.PR.comments)
      >>= fun l ->
      List.fold_left
        (fun acc x -> acc >>*= fun () -> Lwt.return x)
        (Lwt.return (Ok ())) l
    in
    lift_errors "update_pr" update

  let remove_pr t (repo, num) =
    let dir = root repo / "pr" / string_of_int num in
    Log.debug (fun l -> l "remove_pr %s" @@ Path.to_hum dir);
    remove_if_exists t dir

  let comments tree dir =
    read_dir_if_exists tree dir >>= fun ids ->
    Lwt_list.map_p
      (fun n ->
        read_file_if_exists tree (dir / n / "id") >>= fun rid ->
        read_file_if_exists tree (dir / n / "user") >>= fun user ->
        read_file_if_exists ~trim:false tree (dir / n / "body") >|= fun body ->
        let body = match body with None -> "" | Some b -> b in
        let id =
          match rid with
          | None -> None
          | Some id -> ( try Some (int_of_string id) with Failure _ -> None )
        in
        match (id, user) with
        | Some id, Some name ->
            let user = User.v name in
            Some (Comment.v ~id ~user ~body)
        | Some id, None ->
            Log.debug (fun l ->
                l "error: %a/comments/%d/author does not exist" Path.pp dir id
            );
            None
        | _ ->
            Log.debug (fun l ->
                l "error: %a/comments: %s is not a valid id" Path.pp dir n );
            None )
      ids
    >|= fun comments ->
    List.fold_left
      (fun acc -> function None -> acc | Some x -> x :: acc)
      [] (List.rev comments)
    |> Array.of_list

  let pr tree (repo, number) =
    let dir = root repo / "pr" / string_of_int number in
    Log.debug (fun l -> l "pr %a" Path.pp dir);
    read_file_if_exists tree (dir / "head") >>= fun head ->
    read_file_if_exists tree (dir / "state") >>= fun state ->
    read_file_if_exists tree (dir / "title") >>= fun title ->
    read_file_if_exists tree (dir / "owner") >>= fun owner ->
    comments tree (dir / "comments") >>= fun comments ->
    read_file_if_exists tree (dir / "base") >|= fun base ->
    match (head, state, owner) with
    | None, _, _ ->
        Log.debug (fun l ->
            l "error: %a/pr/%d/head does not exist" Repo.pp repo number );
        None
    | _, None, _ ->
        Log.debug (fun l ->
            l "error: %a/pr/%d/state does not exist" Repo.pp repo number );
        None
    | _, _, None ->
        Log.debug (fun l ->
            l "error: %a/pr/%d/owner does not exist" Repo.pp repo number );
        None
    | Some id, Some state, Some owner ->
        let base =
          match base with
          | Some b -> b
          | None ->
              Log.debug (fun l ->
                  l
                    "error: %a/pr/%d/base does not exist, using 'master' \
                     instead"
                    Repo.pp repo number );
              "master"
        in
        let owner = User.v owner in
        let head = Commit.v repo id in
        let title = match title with None -> "" | Some t -> t in
        let state =
          match PR.state_of_string state with
          | Some s -> s
          | None ->
              Log.err (fun l ->
                  l "%s is not a valid PR state, picking `Closed instead" state
              );
              `Closed
        in
        Some (PR.v ~state ~title ~base ~owner ~comments head number)

  let reduce_prs = List.fold_left PR.Set.union PR.Set.empty

  let prs_of_repo tree repo =
    let dir = root repo / "pr" in
    read_dir_if_exists tree dir >>= fun nums ->
    Lwt_list.map_p
      (fun n ->
        pr tree (repo, int_of_string n) >|= function
        | None -> PR.Set.empty
        | Some p -> PR.Set.singleton p )
      nums
    >|= fun prs ->
    let prs = reduce_prs prs in
    Log.debug (fun l ->
        l "prs_of_repo %a -> @;@[<2>%a@]" Repo.pp repo PR.Set.pp prs );
    prs

  let maybe_repos tree = function
    | None -> repos tree
    | Some rs -> Lwt.return rs

  let prs ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.map_p (prs_of_repo tree) (Repo.Set.elements repos) >|= fun prs ->
    let prs = reduce_prs prs in
    Log.debug (fun l -> l "prs -> @;@[<2>%a@]" PR.Set.pp prs);
    prs

  (* Commits *)

  let commit tree { Commit.repo; hash } =
    let dir = root repo / "commit" / hash in
    exists_dir tree dir >|= function
    | false ->
        Log.debug (fun l -> l "commit {%a %s} -> false" Repo.pp repo hash);
        None
    | true ->
        Log.debug (fun l -> l "commit {%a %s} -> true" Repo.pp repo hash);
        Some (Commit.v repo hash)

  let commits_of_repo tree repo =
    let dir = root repo / "commit" in
    read_dir_if_exists tree dir >|= fun commits ->
    List.fold_left
      (fun s id -> Commit.Set.add (Commit.v repo id) s)
      Commit.Set.empty commits
    |> fun cs ->
    Log.debug (fun l ->
        l "commits_of_repo %a -> @;@[<2>%a@]" Repo.pp repo Commit.Set.pp cs );
    cs

  let reduce_commits = List.fold_left Commit.Set.union Commit.Set.empty

  let commits ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.map_p (commits_of_repo tree) (Repo.Set.elements repos)
    >|= fun cs ->
    let cs = reduce_commits cs in
    Log.debug (fun l -> l "commits -> @;@[<2>%a@]" Commit.Set.pp cs);
    cs

  (* Status *)

  let update_status t s =
    let dir =
      root (Status.repo s)
      / "commit" / Status.commit_hash s / "status"
      /@ path (Status.context s)
    in
    Log.debug (fun l -> l "update_status %a" Path.pp dir);
    lift_errors "update_status" (DK.Transaction.make_dirs t dir) >>= fun () ->
    let description =
      match Status.description s with
      | None -> None
      | Some d -> Some (String.trim d)
    in
    let kvs =
      [ ("description", description);
        ("state", Some (Status_state.to_string @@ Status.state s));
        ("target_url", mapo Uri.to_string (Status.url s))
      ]
    in
    Lwt_list.iter_p
      (fun (k, v) ->
        match v with
        | None -> remove_if_exists t (dir / k)
        | Some v ->
            let v = Cstruct.of_string (v ^ "\n") in
            lift_errors "update_status"
            @@ DK.Transaction.create_or_replace_file t (dir / k) v )
      kvs

  let status tree (commit, context) =
    let context = Path.of_steps_exn context in
    let dir =
      root (Commit.repo commit)
      / "commit" / Commit.hash commit / "status" /@ context
    in
    read_file_if_exists tree (dir / "state") >>= fun state ->
    match state with
    | None ->
        Log.debug (fun l -> l "status %a -> None" Path.pp dir);
        Lwt.return_none
    | Some str ->
        let state =
          match Status_state.of_string str with
          | Some s -> s
          | None ->
              Log.err (fun l ->
                  l "%s: invalid state, using `Failure instead" str );
              `Failure
        in
        Log.debug (fun l ->
            l "status %a -> %a" Path.pp context Status_state.pp state );
        read_file_if_exists tree (dir / "description") >>= fun description ->
        read_file_if_exists tree (dir / "target_url") >|= fun url ->
        let context = Path.unwrap context in
        let url = mapo Uri.of_string url in
        Some (Status.v ?description ?url commit context state)

  let reduce_status = List.fold_left Status.Set.union Status.Set.empty

  let statuses_of_commits tree commits =
    Lwt_list.map_p
      (fun commit ->
        let dir = root (Commit.repo commit) / "commit" in
        let dir = dir / Commit.hash commit / "status" in
        walk
          (module Status.Set)
          tree dir
          ("state", fun c -> status tree (commit, c)) )
      (Commit.Set.elements commits)
    >|= fun status ->
    let status = reduce_status status in
    Log.debug (fun l ->
        l "statuses_of_commits %a -> @;@[<2>%a@]" Commit.Set.pp commits
          Status.Set.pp status );
    status

  let maybe_commits tree = function
    | None -> commits tree
    | Some c -> Lwt.return c

  let statuses ?commits:cs tree =
    maybe_commits tree cs >>= fun commits ->
    statuses_of_commits tree commits >|= fun status ->
    Log.debug (fun l -> l "statuses -> @;@[<2>%a@]" Status.Set.pp status);
    status

  (* Refs *)

  let ref tree (repo, name) =
    let path = Path.of_steps_exn name in
    let head = root repo / "ref" /@ path / "head" in
    read_file_if_exists tree head >|= function
    | None ->
        Log.debug (fun l -> l "ref_ %a:%a -> None" Repo.pp repo pp_path name);
        None
    | Some id ->
        Log.debug (fun l -> l "ref_ %a:%a -> %s" Repo.pp repo pp_path name id);
        let head = Commit.v repo id in
        Some (Ref.v head name)

  let refs_of_repo tree repo =
    let dir = root repo / "ref" in
    walk (module Ref.Set) tree dir ("head", fun n -> ref tree (repo, n))
    >|= fun refs ->
    Log.debug (fun l ->
        l "refs_of_repo %a -> @;@[<2>%a@]" Repo.pp repo Ref.Set.pp refs );
    refs

  let reduce_refs = List.fold_left Ref.Set.union Ref.Set.empty

  let refs ?repos:rs tree =
    maybe_repos tree rs >>= fun repos ->
    Lwt_list.map_p (refs_of_repo tree) (Repo.Set.elements repos)
    >|= fun refs ->
    let refs = reduce_refs refs in
    Log.debug (fun l -> l "refs -> @;@[<2>%a@]" Ref.Set.pp refs);
    refs

  let update_ref tr r =
    let path = Path.of_steps_exn (Ref.name r) in
    let dir = root (Ref.repo r) / "ref" /@ path in
    Log.debug (fun l -> l "update_ref %a" Path.pp dir);
    let update =
      DK.Transaction.make_dirs tr dir >>*= fun () ->
      let head = Cstruct.of_string (Ref.commit_hash r ^ "\n") in
      DK.Transaction.create_or_replace_file tr (dir / "head") head
    in
    lift_errors "update_ref" update

  let remove_ref tr (repo, name) =
    let path = Path.of_steps_exn name in
    let dir = root repo / "ref" /@ path in
    Log.debug (fun l -> l "remove_ref %a" Path.pp dir);
    remove_if_exists tr dir

  let update_event t = function
    | Event.Repo (s, r) -> update_repo_aux t s r
    | Event.PR pr -> update_pr t pr
    | Event.Status s -> update_status t s
    | Event.Ref (`Removed r) -> remove_ref t r
    | Event.Ref (`Created r | `Updated r) -> update_ref t r
    | Event.Other o ->
        Log.debug (fun l -> l "ignoring event: %s" @@ snd o);
        Lwt.return_unit

  (* Snapshot *)

  let snapshot_of_repos tree repos =
    commits ~repos tree >>= fun commits ->
    prs ~repos tree >>= fun prs ->
    statuses ~commits tree >>= fun status ->
    refs ~repos tree >|= fun refs ->
    Snapshot.v ~repos ~status ~prs ~refs ~commits

  let snapshot_of_commit c =
    safe_tree c >>= fun tree ->
    repos tree >>= fun repos -> snapshot_of_repos tree repos

  (* Dirty *)

  let reduce_elts = List.fold_left Elt.IdSet.union Elt.IdSet.empty

  let dirty_repos tree =
    let root = Path.empty in
    read_dir_if_exists tree root >>= fun users ->
    Lwt_list.map_p
      (fun user ->
        read_dir_if_exists tree (root / user) >>= fun repos ->
        Lwt_list.map_p
          (fun repo ->
            exists_file tree (root / user / repo / ".dirty") >|= function
            | false -> Elt.IdSet.empty
            | true ->
                let user = User.v user in
                let repo = Repo.v ~user ~repo in
                Elt.IdSet.singleton (`Repo repo) )
          repos
        >|= reduce_elts )
      users
    >|= reduce_elts

  let dirty_prs tree repo =
    let dir = root repo / "pr" in
    read_dir_if_exists tree dir >>= fun nums ->
    Lwt_list.map_p
      (fun n ->
        let d = dir / n / ".dirty" in
        exists_file tree d >|= function
        | false -> Elt.IdSet.empty
        | true -> (
          try Elt.IdSet.singleton (`PR (repo, int_of_string n))
          with Failure _ -> Elt.IdSet.empty ) )
      nums
    >|= reduce_elts

  let dirty_refs tree repo =
    let dir = root repo / "ref" in
    let r name = Lwt.return (Some (`Ref (repo, name))) in
    walk (module Elt.IdSet) tree dir (".dirty", r)

  let dirty_of_commit c : dirty Lwt.t =
    safe_tree c >>= fun t ->
    let ( ++ ) = Elt.IdSet.union in
    (* we handle dirty repo even if not monitored *)
    dirty_repos t >>= fun dirty_repos ->
    repos t >>= fun repos ->
    (* we only check for dirty prs/refs for monitored repos only *)
    Lwt_list.map_p
      (fun r ->
        dirty_prs t r >>= fun prs -> dirty_refs t r >|= fun refs -> prs ++ refs
        )
      (Repo.Set.elements repos)
    >|= fun more -> dirty_repos ++ reduce_elts more

  let dirty_file : Elt.id -> Path.t = function
    | `Repo r -> root r / ".dirty"
    | `PR (r, id) -> root r / "pr" / string_of_int id / ".dirty"
    | `Ref (r, n) -> root r / "ref" /@ Path.of_steps_exn n / ".dirty"
    | _ -> failwith "TODO"

  let clean tr dirty =
    Lwt_list.iter_p (fun d -> remove_if_exists tr (dirty_file d))
    @@ Elt.IdSet.elements dirty

  let empty = Cstruct.of_string ""

  let stain tr dirty =
    Lwt_list.iter_p (fun d -> create_file tr (dirty_file d) empty)
    @@ Elt.IdSet.elements dirty

  (* Elements *)

  let find t (id : Elt.id) =
    match id with
    | `Repo id -> repo t id >|= mapo (fun r -> `Repo r)
    | `Commit id -> commit t id >|= mapo (fun c -> `Commit c)
    | `PR id -> pr t id >|= mapo (fun p -> `PR p)
    | `Ref id -> ref t id >|= mapo (fun r -> `Ref r)
    | `Status id -> status t id >|= mapo (fun s -> `Status s)

  (* Diffs *)

  let combine_repo t tree r =
    repo tree r >>= function
    | None -> Lwt.return (Diff.with_remove (`Repo r) t)
    | Some r ->
        snapshot_of_repos tree (Repo.Set.singleton r) >|= fun s ->
        Elt.Set.fold Diff.with_update (Snapshot.elts s) t

  let combine_commit t tree c =
    commit tree c >|= function
    | None -> Diff.with_remove (`Commit c) t
    | Some c -> Diff.with_update (`Commit c) t

  let combine_pr t tree id =
    pr tree id >|= function
    | Some pr -> Diff.with_update (`PR pr) t
    | None -> Diff.with_remove (`PR id) t

  let combine_status t tree id =
    status tree id >|= function
    | None -> Diff.with_remove (`Status id) t
    | Some s -> Diff.with_update (`Status s) t

  let combine_ref t tree id =
    ref tree id >|= function
    | None -> Diff.with_remove (`Ref id) t
    | Some r -> Diff.with_update (`Ref r) t

  let apply_on_commit diff head =
    Log.debug (fun l -> l "apply");
    safe_tree head >>= fun tree ->
    if Elt.IdSet.is_empty diff then Lwt.return Diff.empty
    else
      Lwt_list.fold_left_s
        (fun acc -> function `Repo repo -> combine_repo acc tree repo
          | `PR id -> combine_pr acc tree id
          | `Ref id -> combine_ref acc tree id
          | `Commit id -> combine_commit acc tree id
          | `Status id ->
              combine_status acc tree id >>= fun acc ->
              combine_commit acc tree (fst id) )
        Diff.empty (Elt.IdSet.elements diff)
      >|= fun r ->
      Log.debug (fun l ->
          l "apply @[<2>%a@]@;@[<2>->%a@]" Elt.IdSet.pp diff Diff.pp r );
      r

  type t = { head : DK.Commit.t; snapshot : Snapshot.t; dirty : dirty }

  let snapshot t = t.snapshot

  let head t = t.head

  let dirty t = t.dirty

  let pp ppf s =
    Fmt.pf ppf "@[%a:@;@[<2>%a@]@]" DK.Commit.pp s.head Snapshot.pp s.snapshot

  let diff x y =
    safe_diff x y >>= fun (diff, dirty) ->
    apply_on_commit diff x >|= fun s -> (s, dirty)

  let tr_head tr =
    DK.Transaction.parents tr >>= function
    | Error e ->
        Log.err (fun l -> l "tr_head: %a" DK.pp_error e);
        Lwt.fail_with "tr_head"
    | Ok [] -> Lwt.fail_with "no parents!"
    | Ok [ p ] -> Lwt.return p
    | Ok _ -> Lwt.fail_with "too many parents!"

  let of_branch ~debug ?old branch =
    DK.Branch.transaction branch >>= function
    | Error e ->
        Log.err (fun l ->
            l "snpshot %s: %a" (DK.Branch.name branch) DK.pp_error e );
        Lwt.fail_with "snapshot"
    | Ok tr -> (
        Log.debug (fun l ->
            let c =
              match old with None -> "*" | Some t -> DK.Commit.id t.head
            in
            l "snapshot %s old=%s" debug c );
        tr_head tr >>= fun head ->
        match old with
        | None ->
            snapshot_of_commit head >>= fun snapshot ->
            dirty_of_commit head >|= fun dirty ->
            (tr, { head; snapshot; dirty })
        | Some old ->
            diff head old.head >|= fun (diff, dirty) ->
            let snapshot = Diff.apply diff old.snapshot in
            (tr, { head; snapshot; dirty }) )

  let of_commit ~debug ?old head =
    Log.debug (fun l ->
        let c = match old with None -> "*" | Some t -> DK.Commit.id t.head in
        l "snapshot %s old=%s" debug c );
    match old with
    | None ->
        snapshot_of_commit head >>= fun snapshot ->
        dirty_of_commit head >|= fun dirty -> { head; snapshot; dirty }
    | Some old ->
        diff head old.head >|= fun (diff, dirty) ->
        let snapshot = Diff.apply diff old.snapshot in
        { head; snapshot; dirty }

  let remove_elt tr = function
    | `Repo repo -> remove_repo tr repo
    | `PR pr -> remove_pr tr pr
    | `Ref r -> remove_ref tr r
    | `Status (h, c) ->
        let dir =
          root (Commit.repo h) / "commit" / Commit.hash h / "status" /@ path c
        in
        remove_if_exists tr dir
    | `Commit c ->
        let dir = root (Commit.repo c) / "commit" / c.Commit.hash in
        remove_if_exists tr dir

  let update_elt tr = function
    | `Repo r -> update_repo tr r
    | `Commit c -> update_commit tr c
    | `PR pr -> update_pr tr pr
    | `Ref r -> update_ref tr r
    | `Status s -> update_status tr s

  let remove ~debug t =
    if Elt.IdSet.is_empty t then None
    else
      let f tr =
        Log.debug (fun l ->
            l "remove_snapshot (from %s):@;%a" debug Elt.IdSet.pp t );
        Lwt_list.iter_p (remove_elt tr) (Elt.IdSet.elements t)
      in
      Some f

  let update ~debug t =
    if Elt.Set.is_empty t then None
    else
      let f tr =
        Log.debug (fun l ->
            l "update_snapshot (from %s):@;%a" debug Elt.Set.pp t );
        Lwt_list.iter_p (update_elt tr) (Elt.Set.elements t)
      in
      Some f

  let apply ~debug diff tr =
    let clean () =
      match remove ~debug (Diff.remove diff) with
      | None -> Lwt.return_unit
      | Some f -> f tr
    in
    let update () =
      match update ~debug (Diff.update diff) with
      | None -> Lwt.return_unit
      | Some f -> f tr
    in
    tr_head tr >>= fun head ->
    clean () >>= fun () ->
    update () >>= fun () -> tr_diff tr head >|= fun diff -> diff <> []
end
