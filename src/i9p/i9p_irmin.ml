open Lwt.Infix
open Result
open Fs9p_misc

module PathSet = I9p_merge.PathSet

module type S = sig
  type repo
  type dir
  val create: string Irmin.Task.f -> repo -> dir
end

let ok x = Lwt.return (Ok x)
let err_enoent = Lwt.return Fs9p_error.enoent
let err_eisdir = Lwt.return Fs9p_error.eisdir
let err_read_only = Lwt.return Fs9p_error.ero
let err_already_exists name = Lwt.return (error "Entry %S already exists" name)
let err_conflict msg = Lwt.return (error "Merge conflict: %s" msg)
let err_unknown_cmd x = Lwt.return (error "Unknown command %S" x)
let err_invalid_commit_id x = error "Invalid commit ID %S" x

module InodeMap = Fs9p_dir.InodeMap

module Make (Inode : Fs9p_inode.S) (Store : I9p_tree.STORE) = struct
  module Dir = Fs9p_dir.Make(Inode)

  type repo = Store.Repo.t
  type dir = Inode.dir

  module Path = Irmin.Path.String_list
  module Tree = I9p_tree.Make(Store)
  module View = Irmin.View(Store)
  module Merge = I9p_merge.Make(Store)(View)

  let irmin_ro_file ~get_root path =
    let read () =
      get_root () >>= fun root ->
      Tree.node root path >>= function
      | `None         -> Lwt.return (Ok None)
      | `Directory _  -> err_eisdir
      | `File content ->
        let contents_t = Store.Private.Repo.contents_t (Tree.repo root) in
        Store.Private.Contents.read_exn contents_t content >|= fun content ->
        Ok (Some (Cstruct.of_string content))
    in
    Fs9p_file.read_only ~read

  let irmin_rw_file ~remove_conflict ~view path =
    let read () =
      View.read view path >|= function
      | None         -> Ok None
      | Some content -> Ok (Some (Cstruct.of_string content))
    in
    let write data =
      View.update view path (Cstruct.to_string data) >|= fun () ->
      remove_conflict path; Ok () in
    let remove () = View.remove view path >|= fun () ->
      remove_conflict path; Ok () in
   Fs9p_file.read_write ~read ~write ~remove

  let name_of_irmin_path ~root path =
    match Path.rdecons path with
    | None -> root
    | Some (_, leaf) -> leaf

  let ro_tree ~name ~get_root =
    let name_of_irmin_path = name_of_irmin_path ~root:name in
    (* Keep track of which qids we're still using. We need to give the same qid to the
       client each time. TODO: use a weak map here. *)
    let nodes = Hashtbl.create 10 in

    let rec get ~dir (ty, leaf) =
      let hash_key = (ty, Path.rcons dir leaf) in
      try Hashtbl.find nodes hash_key
      with Not_found ->
        let inode = inode_of hash_key in
        Hashtbl.add nodes hash_key inode;
        inode

    and inode_of (ty, full_path) =
      match ty with
      | `Directory -> irmin_ro_dir full_path
      | `File      ->
        let path = name_of_irmin_path full_path in
        let file = irmin_ro_file ~get_root full_path in
        Inode.of_file path file

    and irmin_ro_dir path =
      let name = name_of_irmin_path path in
      let ls () =
        get_root () >>= fun root ->
        Tree.get_dir root path >>= function
        | None -> err_enoent
        | Some dir ->
          Tree.ls dir >>= fun items ->
          ok (List.map (get ~dir:path) items)
      in
      let lookup name =
        get_root () >>= fun root ->
        Tree.get_dir root path >>= function
        | None -> err_enoent
        | Some dir ->
          Tree.ty dir name >>= function
          | `File
          | `Directory as ty -> ok (get ~dir:path (ty, name))
          | `None            -> err_enoent
      in
      let remove () = Fs9p_dir.err_ro in
      Dir.read_only ~ls ~lookup ~remove () |> Inode.of_dir name in
    irmin_ro_dir []

  let ro store = ro_tree ~get_root:(fun () -> Tree.snapshot store)

  (* Ugly! *)
  let snapshot ~store view =
    let task () = Irmin.Task.empty in
    let repo = Store.repo (store "snapshot") in
    Store.empty task repo >>= fun dummy_store ->
    let dummy_store = dummy_store () in
    View.make_head dummy_store Irmin.Task.empty ~parents:[] ~contents:view
    >>= fun commit_id ->
    Store.of_commit_id task commit_id repo >>= fun store ->
    Tree.snapshot (store ())

  let remove_shadowed_by items map =
    List.fold_left (fun acc (_, name) ->
        acc |> InodeMap.remove name
      ) map items

  let rec has_prefix ~prefix p =
    match prefix, p with
    | [], _ -> true
    | pre::pres, p::ps ->
        if pre = p then has_prefix ~prefix:pres ps
        else false
    | _, [] -> false

  (* Note: writing to a path removes it from [conflicts], if present *)
  let rw ~conflicts ~store view =
    let remove_conflict path =
      conflicts := !conflicts |> PathSet.remove path in
    let name_of_irmin_path = name_of_irmin_path ~root:"rw" in
    (* Keep track of which qids we're still using. We need to give the
       same qid to the client each time. TODO: use a weak map here. *)
    let nodes = Hashtbl.create 10 in

    let rec get ~dir (ty, leaf) =
      let hash_key = (ty, Path.rcons dir leaf) in
      try Hashtbl.find nodes hash_key
      with Not_found ->
        let inode = inode_of hash_key in
        Hashtbl.add nodes hash_key inode;
        inode

    and inode_of (ty, full_path) =
      match ty with
      | `Directory -> irmin_rw_dir full_path
      | `File ->
        let path = name_of_irmin_path full_path in
        let file = irmin_rw_file ~remove_conflict ~view full_path in
        Inode.of_file path file

    and irmin_rw_dir path =
      let name = name_of_irmin_path path in
      (* Irmin doesn't store empty directories, so we need to store
         that list in the server's memory. Meet [extra_dirs]. *)
      let extra_dirs = ref InodeMap.empty in
      let ls () =
        snapshot ~store view >>= fun root ->
        begin Tree.get_dir root path >>= function
          | None     -> Lwt.return []   (* in parent's extra_dirs? *)
          | Some dir -> Tree.ls dir
        end >>= fun items ->
        extra_dirs := remove_shadowed_by items !extra_dirs;
        let extra_inodes = InodeMap.bindings !extra_dirs |> List.map snd in
        ok (extra_inodes @ List.map (get ~dir:path) items)
      in
      let create name =
        let new_path = Path.rcons path name in
        View.update view new_path "" >|= fun () ->
        remove_conflict new_path;
        Ok (get ~dir:path (`File, name))
      in
      let lookup name =
        let real_result =
          snapshot ~store view >>= fun snapshot ->
          Tree.get_dir snapshot path >>= function
          | None -> err_enoent
          | Some dir ->
            Tree.ty dir name >>= function
            | `File
            | `Directory as ty -> ok (get ~dir:path (ty, name))
            | `None            -> err_enoent
        in
        real_result >|= function
        | Ok _ as ok   -> ok
        | Error _ as e ->
          try Ok (InodeMap.find name !extra_dirs)
          with Not_found -> e
      in
      let mkdir name =
        lookup name >>= function
        | Ok _    -> err_already_exists name
        | Error _ ->
          let new_dir = get ~dir:path (`Directory, name) in
          extra_dirs := !extra_dirs |> InodeMap.add name new_dir;
          remove_conflict (Irmin.Path.String_list.rcons path name);
          ok new_dir
      in
      let remove () =
        (* FIXME: is this correct? *)
        extra_dirs := InodeMap.empty;
        conflicts := !conflicts |> PathSet.filter (fun p ->
          has_prefix ~prefix:path p
        );
        View.remove_rec view path >>= ok
      in
      Dir.read_write ~ls ~create ~mkdir ~lookup ~remove |> Inode.of_dir name in
    irmin_rw_dir []

  let transactions_ctl ~merge ~remover = function
    | "close" -> Lazy.force remover >|= fun () -> Ok ""
    | "commit" -> begin merge () >>= function
      | Ok (`Ok ()) -> Lazy.force remover >|= fun () -> Ok ""
      | Ok (`Conflict msg) -> err_conflict msg
      | Error ename -> Lwt.return (Error {Protocol_9p.Response.Err.ename; errno = None}) end
    | x -> err_unknown_cmd x

  let string_of_parents parents =
    parents |> List.map (fun h -> Store.Hash.to_hum h ^ "\n") |> String.concat ""

  let format_conflicts conflicts =
    let lines = conflicts |> PathSet.elements |> List.map (fun n -> String.concat "/" n ^ "\n") in
    Cstruct.of_string (String.concat "" lines)

  let re_newline = Str.regexp_string "\n"
  let make_instance store ~remover _name =
    let path = [] in
    let msg_file, get_msg = Fs9p_file.mutable_string "" in
    View.of_path (store "view") path >|= fun view ->
    let parents = View.parents view |> string_of_parents in
    let parents_file, get_parents = Fs9p_file.mutable_string parents in
    let conflicts = ref PathSet.empty in
    (* Commit transaction *)
    let merge () =
      match !conflicts with
      | e when not (PathSet.is_empty e) -> Lwt.return (Error "conflicts file is not empty")
      | _ ->
      let parents = get_parents () |> Str.split re_newline in
      match List.map Store.Hash.of_hum parents with
      | exception Invalid_argument msg -> Lwt.return (Error msg)
      | parents ->
      let msg =
        match get_msg () with
        | "" -> "(no commit message)"
        | x -> x in
      let store = store msg in
      View.make_head store (Store.task store) ~parents ~contents:view >>= fun head ->
      Store.merge_head store head >>= ok in
    (* Current state (will finish initialisation below) *)
    let contents = ref InodeMap.empty in
    (* Files present in both normal and merge modes *)
    let stage = rw ~conflicts ~store view in
    let add inode = InodeMap.add (Inode.basename inode) inode in
    let common = InodeMap.empty
      |> add stage
      |> add (Inode.of_file "msg" msg_file)
      |> add (Inode.of_file "parents" parents_file)
      |> add (Inode.of_file "ctl" (Fs9p_file.command (transactions_ctl ~merge ~remover)))
      |> add (Inode.of_file "origin" (Fs9p_file.static_string (Path.to_hum path))) in
    let rec normal_mode () = common
      |> add (Inode.of_file "merge" (Fs9p_file.command merge_mode))
    (* Merge mode *)
    and merge_mode commit_id =
      (* Check hash is valid *)
      let store = store "merge" in
      let repo = Store.repo store in
      match Store.Hash.of_hum commit_id with
      | exception _ -> Lwt.return (error "Invalid commit ID %S" commit_id)
      | their_commit ->
      Store.Private.Commit.mem (Store.Private.Repo.commit_t repo) their_commit >>= function
      | false -> err_enoent
      | true ->
      let unit_task () = Irmin.Task.empty in
      Store.of_commit_id unit_task their_commit repo >>= fun theirs ->
      let theirs = theirs () in
      (* Add to parents *)
      let data = Cstruct.of_string (commit_id ^ "\n") in
      Fs9p_file.size parents_file >>*= fun size ->
      Fs9p_file.open_ parents_file >>*= fun fd ->
      Fs9p_file.write fd ~offset:size data >>*= fun () ->
      (* Grab current "rw" dir as "ours" *)
      View.make_head store (Store.task store) ~parents:(View.parents view) ~contents:view >>= fun our_commit ->
      Store.of_commit_id unit_task our_commit repo >>= fun ours ->
      let ours = ours () in
      let ours_ro = ro ~name:"ours" ours in
      let theirs_ro = ro ~name:"theirs" theirs in
      begin Store.lcas_head ours ~n:1 their_commit >>= function
      | `Max_depth_reached | `Too_many_lcas -> assert false
      | `Ok [] -> Lwt.return (None, Inode.of_dir "base" (Dir.fixed []))
      | `Ok (base::_) ->
          Store.of_commit_id unit_task base repo >|= fun s ->
          let s = s () in
          (Some s, ro ~name:"base" s)
      end >>= fun (base, base_ro) ->
      Merge.merge ~ours ~theirs ~base view >>= fun merge_conflicts ->
      conflicts := PathSet.union !conflicts merge_conflicts;
      let conflicts_file = Fs9p_file.read_only ~read:(fun () -> Lwt.return (Ok (Some (format_conflicts !conflicts)))) in
      contents := common
        |> add (Inode.of_file "merge" (Fs9p_file.command merge_mode))
        |> add (Inode.of_file "conflicts" conflicts_file)
        |> add ours_ro
        |> add base_ro
        |> add theirs_ro;
      Lwt.return (Ok "ok") in
    contents := normal_mode ();
    Ok (Dir.of_map_ref contents)

  let static_dir name items = Inode.of_dir name (Dir.fixed items)

  (* A stream file that initially Lwt.returns [str initial]. Further
     reads call [wait_for_change_from x], where [x] is the value
     previously Lwt.returned and then Lwt.return that, until the file
     is closed. *)
  let watch_stream ~initial ~wait_for_change_from ~str : Fs9p_file.stream =
    let last_seen = ref initial in
    let data = ref (Cstruct.of_string (str initial)) in
    let read count =
      begin if Cstruct.len !data = 0 then (
          wait_for_change_from !last_seen >|= fun now ->
          last_seen := now;
          data := Cstruct.of_string (str now)
        ) else Lwt.return ()
      end >|= fun () ->
      let count = min count (Cstruct.len !data) in
      let response = Cstruct.sub !data 0 count in
        data := Cstruct.shift !data count;
        Ok (response)
    in
    let write _ = err_read_only in
    read, write

  let head_stream store initial_head =
    let cond = Lwt_condition.create () in
    let current = ref initial_head in
    let () =
      let cb _diff =
        Store.head store >|= fun commit_id ->
        if commit_id <> !current then (
          current := commit_id;
          Lwt_condition.broadcast cond ()
        ) in
      let remove_watch =
        Store.watch_head store ?init:initial_head cb in
      ignore remove_watch in (* TODO *)
    let str = function
      | None -> "\n"
      | Some hash -> Store.Hash.to_hum hash ^ "\n" in
    let rec wait_for_change_from old =
      if old <> !current then Lwt.return !current
      else (
        Lwt_condition.wait cond >>= fun () ->
        wait_for_change_from old
      ) in
    watch_stream ~initial:initial_head ~wait_for_change_from ~str

  let head_live store =
    Fs9p_file.of_stream (fun () ->
        Store.head store >|= fun initial_head ->
        head_stream store initial_head
      )

  let reflog_stream store =
    let stream, push = Lwt_stream.create () in
    let () =
      let cb = function
        | `Added x | `Updated (_, x) ->
          push (Some (Store.Hash.to_hum x ^ "\n"));
          Lwt.return ()
        | `Removed _ ->
          push (Some "\n");
          Lwt.return ()
      in
      let remove_watch = Store.watch_head store cb in
      ignore remove_watch (* TODO *)
    in
    let data = ref (Cstruct.create 0) in
    let read count =
      begin if Cstruct.len !data = 0 then (
          Lwt_stream.next stream >|= fun next ->
          data := Cstruct.of_string next
        ) else Lwt.return ()
      end >|= fun () ->
      let count = min count (Cstruct.len !data) in
      let response = Cstruct.sub !data 0 count in
      data := Cstruct.shift !data count;
      Ok (response)
    in
    let  write _ = err_read_only in
    (read, write)

  let reflog store =
    Fs9p_file.of_stream (fun () -> Lwt.return (reflog_stream store))

  let equal_ty a b =
    match a, b with
    | `None, `None -> true
    | `File a, `File b -> a = b
    | `Directory a, `Directory b -> Tree.equal a b
    | _ -> false

  let watch_tree_stream store ~path ~initial =
    let cond = Lwt_condition.create () in
    let current = ref initial in
    let () =
      let cb _diff =
        Tree.snapshot store >>= fun root ->
        Tree.node root path >|= fun node ->
        if not (equal_ty node !current) then (
          current := node;
          Lwt_condition.broadcast cond ()
        ) in
      let remove_watch =
        Store.watch_head store cb in
      ignore remove_watch in (* TODO *)
    let str = function
      | `None -> "\n"
      | `File hash -> "F-" ^ Store.Private.Contents.Key.to_hum hash ^ "\n"
      | `Directory dir ->
        match Tree.hash dir with
        | None -> "\n"
        | Some hash -> "D-" ^ Store.Private.Node.Key.to_hum hash ^ "\n" in
    let rec wait_for_change_from old =
      if not (equal_ty old !current) then Lwt.return !current
      else (
        Lwt_condition.wait cond >>= fun () ->
        wait_for_change_from old
      ) in
    watch_stream ~initial ~wait_for_change_from ~str

  let watch_tree store ~path =
    Fs9p_file.of_stream (fun () ->
        Tree.snapshot store >>= fun snapshot ->
        Tree.node snapshot path >|= fun initial ->
        watch_tree_stream store ~path ~initial
      )

  let rec watch_dir store ~path =
    let live = lazy (Inode.of_file "tree.live" (watch_tree store ~path)) in
    let cache = ref InodeMap.empty in   (* Could use a weak map here *)
    let lookup name =
      try InodeMap.find name !cache
      with Not_found ->
        let new_path = Path.rcons path name in
        let dir = watch_dir store ~path:new_path in
        let inode = Inode.of_dir (name ^ ".node") dir in
        cache := !cache |> InodeMap.add name inode;
        inode
    in
    let ls () =
      let to_inode x =
        match Path.rdecons x with
        | None -> assert false
        | Some (_, name) -> lookup name in
      Store.list store path >|= fun items ->
      Ok (Lazy.force live :: List.map to_inode items)
    in
    let lookup = function
      | "tree.live" -> ok (Lazy.force live)
      | x when Filename.check_suffix x ".node" ->
        ok (lookup (Filename.chop_suffix x ".node"))
      | _ -> err_enoent
    in
    let remove () = Fs9p_dir.err_ro in
    Dir.read_only ~ls ~lookup ~remove ()

  (* Note: can't use [Store.fast_forward_head] because it can sometimes return [false] on success
     (when already up-to-date). *)
  let fast_forward store commit_id =
    let store = store "Fast-forward" in
    Store.head store >>= fun old_head ->
    let do_ff () =
      Store.compare_and_set_head store ~test:old_head ~set:(Some commit_id) >|= function
      | true -> `Ok
      | false -> `Not_fast_forward in   (* (concurrent update) *)
    match old_head with
    | None -> do_ff ()
    | Some expected ->
      Store.lcas_head store commit_id >>= function
      | `Ok lcas ->
        if List.mem expected lcas then do_ff ()
        else Lwt.return `Not_fast_forward
      (* These shouldn't happen, because we didn't set any limits *)
      | `Max_depth_reached | `Too_many_lcas -> assert false

  let fast_forward_merge store =
    Fs9p_file.command @@ fun hash ->
    match Store.Hash.of_hum hash with
    | exception ex ->
      Lwt.return (error "invalid-hash %S: %s" hash (Printexc.to_string ex))
    | hash ->
        fast_forward store hash >|= function
        | `Ok -> Ok ""
        | `Not_fast_forward -> error "not-fast-forward"

  let branch make_task ~remove repo name =
    let name = ref name in
    let remove () = remove !name in
    let make_contents name =
      Store.of_branch_id make_task name repo >|= fun store -> [
        ro ~name:"ro" (store "ro");
        Inode.of_dir "transactions" (Dir.directories (make_instance store) []);
        Inode.of_dir "watch" (watch_dir ~path:[] (store "watch"));
        Inode.of_file "head.live" (head_live (store "watch"));
        Inode.of_file "fast-forward" (fast_forward_merge store);
        Inode.of_file "reflog" (reflog (store "watch"));
        Inode.of_file "head" (Fs9p_file.status (fun () ->
            Store.head (store "head") >|= function
            | None -> "\n"
            | Some head -> Store.Hash.to_hum head ^ "\n"
          ));
      ] in
    let contents = ref (make_contents !name) in
    let ls () = !contents >|= fun contents -> Ok contents in
    let lookup name =
      !contents >|= fun items ->
      let rec aux = function
        | [] -> Fs9p_error.enoent
        | x :: _ when Inode.basename x = name -> Ok x
        | _ :: xs -> aux xs in
      aux items
    in
    let i = Dir.read_only ~ls ~lookup ~remove () |> Inode.of_dir !name in
    let renamed new_name =
      name := new_name;
      contents := make_contents new_name in
    (i, renamed)

  module StringSet = Set.Make(String)

  let branch_dir make_task repo =
    let cache = ref InodeMap.empty in
    let remove name =
      Store.Repo.remove_branch repo name >|= fun () ->
      cache := !cache |> InodeMap.remove name;
      Ok () in
    let get_via_cache name =
      try InodeMap.find name !cache
      with Not_found ->
        let entry = branch ~remove make_task repo name in
        cache := !cache |> InodeMap.add name entry;
        entry
    in
    let ls () =
      Store.Repo.branches repo >|= fun names ->
      let names =
        StringSet.of_list names
        |> StringSet.union (InodeMap.bindings !cache |> List.map fst |> StringSet.of_list)
        |> StringSet.elements in
      Ok (names |> List.map (fun n -> fst (get_via_cache n)))
    in
    let lookup name =
      try ok (InodeMap.find name !cache |> fst)
      with Not_found ->
        Store.Private.Ref.mem (Store.Private.Repo.ref_t repo) name >|= function
        | true  -> Ok (get_via_cache name |> fst)
        | false -> Fs9p_error.enoent
    in
    let mkdir name = ok (get_via_cache name |> fst) in
    let remove () = Fs9p_dir.err_ro in
    let rename inode new_name =
      (* TODO: some races here... *)
      let old_name = Inode.basename inode in
      let refs = Store.Private.Repo.ref_t repo in
      Store.Private.Ref.mem refs new_name >>= function
      | true -> err_eisdir
      | false ->
      Store.Private.Ref.read refs old_name >>= fun head ->
      begin match head with
      | None -> Lwt.return ()
      | Some head -> Store.Private.Ref.update refs new_name head end
      >>= fun () ->
      Store.Private.Ref.remove refs old_name >>= fun () ->
      let entry = InodeMap.find old_name !cache in
      snd entry new_name;
      cache := !cache
        |> InodeMap.remove old_name
        |> InodeMap.add new_name entry;
      Lwt.return (Ok ()) in
    Dir.dir_only ~ls ~lookup ~mkdir ~remove ~rename ()

  (* /trees *)

  let tree_hash_of_hum h =
    try
      if h = "" then Ok `None
      else match String.sub h 0 2, String.sub h 2 (String.length h - 2) with
        | "F-", hash -> Ok (`File (String.trim hash |> Store.Private.Contents.Key.of_hum))
        | "D-", hash -> Ok (`Dir (String.trim hash |> Store.Private.Node.Key.of_hum))
        | _ -> Fs9p_error.enoent
    with _ex ->
      Fs9p_error.enoent

  let trees_dir _make_task repo =
    let inode_of_tree_hash name =
      Lwt.return (tree_hash_of_hum name) >>*= function
      | `File hash ->
        begin
          Store.Private.Contents.read (Store.Private.Repo.contents_t repo) hash
          >|= function
          | Some data ->
            Ok (Fs9p_file.static (Cstruct.of_string data) |> Inode.of_file name)
          | None -> Fs9p_error.enoent
        end
      | `None ->
        let root = Tree.of_dir_hash repo None in
        ok (ro_tree ~name:"ro" ~get_root:(fun () -> Lwt.return root))
      | `Dir hash ->
        let root = Tree.of_dir_hash repo (Some hash) in
        ok (ro_tree ~name:"ro" ~get_root:(fun () -> Lwt.return root))
    in
    let cache = ref InodeMap.empty in   (* Could use a weak map here *)
    let ls () = ok [] in
    let lookup name =
      try ok (InodeMap.find name !cache)
      with Not_found ->
        inode_of_tree_hash name >>*= fun inode ->
        cache := !cache |> InodeMap.add name inode;
        ok inode
    in
    let remove () = Fs9p_dir.err_ro in
    Dir.read_only ~ls ~lookup ~remove ()

  (* /snapshots *)

  let parents_file store =
    let read () =
      let store = store "parents" in
      begin Store.head store >>= function
      | None -> Lwt.return []
      | Some head -> Store.history store ~depth:1 >|= fun hist ->
          Store.History.pred hist head
      end >|= fun parents ->
      Ok (Some (Cstruct.of_string (string_of_parents parents))) in
    Fs9p_file.read_only ~read

  let snapshot_dir store name =
    let open Inode in
    static_dir name [
      ro ~name:"ro" (store "ro");
      of_file "hash" (Fs9p_file.static_string name);
      of_file "parents" (parents_file store)
    ]

  let snapshots_dir make_task repo =
    let cache = ref InodeMap.empty in   (* Could use a weak map here *)
    let ls () = ok [] in
    let lookup name =
      try ok (InodeMap.find name !cache)
      with Not_found ->
        Lwt.return (
          try Ok (Store.Hash.of_hum name)
          with _ex -> err_invalid_commit_id name
        ) >>*= fun commit_id ->
        Store.Private.Commit.mem (Store.Private.Repo.commit_t repo) commit_id >>= function
        | false -> err_enoent
        | true ->
          Store.of_commit_id make_task commit_id repo >|= fun store ->
          let inode = snapshot_dir store name in
          cache := !cache |> InodeMap.add name inode;
          Ok inode
    in
    let remove () = Fs9p_dir.err_ro in
    Dir.read_only ~ls ~lookup ~remove ()

  let create make_task repo =
    Dir.fixed Inode.[
      of_dir "branch" (branch_dir make_task repo);
      of_dir "trees" (trees_dir make_task repo);
      of_dir "snapshots" (snapshots_dir make_task repo);
    ]

end
