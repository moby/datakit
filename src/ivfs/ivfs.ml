open Astring
open Rresult
open Lwt.Infix

module PathSet = Ivfs_merge.PathSet

(* FIXME: remove 9p from the module name! *)
let src = Logs.Src.create "ivfs" ~doc:"Irmin to VFS"
module Log = (val Logs.src_log src: Logs.LOG)

module type S = sig
  type repo
  val create: string Irmin.Task.f -> repo -> Vfs.Dir.t
end

let ( >>*= ) x f =
  x >>= function
  | Ok y -> f y
  | Error _ as e -> Lwt.return e

let ok = Vfs.ok
let err_no_entry = Lwt.return Vfs.Error.no_entry
let err_is_dir = Lwt.return Vfs.Error.is_dir
let err_not_dir = Lwt.return Vfs.Error.not_dir
let err_read_only = Lwt.return Vfs.Error.read_only_file
let err_conflict msg = Vfs.error "Merge conflict: %s" msg
let err_unknown_cmd x = Vfs.error "Unknown command %S" x
let err_invalid_commit_id id = Vfs.error "Invalid commit ID %S" id
let err_invalid_hash h x =
  Vfs.error "invalid-hash %S: %s" h (Printexc.to_string x)
let err_not_fast_forward = Vfs.error "not-fast-forward"

module Make (Store : Ivfs_tree.STORE) = struct

  type repo = Store.Repo.t
  let empty_inode_map: Vfs.Inode.t String.Map.t = String.Map.empty

  module Path = Irmin.Path.String_list
  module Tree = Ivfs_tree.Make(Store)
  module RW = Ivfs_rw.Make(Tree)
  module Merge = Ivfs_merge.Make(Store)(RW)
  module Remote = Ivfs_remote.Make(Store)

  let empty_file = Ivfs_blob.empty

  let stat path root =
    Tree.Dir.lookup_path root path >>= function
    | `None         -> err_no_entry
    | `Directory _  -> err_is_dir
    | `File (f, (`Normal | `Exec as perm)) ->
        Tree.File.size f >|= fun length ->
        Ok {Vfs.length; perm}
    | `File (f, `Link) ->
        Tree.File.content f >>= fun target ->
        Tree.File.size f >|= fun length ->
        Ok {Vfs.length; perm = `Link (Ivfs_blob.to_string target)}

  let irmin_ro_file ~get_root path =
    let read () =
      get_root () >>= fun root ->
      Tree.Dir.lookup_path root path >>= function
      | `None         -> Lwt.return (Ok None)
      | `Directory _  -> err_is_dir
      | `File (f, _perm) ->
          Tree.File.content f >|= fun content ->
          Ok (Some (Ivfs_blob.to_ro_cstruct content))
    in
    let stat () = get_root () >>= stat path in
    Vfs.File.of_kvro ~read ~stat

  let irmin_rw_file ~remove_conflict ~view path =
    match Irmin.Path.String_list.rdecons path with
    | None -> assert false
    | Some (dir, leaf) ->
    let blob () =
      let root = RW.root view in
      Tree.Dir.lookup_path root path >>= function
      | `None         -> Lwt.return (Ok None)
      | `Directory _  -> err_is_dir
      | `File (f, _perm) -> Tree.File.content f >|= fun b -> Ok (Some b)
    in
    let blob_or_empty () =
      blob () >>*= function
      | None -> ok Ivfs_blob.empty
      | Some b -> ok b in
    let remove () =
      RW.remove view dir leaf >>= function
      | Error `Not_a_directory -> err_not_dir
      | Ok () ->
      remove_conflict path; Lwt.return (Ok ())
    in
    let stat () = RW.root view |> stat path in
    let chmod perm =
      RW.chmod view dir leaf perm >>= function
      | Error `Is_a_directory -> err_is_dir
      | Error `Not_a_directory -> err_not_dir
      | Error `No_such_item -> err_no_entry
      | Ok () -> Lwt.return (Ok ())
    in
    let update b =
      RW.update view dir leaf (b, `Keep) >>= function
      | Error `Is_a_directory -> err_is_dir
      | Error `Not_a_directory -> err_not_dir
      | Ok () ->
      remove_conflict path;
      Lwt.return (Ok ())
    in
    let open_ () =
      let read ~offset ~count =
        blob () >>*= function
        | None -> err_no_entry
        | Some b ->
          Lwt.return (Ivfs_blob.read ~offset ~count b)
      and write ~offset data =
        blob_or_empty () >>*= fun b ->
        Lwt.return (Ivfs_blob.write b ~offset data) >>*= update
      in
      ok @@ Vfs.File.create_fd ~read ~write
    in
    let truncate len =
      blob_or_empty () >>*= fun b ->
      Lwt.return (Ivfs_blob.truncate b len) >>*= update
    in
    Vfs.File.create ~stat ~open_ ~truncate ~remove ~chmod

  let name_of_irmin_path ~root path =
    match Path.rdecons path with
    | None -> root
    | Some (_, leaf) -> leaf

  let ro_tree ~name ~get_root =
    let name_of_irmin_path = name_of_irmin_path ~root:name in
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
      | `Directory -> irmin_ro_dir full_path
      | `File      ->
        let path = name_of_irmin_path full_path in
        let file = irmin_ro_file ~get_root full_path in
        Vfs.Inode.file path file

    and irmin_ro_dir path =
      let name = name_of_irmin_path path in
      let ls () =
        get_root () >>= fun root ->
        Tree.Dir.get root path >>= function
        | None -> err_no_entry
        | Some dir ->
          Tree.Dir.ls dir >>= fun items ->
          ok (List.map (get ~dir:path) items)
      in
      let lookup name =
        get_root () >>= fun root ->
        Tree.Dir.get root path >>= function
        | None -> err_no_entry
        | Some dir ->
          Tree.Dir.ty dir name >>= function
          | `File
          | `Directory as ty -> ok (get ~dir:path (ty, name))
          | `None            -> err_no_entry
      in
      let remove () = Vfs.Dir.err_read_only in
      Vfs.Dir.read_only ~ls ~lookup ~remove |> Vfs.Inode.dir name
    in
    irmin_ro_dir []

  let read_only store = ro_tree ~get_root:(fun () -> Tree.snapshot store)

  let remove_shadowed_by items map =
    List.fold_left (fun acc (_, name) ->
        String.Map.remove name acc
      ) map items

  let rec has_prefix ~prefix p =
    match prefix, p with
    | [], _ -> true
    | pre::pres, p::ps ->
      if pre = p then has_prefix ~prefix:pres ps
      else false
    | _, [] -> false

  (* Note: writing to a path removes it from [conflicts], if present *)
  let rw ~conflicts view =
    let remove_conflict path = conflicts := PathSet.remove path !conflicts in
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
        Vfs.Inode.file path file

    and irmin_rw_dir path =
      let name = name_of_irmin_path path in
      (* Irmin doesn't store empty directories, so we need to store
         that list in the server's memory. Meet [extra_dirs]. *)
      let extra_dirs = ref empty_inode_map in
      let ls () =
        let root = RW.root view in
        begin Tree.Dir.get root path >>= function
          | None     -> Lwt.return []   (* in parent's extra_dirs? *)
          | Some dir -> Tree.Dir.ls dir
        end >>= fun items ->
        extra_dirs := remove_shadowed_by items !extra_dirs;
        let extra_inodes = String.Map.bindings !extra_dirs |> List.map snd in
        ok (extra_inodes @ List.map (get ~dir:path) items)
      in
      let mkfile name perm =
        begin match perm with
        | `Normal | `Exec as perm -> RW.update view path name (empty_file, perm)
        | `Link target -> RW.update view path name (Ivfs_blob.of_string target, `Link)
        end >>= function
        | Error `Not_a_directory -> err_not_dir
        | Error `Is_a_directory -> err_is_dir
        | Ok () ->
        let new_path = Path.rcons path name in
        remove_conflict new_path;
        Lwt.return (Ok (get ~dir:path (`File, name)))
      in
      let lookup name =
        let real_result =
          let snapshot = RW.root view in
          Tree.Dir.get snapshot path >>= function
          | None -> err_no_entry
          | Some dir ->
            Tree.Dir.ty dir name >>= function
            | `File
            | `Directory as ty -> ok (get ~dir:path (ty, name))
            | `None            -> err_no_entry
        in
        real_result >|= function
        | Ok _ as ok   -> ok
        | Error _ as e ->
          match String.Map.find name !extra_dirs with
          | Some x -> Ok x
          | None   -> e
      in
      let mkdir name =
        lookup name >>= function
        | Ok _    -> Vfs.Dir.err_already_exists
        | Error _ ->
          let new_dir = get ~dir:path (`Directory, name) in
          extra_dirs := String.Map.add name new_dir !extra_dirs;
          remove_conflict (Irmin.Path.String_list.rcons path name);
          ok new_dir
      in
      let remove () =
        match Irmin.Path.String_list.rdecons path with
        | None -> err_read_only
        | Some (dir, leaf) ->
        (* FIXME: is this correct? *)
        RW.remove view dir leaf >>= function
        | Error `Not_a_directory -> err_not_dir
        | Ok () ->
        extra_dirs := String.Map.empty;
        conflicts := PathSet.filter (has_prefix ~prefix:path) !conflicts;
        Lwt.return (Ok ())
      in
      let rename inode new_name =
        (* TODO: Probably some races here.
           What if inode was previously deleted?
           What if there are two renames at once?  *)
        let old_name = Vfs.Inode.basename inode in
        let in_extras = String.Map.mem old_name !extra_dirs in
        if in_extras then extra_dirs := String.Map.remove old_name !extra_dirs;
        RW.rename view path ~old_name ~new_name >>= function
        | Error `Is_a_directory -> err_is_dir
        | Error `No_such_item when in_extras ->
          extra_dirs := String.Map.add new_name inode !extra_dirs;
          ok ()
        | Error `No_such_item -> err_no_entry
        | Error `Not_a_directory -> err_not_dir
        | Ok () ->
            Vfs.Inode.set_basename inode new_name;
            ok () in
      Vfs.Dir.create ~ls ~mkfile ~mkdir ~lookup ~remove ~rename |>
      Vfs.Inode.dir name
    in
    irmin_rw_dir []

  let transactions_ctl ~merge ~remover = function
    | "close" -> Lazy.force remover >|= fun () -> Ok ""
    | "commit" ->
      begin merge () >>= function
        | Ok (`Ok ())        -> Lazy.force remover >|= fun () -> Ok ""
        | Ok (`Conflict msg) -> err_conflict msg
        | Error ename        -> Vfs.error "%s" ename
      end
    | x -> err_unknown_cmd x

  let string_of_parents parents =
    parents
    |> List.map (fun h -> Store.Hash.to_hum h ^ "\n")
    |> String.concat ~sep:""

  let format_conflicts conflicts =
    let lines =
      conflicts
      |> PathSet.elements
      |> List.map (fun n -> String.concat ~sep:"/" n ^ "\n")
    in
    Cstruct.of_string (String.concat ~sep:"" lines)

  let make_commit root task ~parents =
    let repo = Tree.Dir.repo root in
    Tree.Dir.hash root >>= fun node ->
    let commit = Store.Private.Commit.Val.create task ~parents ~node in
    Store.Private.Commit.add (Store.Private.Repo.commit_t repo) commit

  let make_instance store ~remover _name =
    let path = [] in
    let msg_file, get_msg = Vfs.File.rw_of_string "" in
    let store1 = store "snapshot" in
    let repo = Store.repo store1 in
    Store.head store1 >>= fun orig_head ->
    begin match orig_head with
    | None -> Lwt.return (Tree.Dir.empty repo, [])
    | Some commit_id ->
        Store.Private.Commit.read_exn (Store.Private.Repo.commit_t repo) commit_id >|= fun commit ->
        Tree.Dir.of_hash repo (Store.Private.Commit.Val.node commit),
        [commit_id]
    end >>= fun (orig_root, parents) ->
    let view = RW.of_dir orig_root in
    let parents = string_of_parents parents in
    let parents_file, get_parents = Vfs.File.rw_of_string parents in
    let conflicts = ref PathSet.empty in
    (* Make a commit based on "rw", "parents" and "msg" *)
    let commit_of_view () =
      match !conflicts with
      | e when not (PathSet.is_empty e) ->
        Lwt.return (Error "conflicts file is not empty")
      | _ ->
        let parents = String.cuts ~empty:false ~sep:"\n" (get_parents ()) in
        match List.map Store.Hash.of_hum parents with
        | exception Invalid_argument msg -> Lwt.return (Error msg)
        | parents ->
          let msg = match get_msg () with
            | "" -> "(no commit message)"
            | x -> x
          in
          let store = store msg in
          let root = RW.root view in
          make_commit root (Store.task store) ~parents >|= fun c ->
          Ok (c, "Merge", parents)
    in
    (* Commit transaction *)
    let merge () =
      commit_of_view () >>*= fun (head, msg, _parents) ->
      Store.merge_head (store msg) head >|= fun x -> Ok x
    in
    (* Current state (will finish initialisation below) *)
    let contents = ref empty_inode_map in
    (* Files present in both normal and merge modes *)
    let stage = rw ~conflicts view in
    let add inode = String.Map.add (Vfs.Inode.basename inode) inode in
    let ctl = Vfs.File.command (transactions_ctl ~merge ~remover) in
    let origin = Vfs.File.ro_of_string (Path.to_hum path) in
    let common =
      empty_inode_map
      |> add stage
      |> add (Vfs.Inode.file "msg"     msg_file)
      |> add (Vfs.Inode.file "parents" parents_file)
      |> add (Vfs.Inode.file "ctl"     ctl)
      |> add (Vfs.Inode.file "origin"  origin)
    in
    let rec normal_mode () =
      common
      |> add (Vfs.Inode.file "merge" (Vfs.File.command merge_mode))

    (* Merge mode *)
    and merge_mode commit_id =
      (* Check hash is valid *)
      let store = store "merge" in
      let repo = Store.repo store in
      match Store.Hash.of_hum commit_id with
      | exception _  -> err_invalid_commit_id commit_id
      | their_commit ->
        Store.Private.Commit.mem (Store.Private.Repo.commit_t repo) their_commit
        >>= function
        | false -> err_no_entry
        | true ->
          let unit_task () = Irmin.Task.empty in
          Store.of_commit_id unit_task their_commit repo >>= fun theirs ->
          let theirs = theirs () in
          (* Grab current "rw" dir as "ours" *)
          commit_of_view () >>= function
          | Error e -> Vfs.error "Can't start merge: %s" e
          | Ok (our_commit, _msg, our_parents) ->
          Store.of_commit_id unit_task our_commit repo >>= fun ours ->
          let ours = ours () in
          let ours_ro = read_only ~name:"ours" ours in
          let theirs_ro = read_only ~name:"theirs" theirs in
          begin match our_parents with
          | [] ->
              (* Optimisation: if our new commit has no parents then we know there
                 can be no LCA, so avoid searching (which would be slow, since Irmin
                 would have to explore the entire history to check). *)
              Lwt.return (None, Vfs.Inode.dir "base" Vfs.Dir.empty)
          | _ ->
              Store.lcas_head ours ~n:1 their_commit >>= function
                | `Max_depth_reached | `Too_many_lcas -> assert false
                | `Ok [] -> Lwt.return (None, Vfs.Inode.dir "base" Vfs.Dir.empty)
                | `Ok (base::_) ->
                  Store.of_commit_id unit_task base repo >|= fun s ->
                  let s = s () in
                  (Some s, read_only ~name:"base" s)
          end >>= fun (base, base_ro) ->
          (* Add to parents *)
          let data = Cstruct.of_string (commit_id ^ "\n") in
          Vfs.File.size parents_file >>*= fun size ->
          Vfs.File.open_ parents_file >>*= fun fd ->
          Vfs.File.write fd ~offset:size data >>*= fun () ->
          (* Do the merge *)
          Merge.merge ~ours ~theirs ~base view >>= fun merge_conflicts ->
          conflicts := PathSet.union !conflicts merge_conflicts;
          let conflicts_file =
            let read () = ok (Some (format_conflicts !conflicts)) in
            Vfs.File.of_kvro ~read ~stat:(Vfs.File.stat_of ~read)
          in
          contents :=
            common
            |> add (Vfs.Inode.file "merge" (Vfs.File.command merge_mode))
            |> add (Vfs.Inode.file "conflicts" conflicts_file)
            |> add ours_ro
            |> add base_ro
            |> add theirs_ro;
          Lwt.return (Ok "ok")
    in
    contents := normal_mode ();
    Lwt.return (Ok (Vfs.Dir.of_map_ref contents))

  let static_dir name items = Vfs.Inode.dir name (Vfs.Dir.of_list items)

  let head_stream store initial_head =
    let session = Vfs.File.Stream.session initial_head in
    let remove_watch =
      let cb _ = Store.head store >|= Vfs.File.Stream.publish session in
      Store.watch_head store ?init:initial_head cb
    in
    let pp ppf = function
      | None      -> Fmt.string ppf "\n"
      | Some hash -> Fmt.string ppf (Store.Hash.to_hum hash ^ "\n")
    in
    ignore remove_watch; (* TODO *)
    Vfs.File.Stream.create pp session

  let head_live store =
    Vfs.File.of_stream (fun () ->
        Store.head store >|= fun initial_head ->
        head_stream store initial_head
      )

  let reflog_stream store =
    let session = Vfs.File.Stream.session None in
    let remove_watch =
      let cb = function
        | `Added x | `Updated (_, x) -> Vfs.File.Stream.publish session (Some x)
        | `Removed _                 -> Vfs.File.Stream.publish session None
      in
      Store.watch_head store (fun x -> Lwt.return @@ cb x)
    in
    let pp ppf = function
      | Some x -> Fmt.pf ppf "%s\n" (Store.Hash.to_hum x)
      | None   -> Fmt.string ppf "\n"
    in
    ignore remove_watch; (* TODO *)
    Vfs.File.Stream.create pp session

  let reflog store =
    Vfs.File.of_stream (fun () -> Lwt.return (reflog_stream store))

  let hash_line store path =
    Tree.snapshot store >>= fun snapshot ->
    Tree.Dir.lookup_path snapshot path >>= function
    | `None              -> Lwt.return "\n"
    | `File (f, `Normal) -> Tree.File.hash f >|= Fmt.strf "F-%a\n" Tree.File.pp_hash
    | `File (f, `Exec)   -> Tree.File.hash f >|= Fmt.strf "X-%a\n" Tree.File.pp_hash
    | `File (f, `Link)   -> Tree.File.hash f >|= Fmt.strf "L-%a\n" Tree.File.pp_hash
    | `Directory dir ->
      Tree.Dir.hash dir >|= fun h ->
      Fmt.strf "D-%s\n" @@ Store.Private.Node.Key.to_hum h

  let watch_tree_stream store ~path ~init =
    let session = Vfs.File.Stream.session init in
    let current = ref init in
    let remove_watch =
      let cb _ =
        hash_line store path >|= fun line ->
        if line <> !current then (
          current := line;
          Vfs.File.Stream.publish session !current
        ) in
      Store.watch_head store cb
    in
    ignore remove_watch; (* TODO *)
    Vfs.File.Stream.create Fmt.string session

  let watch_tree store ~path =
    Vfs.File.of_stream (fun () ->
        hash_line store path >|= fun init ->
        watch_tree_stream store ~path ~init
      )

  let rec watch_dir store ~path =
    let live = lazy (Vfs.Inode.file "tree.live" (watch_tree store ~path)) in
    let cache = ref empty_inode_map in   (* Could use a weak map here *)
    let lookup name = match String.Map.find name !cache with
      | Some x -> x
      | None   ->
        let new_path = Path.rcons path name in
        let dir = watch_dir store ~path:new_path in
        let inode = Vfs.Inode.dir (name ^ ".node") dir in
        cache := String.Map.add name inode !cache;
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
      | _ -> err_no_entry
    in
    let remove () = Vfs.Dir.err_read_only in
    Vfs.Dir.read_only ~ls ~lookup ~remove

  (* Note: can't use [Store.fast_forward_head] because it can
     sometimes return [false] on success (when already up-to-date). *)
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
    Vfs.File.command (fun hash ->
        match Store.Hash.of_hum hash with
        | exception ex -> err_invalid_hash hash ex
        | hash         ->
          let commit_t = Store.Private.Repo.commit_t (Store.repo (store "commit_t")) in
          Store.Private.Commit.mem commit_t hash >>= function
          | false -> Vfs.error "Commit not in store"
          | true ->
            fast_forward store hash >>= function
            | `Ok               -> ok ""
            | `Not_fast_forward -> err_not_fast_forward
      )

  let status store () =
    Store.head (store "head") >|= function
    | None      -> "\n"
    | Some head -> Store.Hash.to_hum head ^ "\n"

  let transactions store =
    let lock = Lwt_mutex.create () in
    let items = ref String.Map.empty in
    let ls () = ok (String.Map.bindings !items |> List.map snd) in
    let lookup name = match String.Map.find name !items with
      | Some x -> ok x
      | None   -> err_no_entry
    in
    let make = make_instance store in
    let remover name =
      lazy (
        Lwt_mutex.with_lock lock (fun () ->
            items := String.Map.remove name !items;
            Lwt.return_unit
          ))
    in
    let mkdir name =
      Lwt_mutex.with_lock lock (fun () ->
          if String.Map.mem name !items then Vfs.Dir.err_already_exists
          else (
            let remover = remover name in
            make ~remover name >>= function
            | Error _ as e -> Lwt.return e
            | Ok dir       ->
              if Lazy.is_val remover then err_no_entry else (
                let inode = Vfs.Inode.dir name dir in
                items := String.Map.add name inode !items;
                ok inode
              )))
    in
    let mkfile _ _ = Vfs.Dir.err_dir_only in
    let rename _ _ = Vfs.Dir.err_read_only in   (* TODO *)
    let remove _ = Vfs.Dir.err_read_only in
    Vfs.Dir.create ~ls ~mkfile ~mkdir ~lookup ~remove ~rename

  let branch make_task ~remove repo name =
    let name = ref name in
    let remove () = remove !name in
    let make_contents name =
      Store.of_branch_id make_task name repo >|= fun store -> [
        read_only ~name:"ro" (store "ro");
        Vfs.Inode.dir  "transactions" (transactions store);
        Vfs.Inode.dir  "watch"        (watch_dir ~path:[] @@ store "watch");
        Vfs.Inode.file "head.live"    (head_live @@ store "watch");
        Vfs.Inode.file "fast-forward" (fast_forward_merge store);
        Vfs.Inode.file "reflog"       (reflog @@ store "watch");
        Vfs.Inode.file "head"         (Vfs.File.status (status store));
      ] in
    let contents = ref (make_contents !name) in
    let ls () = !contents >|= fun contents -> Ok contents in
    let lookup name =
      !contents >|= fun items ->
      let rec aux = function
        | [] -> Vfs.Error.no_entry
        | x :: _ when Vfs.Inode.basename x = name -> Ok x
        | _ :: xs -> aux xs in
      aux items
    in
    let i = Vfs.Dir.read_only ~ls ~lookup ~remove |> Vfs.Inode.dir !name in
    let renamed new_name =
      Vfs.Inode.set_basename i new_name;
      name := new_name;
      contents := make_contents new_name
    in
    (i, renamed)

  module StringSet = Set.Make(String)

  let branch_dir make_task repo =
    let cache = ref String.Map.empty in
    let remove name =
      Store.Repo.remove_branch repo name >|= fun () ->
      cache := String.Map.remove name !cache;
      Ok () in
    let get_via_cache name = match String.Map.find name !cache with
      | Some x -> x
      | None   ->
        let entry = branch ~remove make_task repo name in
        cache :=  String.Map.add name entry !cache;
        entry
    in
    let ls () =
      Store.Repo.branches repo >|= fun names ->
      let names =
        let names = StringSet.of_list names in
        String.Map.bindings !cache
        |> List.map fst
        |> StringSet.of_list
        |> StringSet.union names
        |> StringSet.elements
      in
      Ok (List.map (fun n -> fst (get_via_cache n)) names)
    in
    let lookup name = match String.Map.find name !cache with
      | Some (x, _) -> ok x
      | None        ->
        Store.Private.Ref.mem (Store.Private.Repo.ref_t repo) name >>= function
        | true  -> ok (get_via_cache name |> fst)
        | false -> err_no_entry
    in
    let mkdir name = ok (get_via_cache name |> fst) in
    let remove () = Vfs.Dir.err_read_only in
    let rename inode new_name =
      (* TODO: some races here... *)
      let old_name = Vfs.Inode.basename inode in
      let refs = Store.Private.Repo.ref_t repo in
      Store.Private.Ref.mem refs new_name >>= function
      | true -> err_is_dir
      | false ->
        Store.Private.Ref.read refs old_name >>= fun head ->
        begin match head with
          | None      -> Lwt.return_unit
          | Some head -> Store.Private.Ref.update refs new_name head end
        >>= fun () ->
        Store.Private.Ref.remove refs old_name >>= fun () ->
        match String.Map.find old_name !cache with
        | None      -> err_no_entry
        | Some entry ->
          snd entry new_name;
          cache :=
            !cache
            |> String.Map.remove old_name
            |> String.Map.add new_name entry;
          ok ()
    in
    Vfs.Dir.dir_only ~ls ~lookup ~mkdir ~remove ~rename

  (* /trees *)

  let tree_hash_of_hum h =
    let file ty h = `File (ty, String.trim h |> Store.Private.Contents.Key.of_hum) in
    let dir h = `Dir (String.trim h |> Store.Private.Node.Key.of_hum) in
    try
      match String.span ~min:2 ~max:2 h with
        | "F-", hash -> Ok (file `Normal hash)
        | "X-", hash -> Ok (file `Exec hash)
        | "L-", hash -> Ok (file `Link hash)
        | "D-", hash -> Ok (dir hash)
        | _ -> Vfs.Error.no_entry
    with _ex ->
      Vfs.Error.no_entry

  let trees_dir _make_task repo =
    let inode_of_tree_hash name =
      Lwt.return (tree_hash_of_hum name) >>*= function
      | `File (ty, hash) ->
        begin
          Store.Private.Contents.read (Store.Private.Repo.contents_t repo) hash
          >|= function
          | None      -> Vfs.Error.no_entry
          | Some data ->
            let perm =
              match ty with
              | `Normal | `Exec as perm -> perm
              | `Link -> `Link data in
            Ok (Vfs.File.ro_of_string ~perm data |> Vfs.Inode.file name)
        end
      | `Dir hash ->
        let root = Tree.Dir.of_hash repo hash in
        ok (ro_tree ~name:"ro" ~get_root:(fun () -> Lwt.return root))
    in
    let cache = ref String.Map.empty in   (* Could use a weak map here *)
    let ls () = ok [] in
    let lookup name = match String.Map.find name !cache with
      | Some x -> ok x
      | None   ->
        inode_of_tree_hash name >>*= fun inode ->
        cache := String.Map.add name inode !cache;
        ok inode
    in
    let remove () = Vfs.Dir.err_read_only in
    Vfs.Dir.read_only ~ls ~lookup ~remove

  (* /snapshots *)

  let parents_file store =
    let read () =
      begin Store.head store >>= function
        | None -> Lwt.return []
        | Some head -> Store.history store ~depth:1 >|= fun hist ->
          Store.History.pred hist head
      end >|= fun parents ->
      Ok (Some (Cstruct.of_string (string_of_parents parents))) in
    Vfs.File.of_kvro ~read ~stat:(Vfs.File.stat_of ~read)

  let msg_file store commit_id =
    let read () =
      let repo = Store.repo store in
      let id = Store.Private.Commit.Key.of_hum commit_id in
      Store.Repo.task_of_commit_id repo id >|= fun task ->
      let messages = Irmin.Task.messages task in
      let msg = (String.concat ~sep:"\n" messages) ^ "\n" in
      Ok (Some (Cstruct.of_string msg))
    in
    Vfs.File.of_kvro ~read ~stat:(Vfs.File.stat_of ~read)

  let snapshot_dir store name =
    let store = store "ro" in
    let dirs = [
      read_only ~name:"ro"     store;
      Vfs.Inode.file "hash"    (Vfs.File.ro_of_string name);
      Vfs.Inode.file "msg"     (msg_file store name);
      Vfs.Inode.file "parents" (parents_file store)
    ] in
    static_dir name (fun () -> dirs)

  let snapshots_dir make_task repo =
    let cache = ref empty_inode_map in   (* Could use a weak map here *)
    let ls () = ok [] in
    let lookup name = match String.Map.find name !cache with
      | Some x ->  ok x
      | None   ->
        begin
          try ok (Store.Hash.of_hum name)
          with _ex -> err_invalid_commit_id name
        end >>*= fun commit_id ->
        Store.Private.Commit.mem (Store.Private.Repo.commit_t repo) commit_id
        >>= function
        | false -> err_no_entry
        | true  ->
          Store.of_commit_id make_task commit_id repo >|= fun store ->
          let inode = snapshot_dir store name in
          cache := String.Map.add name inode !cache;
          Ok inode
    in
    let remove () = Vfs.Dir.err_read_only in
    Vfs.Dir.read_only ~ls ~lookup ~remove

  let create make_task repo =
    let dirs = [
      Vfs.Inode.dir "branch"     (branch_dir make_task repo);
      Vfs.Inode.dir "trees"      (trees_dir make_task repo);
      Vfs.Inode.dir "snapshots"  (snapshots_dir make_task repo);
      Vfs.Inode.dir "remotes"    (Remote.create make_task repo);
    ] in
    Vfs.Dir.of_list (fun () -> dirs)

end
