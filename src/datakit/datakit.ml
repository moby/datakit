open Astring
open Rresult
open Lwt.Infix

let src = Logs.Src.create "DataKit" ~doc:"Irmin VFS for DataKit"
module Log = (val Logs.src_log src : Logs.LOG)

module Hash = Irmin.Hash.SHA1
module Path = Path
module Metadata = Metadata
module Branch = Branch
module Blob = Blob

type hash = Hash.t
type path = Path.t
type step = Path.step
type perm = Metadata.t
type branch = Branch.t
type blob = Blob.t

module type S = Store.S
module type GIT_S_MAKER = Store.GIT_S_MAKER
module Make_git = Store.Make_git

module Make (M: Irmin.S_MAKER) = M (Metadata)(Blob)(Path)(Branch)(Hash)

module type VFS = sig
  type repo
  val create: info:(string -> Irmin.Info.t) -> repo -> Vfs.Dir.t
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
let err_invalid_hash h x =
  Vfs.error "invalid-hash %S: %s" h (Printexc.to_string x)
let err_not_fast_forward = Vfs.error "not-fast-forward"

module Dir = Dir.Make

module Vfs (Store : Store.S) = struct

  type repo = Store.Repo.t
  let empty_inode_map: Vfs.Inode.t String.Map.t = String.Map.empty

  module Dir = Dir(Store)
  module Merge = Merge.Make(Store)(Dir)
  module Remote = Remote.Make(Store)

  let commit_of_commit_id repo name =
    match Store.Commit.Hash.of_string name with
    | Error (`Msg e) -> err_invalid_hash name (Failure e)
    | Ok hash        ->
      Store.Commit.of_hash repo hash >>= function
      | None      -> err_no_entry
      | Some hash -> ok hash

  let string_of_commits ids =
    ids
    |> List.map (fun h -> Fmt.strf "%a\n" Store.Commit.pp h)
    |> String.concat ~sep:""

    module CommitListFile : sig
    type t
    val make : Store.Repo.t -> Store.Commit.t list -> t
    val file : t -> Vfs.File.t
    val read : t -> (Store.Commit.t list, Vfs.Error.t) result Lwt.t
    val append : t -> Store.Commit.t -> unit Vfs.or_err
  end = struct
    type t = {
      repo : Store.Repo.t;
      file : Vfs.File.t;
      get  : unit -> string;
    }

    let make repo init =
      let init = string_of_commits init in
      let file, get = Vfs.File.rw_of_string init in
      { repo; file; get }

    let file t = t.file

    let read t =
      let lines = String.cuts ~empty:false ~sep:"\n" (t.get ()) in
      Lwt_list.fold_left_s (fun acc line ->
          match acc with
          | Error _ -> Lwt.return acc
          | Ok acc  ->
            commit_of_commit_id t.repo line >|= function
            | Error _ as e -> e
            | Ok commit    -> Ok (commit :: acc)
        ) (Ok []) (List.rev lines)

    let append t commit =
      let data =
        Fmt.kstrf (fun x -> Cstruct.of_string x) "%a\n" Store.Commit.pp commit
      in
      Vfs.File.size t.file >>*= fun size ->
      Vfs.File.open_ t.file >>*= fun fd ->
      Vfs.File.write fd ~offset:size data
  end

  module PathSetFile : sig
    type t
    val make : unit -> t
    val add_all : t -> Path.Set.t -> unit
    val read : t -> Path.Set.t
    val remove_file : t -> Store.Key.t -> unit
    val remove_subtree : t -> Store.Key.t -> unit
    val file : t -> Vfs.File.t
  end = struct
    type t = Path.Set.t ref

    let format_conflicts conflicts =
      let lines =
        conflicts
        |> Path.Set.elements
        |> List.map (fun p -> Fmt.strf "%a\n" Store.Key.pp p)
      in
      Cstruct.of_string (String.concat ~sep:"" lines)

    let make () =
      ref Path.Set.empty

    let read t = !t

    let remove_file t path =
      t := Path.Set.remove path !t

    let rec has_prefix ~prefix p =
      match Path.decons prefix, Path.decons p with
      | None, _ -> true
      | Some (pre, pres), Some (p, ps) ->
        if pre = p then has_prefix ~prefix:pres ps
        else false
      | _ -> false

    let remove_subtree t path =
      t := Path.Set.filter (has_prefix ~prefix:path) !t

    let add_all t xs =
      t := Path.Set.union !t xs

    let file t =
      let read () = ok (Some (format_conflicts !t)) in
      Vfs.File.of_kvro ~read ~stat:(Vfs.File.stat_of ~read)
  end

  let empty_file = Blob.empty

  let stat path root =
    Store.Tree.find_tree root path >>= function
    | None -> err_no_entry
    | Some (`Node _) -> err_is_dir
    | Some (`Contents (f, (`Normal | `Exec as perm))) ->
      let length = Blob.len f in
      ok {Vfs.length; perm}
    | Some (`Contents (f, `Link)) ->
      let length = Blob.len f in
      let target = Blob.to_string f in
      ok {Vfs.length; perm = `Link target}

  let irmin_ro_file ~get_root path =
    let read () =
      get_root () >>= fun root ->
      Store.Tree.find_tree root path >>= function
      | None -> Lwt.return (Ok None)
      | Some (`Node _) -> err_is_dir
      | Some (`Contents (f, _perm)) ->
        ok (Some (Blob.to_ro_cstruct f))
    in
    let stat () = get_root () >>= stat path in
    Vfs.File.of_kvro ~read ~stat

  let irmin_rw_file ~remove_conflict ~view path =
    match Path.rdecons path with
    | None -> assert false
    | Some (dir, leaf) ->
      let blob () =
        let root = Dir.root view in
        Store.Tree.find_tree root path >>= function
        | None -> Lwt.return (Ok None)
        | Some (`Node _) -> err_is_dir
        | Some (`Contents (f, _perm)) -> ok (Some f)
      in
      let blob_or_empty () =
        blob () >>*= function
        | None -> ok Blob.empty
        | Some b -> ok b in
      let remove () =
        Dir.remove view dir leaf >>= function
        | Error `Not_a_directory -> err_not_dir
        | Ok () ->
          remove_conflict path; Lwt.return (Ok ())
      in
      let stat () = Dir.root view |> stat path in
      let chmod perm =
        Dir.chmod view dir leaf perm >>= function
        | Error `Is_a_directory -> err_is_dir
        | Error `Not_a_directory -> err_not_dir
        | Error `No_such_item -> err_no_entry
        | Ok () -> Lwt.return (Ok ())
      in
      let update b =
        Dir.update view dir leaf (b, `Keep) >>= function
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
            Lwt.return (Blob.read ~offset ~count b)
        and write ~offset data =
          blob_or_empty () >>*= fun b ->
          Lwt.return (Blob.write b ~offset data) >>*= update
        in
        ok @@ Vfs.File.create_fd ~read ~write
      in
      let truncate len =
        blob_or_empty () >>*= fun b ->
        Lwt.return (Blob.truncate b len) >>*= update
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

    let rec get ~dir (leaf, ty) =
      let hash_key = (ty, Path.rcons dir leaf) in
      try Hashtbl.find nodes hash_key
      with Not_found ->
        let inode = inode_of hash_key in
        Hashtbl.add nodes hash_key inode;
        inode

    and inode_of (ty, full_path) =
      match ty with
      | `Node -> irmin_ro_dir full_path
      | `Contents      ->
        let path = name_of_irmin_path full_path in
        let file = irmin_ro_file ~get_root full_path in
        Vfs.Inode.file path file

    and irmin_ro_dir path =
      let name = name_of_irmin_path path in
      let ls () =
        get_root () >>= fun root ->
        Store.Tree.find_tree root path >>= function
        | None | Some (`Contents _) -> err_no_entry
        | Some (`Node _ as dir) ->
          Store.Tree.list dir Store.Key.empty >>= fun items ->
          ok (List.map (get ~dir:path) items)
      in
      let lookup name =
        get_root () >>= fun root ->
        Store.Tree.find_tree root path >>= function
        | None | Some (`Contents _) -> err_no_entry
        | Some (`Node _ as dir) ->
          let step = Store.Key.v [name] in
          Store.Tree.kind dir step >>= function
          | None -> err_no_entry
          | Some (`Contents | `Node as ty) -> ok (get ~dir:path (name, ty))
      in
      let remove () = Vfs.Dir.err_read_only in
      Vfs.Dir.read_only ~ls ~lookup ~remove |> Vfs.Inode.dir name
    in
    irmin_ro_dir Path.empty

  let read_only store = ro_tree ~get_root:(fun () -> Store.tree store)

  let remove_shadowed_by items map =
    List.fold_left (fun acc (name, _) ->
        String.Map.remove name acc
      ) map items

  (* Note: writing to a path removes it from [conflicts], if present *)
  let rw ~conflicts view =
    let remove_conflict = PathSetFile.remove_file conflicts in
    let name_of_irmin_path = name_of_irmin_path ~root:"rw" in
    (* Keep track of which qids we're still using. We need to give the
       same qid to the client each time. TODO: use a weak map here. *)
    let nodes = Hashtbl.create 10 in

    let rec get ~dir (leaf, ty) =
      let hash_key = (ty, Path.rcons dir leaf) in
      try Hashtbl.find nodes hash_key
      with Not_found ->
        let inode = inode_of hash_key in
        Hashtbl.add nodes hash_key inode;
        inode

    and inode_of (ty, full_path) =
      match ty with
      | `Node -> irmin_rw_dir full_path
      | `Contents ->
        let path = name_of_irmin_path full_path in
        let file = irmin_rw_file ~remove_conflict ~view full_path in
        Vfs.Inode.file path file

    and irmin_rw_dir path =
      let name = name_of_irmin_path path in
      (* Irmin doesn't store empty directories, so we need to store
         that list in the server's memory. Meet [extra_dirs]. *)
      let extra_dirs = ref empty_inode_map in
      let ls () =
        let root = Dir.root view in
        begin Store.Tree.find_tree root path >>= function
          | None | Some (`Contents _) -> Lwt.return [] (* in parent's extra_dirs? *)
          | Some (`Node _ as dir) -> Store.Tree.list dir Store.Key.empty
        end >>= fun items ->
        extra_dirs := remove_shadowed_by items !extra_dirs;
        let extra_inodes = String.Map.bindings !extra_dirs |> List.map snd in
        ok (extra_inodes @ List.map (get ~dir:path) items)
      in
      let mkfile name perm =
        begin match perm with
          | `Normal | `Exec as perm -> Dir.update view path name (empty_file, perm)
          | `Link target -> Dir.update view path name (Blob.string target, `Link)
        end >>= function
        | Error `Not_a_directory -> err_not_dir
        | Error `Is_a_directory -> err_is_dir
        | Ok () ->
          let new_path = Path.rcons path name in
          remove_conflict new_path;
          Lwt.return (Ok (get ~dir:path (name, `Contents)))
      in
      let lookup name =
        let real_result =
          let snapshot = Dir.root view in
          Store.Tree.find_tree snapshot path >>= function
          | None | Some (`Contents _) -> err_no_entry
          | Some (`Node _ as dir) ->
            let step = Store.Key.v [name] in
            Store.Tree.kind dir step >>= function
            | Some (`Contents | `Node as ty) -> ok (get ~dir:path (name, ty))
            | None -> err_no_entry
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
          let new_dir = get ~dir:path (name, `Node) in
          extra_dirs := String.Map.add name new_dir !extra_dirs;
          remove_conflict (Path.rcons path name);
          ok new_dir
      in
      let remove () =
        match Path.rdecons path with
        | None -> err_read_only
        | Some (dir, leaf) ->
          (* FIXME: is this correct? *)
          Dir.remove view dir leaf >>= function
          | Error `Not_a_directory -> err_not_dir
          | Ok () ->
            extra_dirs := String.Map.empty;
            PathSetFile.remove_subtree conflicts path;
            Lwt.return (Ok ())
      in
      let rename inode new_name =
        (* TODO: Probably some races here.
           What if inode was previously deleted?
           What if there are two renames at once?  *)
        let old_name = Vfs.Inode.basename inode in
        let in_extras = String.Map.mem old_name !extra_dirs in
        if in_extras then extra_dirs := String.Map.remove old_name !extra_dirs;
        Dir.rename view path ~old_name ~new_name >>= function
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
    irmin_rw_dir Path.empty

  module Diff : sig
    val vfs_dir : Store.Repo.t -> (unit -> Store.tree Lwt.t) -> Vfs.Dir.t
    (* [vfs_dir head] is directory that provides diffs against [head ()]. *)
  end = struct
    let pp_op ppf = function
      | `Added _   -> Fmt.string ppf "+"
      | `Removed _ -> Fmt.string ppf "-"
      | `Updated _ -> Fmt.string ppf "*"

    let pp_diff ppf (path, op) =
      let path = Fmt.to_to_string Store.Key.pp path in
      let path =
        if Filename.is_relative path then path
        else String.with_index_range ~first:1 path
      in
      Fmt.pf ppf "%a %s\n" pp_op op path

    let vfs_dir repo head =
      let lookup name =
        head () >>= fun head ->
        commit_of_commit_id repo name >>*= fun commit ->
        Store.of_commit commit >>= fun t ->
        Store.tree t >>= fun parent_t ->
        Store.Tree.diff parent_t head >>= fun diff ->
        let lines = List.map (Fmt.to_to_string pp_diff) diff in
        let file  = Vfs.File.ro_of_string (String.concat ~sep:"" lines) in
        ok (Vfs.Inode.file name file)
      in
      let remove () = Vfs.File.err_read_only in
      let ls () = ok [] in
      Vfs.Dir.read_only ~ls ~lookup ~remove
  end

  module Transaction : sig
    val make :
      Store.t -> info:(string -> Irmin.Info.t) -> remover:unit Lwt.t Lazy.t ->
      Vfs.Dir.t Lwt.t
    (* [make store ~info ~remover] is a directory for making a
       transaction on [store].  When done, it will force [remover]
       (which should remove the directory). *)
  end = struct
    type t = {
      repo : Store.Repo.t;
      store : Store.t;                 (* The branch we're going to commit on *)
      view : Dir.t;        (* The read/write directory with the current state *)
      get_msg : unit -> string;
      parents : CommitListFile.t;
      conflicts : PathSetFile.t;
    }

    let base ~our_parents ~ours ~their_commit =
      match our_parents with
      | [] ->
        (* Optimisation: if our new commit has no parents then we know there
           can be no LCA, so avoid searching (which would be slow, since Irmin
           would have to explore the entire history to check). *)
        Lwt.return None
      | _ ->
        Store.lcas_with_commit ours ~n:1 their_commit >>= function
        | Error (`Max_depth_reached | `Too_many_lcas) -> assert false
        | Ok []        -> Lwt.return None
        | Ok (base::_) -> Store.of_commit base >|= fun s -> Some s

    let snapshot store =
      Store.Head.find store >>= function
      | None    -> Lwt.return (Store.Tree.empty, [])
      | Some id -> Store.Commit.tree id >|= fun tree -> (tree, [id])

    let store_of_hash repo hash =
      commit_of_commit_id repo hash >>= function
      | Error _ as e    -> Lwt.return e
      | Ok their_commit ->
        Store.of_commit their_commit >|= fun store ->
        Ok (store, their_commit)

    let base_dir lca =
      match lca with
      | None -> Vfs.Inode.dir "base" Vfs.Dir.empty
      | Some base -> read_only ~name:"base" base

    let check_no_conflicts conflicts =
      match PathSetFile.read conflicts with
      | e when not (Path.Set.is_empty e) ->
        Vfs.error "conflicts file is not empty"
      | _ -> ok ()

    (* Make a commit based on "rw", "parents" and "msg" *)
    let commit_of_view ~info t =
      check_no_conflicts t.conflicts >>*= fun () ->
      CommitListFile.read t.parents >>*= fun parents ->
      let msg = match t.get_msg () with
        | "" -> "(no commit message)"
        | x -> x
      in
      let root = Dir.root t.view in
      Store.Commit.v t.repo ~info:(info msg) ~parents root >|= fun c ->
      Ok (c, "Merge", parents)

    (* Commit transaction *)
    let merge ~info t =
      commit_of_view ~info t >>= function
      | Error e -> Lwt.return (Error (`Vfs e))
      | Ok (head, msg, _parents) ->
        (* FIXME(samoht): why do we reuse the same commit message here? *)
        Store.Head.merge ~info:(fun () -> info msg) ~into:t.store head
        >|= function
        | Error (`Conflict _) as e -> e
        | Ok () -> Ok ()

    let transactions_ctl ~info t ~remover = function
      | "close" -> Lazy.force remover >|= fun () -> Ok ""
      | "commit" ->
        begin merge ~info t >>= function
          | Ok ()                 -> Lazy.force remover >|= fun () -> Ok ""
          | Error (`Conflict msg) -> err_conflict msg
          | Error (`Vfs err)      -> Lwt.return (Error err)
        end
      | x -> err_unknown_cmd x

    let make store ~info ~remover =
      let path = Path.empty in
      let msg_file, get_msg = Vfs.File.rw_of_string "" in
      snapshot store >>= fun (orig_root, parents) ->
      let repo = Store.repo store in
      let t = {
        repo;
        store;
        view = Dir.v repo orig_root;
        get_msg;
        parents = CommitListFile.make repo parents;
        conflicts = PathSetFile.make ();
      } in
      (* Current state (will finish initialisation below) *)
      let contents = ref empty_inode_map in
      (* Files present in both normal and merge modes *)
      let stage = rw ~conflicts:t.conflicts t.view in
      let add inode = String.Map.add (Vfs.Inode.basename inode) inode in
      let ctl = Vfs.File.command (transactions_ctl t ~info ~remover) in
      let origin = Vfs.File.ro_of_string (Fmt.to_to_string Store.Key.pp path) in
      let diff = Diff.vfs_dir repo (fun  () -> Lwt.return (Dir.root t.view)) in
      let common =
        empty_inode_map
        |> add stage
        |> add (Vfs.Inode.file "msg"     msg_file)
        |> add (Vfs.Inode.file "parents" (CommitListFile.file t.parents))
        |> add (Vfs.Inode.file "ctl"     ctl)
        |> add (Vfs.Inode.file "origin"  origin)
        |> add (Vfs.Inode.dir  "diff"    diff)
      in
      (* Merge mode *)
      let rec merge_mode commit_id =
        let repo = Store.repo store in
        store_of_hash repo commit_id >>*= fun (theirs, their_commit) ->
        (* Grab current "rw" dir as "ours" *)
        commit_of_view ~info t >>*= fun (our_commit, _msg, our_parents) ->
        Store.of_commit our_commit >>= fun ours ->
        let ours_ro = read_only ~name:"ours" ours in
        let theirs_ro = read_only ~name:"theirs" theirs in
        base ~our_parents ~ours ~their_commit >>= fun base ->
        (* Add to parents *)
        CommitListFile.append t.parents their_commit >>*= fun () ->
        (* Do the merge *)
        Merge.merge ~ours ~theirs ~base t.view >>= fun merge_conflicts ->
        PathSetFile.add_all t.conflicts merge_conflicts;
        contents :=
          common
          |> add (Vfs.Inode.file "merge" (Vfs.File.command merge_mode))
          |> add (Vfs.Inode.file "conflicts" (PathSetFile.file t.conflicts))
          |> add ours_ro
          |> add (base_dir base)
          |> add theirs_ro;
        Lwt.return (Ok "ok")
      in
      let normal_mode () =
        common
        |> add (Vfs.Inode.file "merge" (Vfs.File.command merge_mode))
      in
      contents := normal_mode ();
      Lwt.return (Vfs.Dir.of_map_ref contents)
  end

  let static_dir name items = Vfs.Inode.dir name (Vfs.Dir.of_list items)

  let head_stream store initial_head =
    let session = Vfs.File.Stream.session initial_head in
    let remove_watch =
      let cb _ = Store.Head.find store >|= Vfs.File.Stream.publish session in
      Store.watch store ?init:initial_head cb
    in
    let pp ppf = function
      | None      -> Fmt.string ppf "\n"
      | Some hash -> Fmt.pf ppf "%a\n" Store.Commit.pp hash
    in
    ignore remove_watch; (* TODO *)
    Vfs.File.Stream.create pp session

  let head_live store =
    Vfs.File.of_stream (fun () ->
        Store.Head.find store >|= fun initial_head ->
        head_stream store initial_head
      )

  let reflog_stream store =
    let session = Vfs.File.Stream.session None in
    let remove_watch =
      let cb = function
        | `Added x | `Updated (_, x) -> Vfs.File.Stream.publish session (Some x)
        | `Removed _                 -> Vfs.File.Stream.publish session None
      in
      Store.watch store (fun x -> Lwt.return @@ cb x)
    in
    let pp ppf = function
      | Some x -> Fmt.pf ppf "%a\n" Store.Commit.pp x
      | None   -> Fmt.string ppf "\n"
    in
    ignore remove_watch; (* TODO *)
    Vfs.File.Stream.create pp session

  let reflog store =
    Vfs.File.of_stream (fun () -> Lwt.return (reflog_stream store))

  let hash_line store path =
    Store.tree store >>= fun snapshot ->
    Log.debug (fun x -> x "hash_line snapshot=%a path=%a"
                  (Irmin.Type.pp_json Store.tree_t) snapshot
                  (Irmin.Type.pp_json Store.key_t) path);
    (* FIXME: `hash` is probably slow as there is no caching *)
    let repo = Store.repo store in
    let hash = Store.Contents.hash repo in
    let pp_hash = Store.Contents.Hash.pp in
    Store.Tree.find_tree snapshot path >>= function
    | None                          -> Lwt.return "\n"
    | Some (`Contents (f, `Normal)) -> hash f >|= Fmt.strf "F-%a\n" pp_hash
    | Some (`Contents (f, `Exec))   -> hash f >|= Fmt.strf "X-%a\n" pp_hash
    | Some (`Contents (f, `Link))   -> hash f >|= Fmt.strf "L-%a\n" pp_hash
    | Some (`Node _ as dir) ->
      Store.Tree.hash repo dir >|= function
       | `Node n -> Fmt.strf "D-%a\n" Store.Tree.Hash.pp n
       | `Contents (c, _m) -> Fmt.strf "D-%a\n" Store.Contents.Hash.pp c

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
      Store.watch store cb
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
      let to_inode (x, _) = lookup x in
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
  let fast_forward store commit =
    Store.Head.fast_forward store commit >|= function
    | Ok ()            -> `Ok
    | Error `No_change -> `Ok
    | Error `Rejected  -> `Not_fast_forward (* (concurrent update) *)
    (* These shouldn't happen, because we didn't set any limits *)
    | Error (`Max_depth_reached | `Too_many_lcas) -> assert false

  let fast_forward_merge store =
    Vfs.File.command (fun hash ->
        commit_of_commit_id (Store.repo store) hash >>*= fun hash ->
        fast_forward store hash >>= function
        | `Ok               -> ok ""
        | `Not_fast_forward -> err_not_fast_forward
      )

  let status store () =
    Store.Head.find store >|= function
    | None      -> "\n"
    | Some head -> Fmt.strf "%a\n" Store.Commit.pp head

  let transactions ~info store =
    let lock = Lwt_mutex.create () in
    let items = ref String.Map.empty in
    let ls () = ok (String.Map.bindings !items |> List.map snd) in
    let lookup name = match String.Map.find name !items with
      | Some x -> ok x
      | None   -> err_no_entry
    in
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
            Transaction.make store ~info ~remover >>= fun dir ->
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

  let branch ~info ~remove repo name =
    let name = ref name in
    let remove () = remove !name in
    let make_contents name =
      Store.of_branch repo name >|= fun store ->
      [
        read_only ~name:"ro" store;
        Vfs.Inode.dir  "transactions" (transactions ~info store);
        Vfs.Inode.dir  "watch"        (watch_dir ~path:Path.empty store);
        Vfs.Inode.file "head.live"    (head_live store);
        Vfs.Inode.file "fast-forward" (fast_forward_merge store);
        Vfs.Inode.file "reflog"       (reflog store);
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

  let branch_dir ~info repo =
    let cache = ref String.Map.empty in
    let remove name =
      Store.Branch.remove repo name >|= fun () ->
      cache := String.Map.remove name !cache;
      Ok ()
    in
    let get_via_cache name = match String.Map.find name !cache with
      | Some x -> x
      | None   ->
        let entry = branch ~remove ~info repo name in
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
        Store.Branch.mem repo name >>= function
        | true  -> ok (get_via_cache name |> fst)
        | false -> err_no_entry
    in
    let mkdir name = ok (get_via_cache name |> fst) in
    let remove () = Vfs.Dir.err_read_only in
    let rename inode new_name =
      (* TODO: some races here... *)
      let old_name = Vfs.Inode.basename inode in
      Store.Branch.mem repo new_name >>= function
      | true -> err_is_dir
      | false ->
        Store.Branch.find repo old_name >>= fun head ->
        begin match head with
          | None      -> Lwt.return_unit
          | Some head -> Store.Branch.set repo new_name head
        end >>= fun () ->
        Store.Branch.remove repo old_name >>= fun () ->
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
    let file ty h =
      let h = String.trim h in
      match Store.Contents.Hash.of_string h with
      | Error _ -> Vfs.Error.no_entry
      | Ok hash -> Ok (`Contents (hash, ty))
    in
    let dir h =
      let h = String.trim h in
      match Store.Tree.Hash.of_string h with
      | Error _ -> Vfs.Error.no_entry
      | Ok hash -> Ok (`Node hash)
    in
    match String.span ~min:2 ~max:2 h with
    | "F-", hash -> file `Normal hash
    | "X-", hash -> file `Exec hash
    | "L-", hash -> file `Link hash
    | "D-", hash -> dir hash
    | _ -> Vfs.Error.no_entry

  let trees_dir repo =
    let inode_of_tree_hash name =
      Lwt.return (tree_hash_of_hum name) >>*= function
      | `Contents (hash, ty) ->
        begin
          Store.Contents.of_hash repo hash >|= function
          | None      -> Vfs.Error.no_entry
          | Some data ->
            let data = Blob.to_string data in
            let perm =
              match ty with
              | `Normal | `Exec as perm -> perm
              | `Link -> `Link data
            in
            Ok (Vfs.File.ro_of_string ~perm data |> Vfs.Inode.file name)
        end
      | `Node hash ->
        Store.Tree.of_hash repo (`Node hash) >|= function
        | None      -> Vfs.Error.no_entry
        | Some data ->
          Ok (ro_tree ~name:"ro" ~get_root:(fun () -> Lwt.return data))
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
      begin Store.Head.find store >>= function
        | None -> Lwt.return []
        | Some head ->
          Store.history store ~depth:1 >|= fun hist ->
          try Store.History.pred hist head
          with Invalid_argument _ -> []
      end >|= fun parents ->
      Ok (Some (Cstruct.of_string (string_of_commits parents)))
    in
    Vfs.File.of_kvro ~read ~stat:(Vfs.File.stat_of ~read)

  let msg_file store commit_id =
    let read () =
      let repo = Store.repo store in
      commit_of_commit_id repo commit_id >>*= fun commit ->
      let info = Store.Commit.info commit in
      let msg = Irmin.Info.message info ^ "\n" in
      ok (Some (Cstruct.of_string msg))
    in
    Vfs.File.of_kvro ~read ~stat:(Vfs.File.stat_of ~read)

  let snapshot_dir store name =
    let repo = Store.repo store in
    let dirs = Vfs.ok [
        read_only ~name:"ro"     store;
        Vfs.Inode.file "hash"    (Vfs.File.ro_of_string name);
        Vfs.Inode.file "msg"     (msg_file store name);
        Vfs.Inode.file "parents" (parents_file store);
        Vfs.Inode.dir  "diff"    (Diff.vfs_dir repo (fun () -> Store.tree store));
      ] in
    static_dir name (fun () -> dirs)

  let snapshots_dir repo =
    let cache = ref empty_inode_map in   (* Could use a weak map here *)
    let ls () = ok [] in
    let lookup name = match String.Map.find name !cache with
      | Some x ->  ok x
      | None   ->
        commit_of_commit_id repo name >>*= fun commit ->
        Store.of_commit commit >|= fun store ->
        let inode = snapshot_dir store name in
        cache := String.Map.add name inode !cache;
        Ok inode
    in
    let remove () = Vfs.Dir.err_read_only in
    Vfs.Dir.read_only ~ls ~lookup ~remove

  let commit_dir repo =
    let ls () = ok [] in
    let lookup name =
      commit_of_commit_id repo name >|= function
      | Ok commit ->
        let json =
          Fmt.to_to_string (Irmin.Type.pp_json (Store.commit_t repo)) commit
        in
        let hash = Fmt.to_to_string Store.Commit.pp commit in
        let inode = Vfs.File.ro_of_string json in
        Ok (Vfs.Inode.file hash inode)
      | Error _ as e -> e
    in
    let remove () = Vfs.File.err_read_only in
    Vfs.Dir.read_only ~ls ~lookup ~remove

  let create ~info repo =
    let dirs = Vfs.ok [
        Vfs.Inode.dir "branch"     (branch_dir ~info repo);
        Vfs.Inode.dir "trees"      (trees_dir repo);
        Vfs.Inode.dir "commits"    (commit_dir repo);
        Vfs.Inode.dir "snapshots"  (snapshots_dir repo);
        Vfs.Inode.dir "remotes"    (Remote.create repo);
        Vfs.Inode.dir "debug"      Vfs.Logs.dir;
      ] in
    Vfs.Dir.of_list (fun () -> dirs)

end
