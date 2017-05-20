open Lwt.Infix
open Datakit_client

let src = Logs.Src.create "datakit.git" ~doc:"DataKit Git client bindings"
module Log = (val Logs.src_log src: Logs.LOG)

module Infix = struct
  let (>>=) x f =
    x >>= function
    | Ok x         -> f x
    | Error _ as e -> Lwt.return e

  let (>|=) x f =
    x >|= function
    | Ok x         -> Ok (f x)
    | Error _ as e -> e
end

module IO = struct
  include Git_unix.Sync.IO
  let ctx () = Lwt.return_none
end

(* Similar to Irmin.Branch.String but allow '/' in branch names. *)
module B = struct
  include Irmin.Branch.String

  let is_valid s =
    let ok = ref true in
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      (match s.[!i] with
       | '/'
       | 'a' .. 'z'
       | 'A' .. 'Z'
       | '0' .. '9'
       | '-'| '_' | '.' -> ()
       | _ -> ok := false
      );
      incr i;
    done;
    !ok
end

module Make  = Irmin_git.FS.Make(IO)(Git_unix.Zlib)(Git_unix.FS.IO)
module S = Make(Irmin.Contents.Cstruct)(Irmin.Path.String_list)(B)

module PathSet = struct
  include Set.Make(struct
      include S.Key
      let compare = Irmin.Type.compare t
    end)

  let rec has_prefix ~prefix p =
    match S.Key.decons prefix, S.Key.decons p with
    | None, _ -> true
    | Some (pre, pres), Some (p, ps) ->
      if pre = p then has_prefix ~prefix:pres ps
      else false
    | _ -> false

  let remove_rec path t = filter (fun x -> not (has_prefix ~prefix:path x)) t

end

type t = {
  repo  : S.repo;
  author: string;
}

let (>?=) x f = match x with None -> None | Some x -> Some (f x)

let default_author = "datakit@mobyproject.org"

let connect ?head ?bare ?level ?dot_git ?(author=default_author) path =
  let head = head >?= Git.Reference.of_raw in
  let config = Irmin_git.config ?head ?bare ?level ?dot_git path in
  S.Repo.v config >|= fun repo ->
  { repo; author }

type error = [
  | `Already_exists
  | `Does_not_exist
  | `Is_dir
  | `Not_dir
  | `Not_file
  | `Not_symlink
  | `Transaction_closed
  | `Msg of string
  | `Conflict of string
  | `Conflicts
]

type +'a result = ('a, error) Result.result Lwt.t

let pp_error f : error -> unit = function
  | `Does_not_exist -> Fmt.string f "No such file or directory"
  | `Already_exists -> Fmt.string f "Already exists"
  | `Is_dir -> Fmt.string f "Is a directory"
  | `Not_symlink -> Fmt.string f "Not a symlink"
  | `Not_dir -> Fmt.string f "Not a directory"
  | `Not_file -> Fmt.string f "Not a file"
  | `Transaction_closed -> Fmt.string f "Transaction is closed"
  | `Conflicts -> Fmt.string f "conflicts file is not empty"
  | `Msg s -> Fmt.pf f "internal error: %s" s
  | `Conflict c -> Fmt.pf f "conflict: %s" c

let err_does_not_exist = Lwt.return (Error `Does_not_exist)
let err_transaction_closed = Lwt.return (Error `Transaction_closed)
let err_conflicts = Lwt.return (Error `Conflicts)
let err_msg fmt = Fmt.kstrf (fun x -> Error (`Msg x :> error)) fmt

module Tree = struct

  type t = S.tree

  let str = Cstruct.to_string

  let read t path =
    let path = Path.unwrap path in
    S.Tree.find_tree t path >>= function
    | None                        -> err_does_not_exist
    | Some (`Contents (x, `Link)) -> Lwt.return (Ok (`Link (str x)))
    | Some (`Contents (x, _))     -> Lwt.return (Ok (`File x))
    | Some (`Node _ as dir)       ->
      S.Tree.list dir [] >|= fun ls ->
      Ok (`Dir (List.map fst ls))

  let link l = { Datakit_client.kind = `Link; size = Int64.of_int (Cstruct.len l) }
  let dir = { kind = `Dir; size = 0L }
  let file f = { kind = `File; size = Int64.of_int (Cstruct.len f) }
  let exec f = { kind = `Exec; size = Int64.of_int (Cstruct.len f) }

  let stat t path =
    let path = Path.unwrap path in
    S.Tree.find_tree t path >|= function
    | None                          -> Ok (None)
    | Some (`Contents (x, `Link))   -> Ok (Some (link x))
    | Some (`Contents (x, `Exec))   -> Ok (Some (exec x))
    | Some (`Contents (x, `Normal)) -> Ok (Some (file x))
    | Some (`Node _)                -> Ok (Some dir)

  let exists t path =
    let path = Path.unwrap path in
    S.Tree.mem t path >|= fun b -> Ok b

  let read_file t path =
    read t path >|= function
    | Error _ as e -> e
    | Ok (`File x) -> Ok x
    | Ok _         -> Error `Not_file

  let read_link t path =
    read t path >|= function
    | Error _ as e -> e
    | Ok (`Link x) -> Ok x
    | Ok _         -> Error `Not_symlink
  let read_dir t path =
    read t path >|= function
    | Error _ as e -> e
    | Ok (`Dir x)  -> Ok x
    | Ok _         -> Error `Not_dir

  let exists_file t path =
    read t path >|= function
    | Ok (`File _)          -> Ok true
    | Ok _                  -> Ok false
    | Error `Does_not_exist -> Ok false
    | Error _ as e          -> e

  let exists_dir t path =
    read t path >|= function
    | Ok (`Dir _)           -> Ok true
    | Ok _                  -> Ok false
    | Error `Does_not_exist -> Ok false
    | Error _ as e          -> e

  let diff x y =
    S.Tree.diff y x >|= fun diff ->
    let diff =
      List.map (fun (path, diff) ->
        let path = Path.of_steps_exn path in
        match diff with
        | `Added _   -> `Added path
        | `Removed _ -> `Removed path
        | `Updated _ -> `Updated path
        ) diff
    in
    Ok diff

end

let ok x = Ok x

module Commit = struct
  type t = S.commit
  let pp = S.Commit.pp
  let raw x = S.Commit.Hash.to_raw (S.Commit.hash x)
  let compare x y = Cstruct.compare (raw x) (raw y)
  let tree t = S.Commit.tree t >|= ok
  let id t = Fmt.to_to_string S.Commit.pp t
  let parents t = S.Commit.parents t >|= ok

  let message t =
    let msg = Irmin.Info.message (S.Commit.info t) ^ "\n" in
    Lwt.return (Ok msg)

  let diff x y =
    let open Infix in
    tree x >>= fun x ->
    tree y >>= fun y ->
    Tree.diff x y
end

type transaction = {
    mutable closed   : bool;
    mutable tree     : S.tree;
    mutable parents  : S.commit list;
    mutable conflicts: PathSet.t;
    store            : S.t;
    t                : t;
    lock             : Lwt_mutex.t;
  }

module Merge = struct

  (* similar to ivfs_merge ... maybe we should be merged *)

  open Astring

  let merge_file =
    let blob =
      Irmin.Merge.idempotent Irmin.Type.(pair S.contents_t S.metadata_t)
    in
    Irmin.Merge.(option blob)

  let map tree = S.Tree.list tree S.Key.empty >|= String.Map.of_list

  let update t path (f, metadata) =
    S.Tree.add t.tree path ~metadata f >|= fun tree ->
    t.tree <- tree

  let remove t path =
    S.Tree.remove t.tree path >|= fun tree ->
    t.tree <- tree

  let (/) = S.Key.rcons

  let merge ~ours ~theirs ~base result =
    let conflicts = ref PathSet.empty in
    let note_conflict path leaf msg =
      let path = path / leaf in
      conflicts := PathSet.add path !conflicts;
      let f = Cstruct.of_string (Fmt.strf "** Conflict **\n%s\n" msg) in
      update result path (f, `Normal)
    in
    let as_dir = function
      | None   -> S.Tree.empty
      | Some v -> v
    in
    let rec merge_dir ~ours ~theirs ~base path =
      map ours >>= fun our_files ->
      map theirs >>= fun their_files ->
      (* Types tells us the type the result will have, if successful,
         or [`Conflict] if we know it won't work. *)
      let types =
        String.Map.merge (fun _leaf ours theirs ->
            match ours, theirs with
            | Some `Node, Some `Node -> Some `Node
            | Some `Contents, Some `Contents -> Some `Contents
            | Some _, Some _ -> Some `Conflict
            | Some `Contents, None | None, Some `Contents -> Some `Contents
            | Some `Node, None | None, Some `Node -> Some `Node
            | None, None -> assert false
          ) our_files their_files
      in
      String.Map.bindings types |> Lwt_list.iter_s (fun (leaf, ty) ->
          let sub_path = S.Key.rcons path leaf in
          let step = S.Key.v [leaf] in
          match ty with
          | `Conflict -> note_conflict path leaf "File vs dir"
          | `Node ->
            S.Tree.find_tree ours step >|= as_dir >>= fun ours ->
            S.Tree.find_tree theirs step >|= as_dir >>= fun theirs ->
            S.Tree.find_tree base step >|= as_dir >>= fun base ->
            merge_dir ~ours ~theirs ~base sub_path
          | `Contents ->
            S.Tree.find_all ours step >>= fun ours ->
            S.Tree.find_all theirs step >>= fun theirs ->
            let old () =
              S.Tree.find_all base step >|= fun f ->
              Ok (Some f)
            in
            Irmin.Merge.f merge_file ~old ours theirs >>= function
            | Ok (Some x) -> update result (path / leaf) x
            | Ok None     -> remove result (path / leaf)
            | Error (`Conflict "default") ->
              note_conflict path leaf "Changed on both branches"
            | Error (`Conflict x)         -> note_conflict path leaf x
        )
    in
    S.tree ours >>= fun ours ->
    S.tree theirs >>= fun theirs ->
    begin match base with
      | None      -> Lwt.return S.Tree.empty
      | Some base -> S.tree base
    end >>= fun base ->
    merge_dir ~ours ~theirs ~base S.Key.empty >>= fun () ->
    Lwt.return !conflicts

end

module Transaction = struct

  type t = transaction

  let v t ~store =
    let closed = false in
    let lock = Lwt_mutex.create () in
    let conflicts = PathSet.empty in
    (S.Head.find store >>= function
      | None      -> Lwt.return ([], S.Tree.empty)
      | Some head ->
        S.Commit.tree head >|= fun tree ->
        ([head], tree)
    ) >|= fun (parents, tree) ->
    Ok { closed; tree; parents; store; t; lock; conflicts }

  let read t = Tree.read t.tree
  let stat t = Tree.stat t.tree
  let exists t = Tree.exists t.tree
  let exists_file t = Tree.exists_file t.tree
  let exists_dir t = Tree.exists_dir t.tree
  let read_file t = Tree.read_file t.tree
  let read_dir t = Tree.read_dir t.tree
  let read_link t = Tree.read_link t.tree

  let create t path f =
    if t.closed then err_transaction_closed
    else Lwt_mutex.with_lock t.lock (fun () ->
        let path = Datakit_client.Path.unwrap path in
        (* FIXME(samoht): why ?
        S.Tree.mem t.tree path >>= function
        | true  -> err_already_exists
        | false -> *)
          f t.tree path >|= fun tree ->
          t.tree <- tree;
          t.conflicts <- PathSet.remove_rec path t.conflicts;
          Ok ()
      )

  let replace t path f =
    if t.closed then err_transaction_closed
    else Lwt_mutex.with_lock t.lock (fun () ->
        let path = Path.unwrap path in
        S.Tree.mem t.tree path >>= function
        | false -> err_does_not_exist
        | true  ->
          f t.tree path >|= fun tree ->
          t.tree <- tree;
          t.conflicts <- PathSet.remove_rec path t.conflicts;
          Ok ()
      )

  let update t path f =
    if t.closed then err_transaction_closed
    else Lwt_mutex.with_lock t.lock (fun () ->
        let path = Path.unwrap path in
        S.Tree.find_all t.tree path >>= function
        | None   -> err_does_not_exist
        | Some x ->
          f t.tree path x >|= fun tree ->
          t.tree <- tree;
          t.conflicts <- PathSet.remove_rec path t.conflicts;
          Ok ()
      )

  let create_or_replace t path f =
    if t.closed then err_transaction_closed
    else Lwt_mutex.with_lock t.lock (fun () ->
        let path = Path.unwrap path in
        f t.tree path >|= fun tree ->
        t.tree <- tree;
        t.conflicts <- PathSet.remove_rec path t.conflicts;
        Ok ()
      )

  let create_dir t path =
    create t path (fun t path ->
        (* FIXME: it's probably a no-op currently *)
        S.Tree.add_tree t path S.Tree.empty
      )

  let create_file t path ?(executable=false) f =
    create t path (fun t path ->
        let metadata = if executable then `Exec else `Normal in
        S.Tree.add t ~metadata path f
      )

  let create_symlink t path l =
    create t path (fun t path ->
        S.Tree.add t path ~metadata:`Link (Cstruct.of_string l)
      )

  let replace_file t path f =
    replace t path (fun t path -> S.Tree.add t path f)

  let create_or_replace_file t path f =
    create_or_replace t path (fun t path -> S.Tree.add t path f)

  let set_executable t path b =
    let metadata = if b then `Exec else `Normal in
    update t path (fun t path (f, m) ->
        if m = metadata then Lwt.return t
        else S.Tree.add t path ~metadata f
      )

  let remove t path =
    create_or_replace t path S.Tree.remove

  let truncate t path n =
    update t path (fun t path (f, metadata) ->
        let c = Cstruct.len f in
        let n = Int64.to_int n in
        if c = n then Lwt.return t
        else if c > n then
          let f = Cstruct.sub f 0 n in
          S.Tree.add t path ~metadata f
        else
          let buf = Cstruct.create n in
          Cstruct.memset buf 0;
          Cstruct.blit f 0 buf 0 c;
          Log.debug (fun l -> l "extend %S" (Cstruct.to_string buf));
          S.Tree.add t path ~metadata buf
      )

  let make_dirs t path =
    create_or_replace t path (fun t path ->
        (* FIXME: this is probably a no-op *)
        S.Tree.add_tree t path S.Tree.empty
      )

  let info t msg () =
    let date = Int64.of_float (Unix.gettimeofday ()) in
    Irmin.Info.v ~date ~author:t.t.author msg

  let commit t ~message =
    if t.closed then err_transaction_closed
    else if not (PathSet.is_empty t.conflicts) then err_conflicts
    else
      let info = info t message () in
      S.Commit.v t.t.repo ~info ~parents:t.parents t.tree >>= fun c ->
      t.closed <- true;
      (* FIXME(samoht): why do we reuse the same commit message here? *)
      S.Head.merge ~info:(fun () -> info) ~into:t.store c >|= function
      | Error (`Conflict _) as e -> e
      | Ok () -> Ok ()

  let abort t = t.closed <- true; Lwt.return (Ok ())

  type merge_inputs = {
    ours  : Tree.t;
    theirs: Tree.t;
    base  : Tree.t;
  }

  (* from ivfs *)
  let base ~our_parents ~ours ~their_commit =
    match our_parents with
    | [] ->
      (* Optimisation: if our new commit has no parents then we know there
           can be no LCA, so avoid searching (which would be slow, since Irmin
           would have to explore the entire history to check). *)
      Lwt.return None
    | _ ->
      S.lcas_with_commit ours ~n:1 their_commit >>= function
      | Error (`Max_depth_reached | `Too_many_lcas) -> assert false
      | Ok []        -> Lwt.return None
      | Ok (base::_) -> S.of_commit base >|= fun s -> Some s

  let paths conflicts =
    PathSet.elements conflicts
    |> List.map Path.of_steps_exn

  let merge t their_commit =
    if t.closed then err_transaction_closed
    else
      S.of_commit their_commit >>= fun theirs ->
      let info = info t "(no commit message)" () in
      let our_parents = t.parents in
      let repo = t.t.repo in
      S.Commit.v repo ~info ~parents:our_parents t.tree >>= fun our_commit ->
      S.of_commit our_commit >>= fun ours ->
      base ~our_parents ~ours ~their_commit >>= fun base ->
      t.parents <- their_commit :: our_parents;
      Merge.merge ~ours ~theirs ~base t >>= fun merge_conflicts ->
      t.conflicts <- PathSet.union t.conflicts merge_conflicts;
      S.tree ours >>= fun ours ->
      S.tree theirs >>= fun theirs ->
      (match base with
       | None   -> Lwt.return S.Tree.empty
       | Some s -> S.tree s
      ) >|= fun base ->
      let inputs = { ours; theirs; base } in
      Ok (inputs, paths merge_conflicts)

  let parents t = Lwt.return (Ok t.parents)
  let set_parents t c = t.parents <- c; Lwt.return (Ok ())
  let conflicts t = Lwt.return (Ok (paths t.conflicts))

  let closed t = t.closed

  let diff t c =
    S.Commit.tree c >>= fun tree ->
    Tree.diff t.tree tree

end

module Branch = struct

  type nonrec t = {
    t           : t;
    mutable name: S.branch
  }

  let v t name = { t; name }
  let name t = t.name
  let remove t = S.Branch.remove t.t.repo t.name >|= ok
  let repo t = t.t.repo
  let head t = S.Branch.find (repo t) t.name >|= ok

  let abort_if_off switch fn =
    match switch with
    | None -> fn ()
    | Some sw when Lwt_switch.is_on sw -> fn ()
    | Some _ -> Lwt.return (ok `Abort)

  exception Stop

  let wait_for_head t ?switch f =
    let stop = ref (fun () -> Lwt.return_unit) in
    let result = ref (err_msg "no result") in
    let callback diff =
      (abort_if_off switch @@ fun () ->
       let v = match diff with
         | `Removed _ -> None
         | `Added t | `Updated (_, t) -> Some t
       in
       f v
      ) >>= function
      | Ok `Again -> Lwt.return_unit
      | Ok (`Abort | `Finish _) | Error _ as x -> result := x; Lwt.fail Stop
    in
    S.Branch.find (repo t) t.name >>= fun head ->
    f head >>= function
    | Ok (`Abort | `Finish _) | Error _  as x -> Lwt.return x
    | Ok `Again ->
      S.of_branch (repo t) t.name >>= fun t ->
      let th, u = Lwt.task () in
      Lwt_switch.add_hook_or_exec switch (fun () ->
          Lwt.wakeup u ();
          Lwt.return_unit
        ) >>= fun () ->
      Lwt.catch (fun () ->
          S.watch ?init:head t callback >>= fun w ->
          stop := (fun () -> Lwt.wakeup u (); S.unwatch w);
          th >|= fun () ->
          Ok `Abort
        ) (function
          | Stop -> !stop () >|= fun () -> !result
          | e    -> Lwt.return (err_msg "%a" Fmt.exn e)
        )

  let wait_for_path t ?switch path f =
    let stop = ref (fun () -> Lwt.return_unit) in
    let value = function
      | `Contents (c, `Link)   -> Some (`Link (Cstruct.to_string c))
      | `Contents (c, `Exec)   -> Some (`Exec c)
      | `Contents (c, `Normal) -> Some (`File c)
      | `Node _ as dir         -> Some (`Dir dir)
    in
    S.of_branch (repo t) t.name >>= fun t ->
    S.Head.find t >>= fun head ->
    let path = Path.unwrap path in
    S.find_tree t path >>= fun tree ->
    f (match tree with None -> None | Some t -> value t) >>= function
    | Ok (`Abort | `Finish _) | Error _ as x -> Lwt.return x
    | Ok `Again ->
      let result = ref (err_msg "no result") in
      let callback diff =
        (abort_if_off switch @@ fun () ->
         let v = match diff with
           | `Removed _ -> None
           | `Added (_, t)
           | `Updated (_, (_, t)) -> value t
         in
         f v
        ) >>= function
        | Ok `Again -> Lwt.return_unit
        | Ok (`Abort | `Finish _) | Error _ as x -> result := x; Lwt.fail Stop
      in
      let th, u = Lwt.task () in
      Lwt_switch.add_hook_or_exec switch (fun () ->
          Lwt.wakeup u ();
          Lwt.return_unit
        ) >>= fun () ->
      Lwt.catch (fun () ->
          S.watch_key ?init:head t path callback >>= fun w ->
          stop := (fun () -> Lwt.wakeup u (); S.unwatch w);
          th >|= fun () ->
          Ok `Abort
        ) (function
          | Stop -> !stop () >|= fun () -> !result
          | e    -> Lwt.return (err_msg "%a" Fmt.exn e)
        )

  let pp_ff_error = Irmin.Type.pp_json S.ff_error_t

  let fast_forward t c =
    S.of_branch (repo t) t.name >>= fun store ->
    S.Head.fast_forward store c >|= function
    | Ok ()   -> Ok ()
    | Error e -> err_msg "ff error: %a" pp_ff_error e

  let transaction t =
    S.of_branch (repo t) t.name >>= fun store ->
    Transaction.v t.t ~store

  let with_transaction t f =
    transaction t >>= function
    | Error _ as e -> Lwt.return e
    | Ok tr        ->
      let open Infix in
      f tr >>= fun res ->
      (if not (Transaction.closed tr) then Transaction.abort tr
       else Lwt.return (Ok ()))
      >|= fun () ->
      res

end

let branches t = S.Branch.list t.repo >|= ok
let remove_branch t n = S.Branch.remove t.repo n >|= ok
let branch t name = Lwt.return (Ok (Branch.v t name))

let commit t h =
  match S.Commit.Hash.of_string h with
  | Error e -> Lwt.return (Error (e :> error))
  | Ok hash ->
    S.Commit.of_hash t.repo hash >|= function
    | None   -> err_msg "%s is not a valid commit" h
    | Some x -> Ok x

let tree t h =
  match S.Tree.Hash.of_string h with
  | Error e -> Lwt.return (Error (e :> error))
  | Ok hash ->
    S.Tree.of_hash t.repo hash >|= function
    | None   -> err_msg "%s is not a valid tree" h
    | Some x -> Ok x

module Sync = Irmin.Sync(S)

let fetch t ~url ~branch =
  let remote = Irmin.remote_uri url in
  S.of_branch t.repo branch >>= fun store ->
  Sync.fetch store remote >|= function
  | Ok x    -> Ok x
  | Error e -> err_msg "fetch: %a" Sync.pp_fetch_error e

let disconnect _ = Lwt.return (Ok ())

let () =
  Irmin.Private.Watch.set_listen_dir_hook Irmin_watcher.hook
