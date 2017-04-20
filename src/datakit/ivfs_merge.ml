open Astring
open Lwt.Infix

let src = Logs.Src.create "ivfs.merge" ~doc:"Irmin VFS"
module Log = (val Logs.src_log src : Logs.LOG)

type path = Ivfs_tree.path
type step = Ivfs_tree.step

module Path = struct
  include Ivfs_tree.Path
  let compare = Irmin.Type.compare t
end
module PathSet = Set.Make(Path)

module type RW = sig
  type t
  val update_force : t -> Ivfs_tree.path -> string -> Ivfs_blob.t * Ivfs_tree.perm -> unit Lwt.t
  val remove_force : t -> Ivfs_tree.path -> string -> unit Lwt.t
end

module Make (Store : Ivfs_tree.S) (RW : RW) = struct
  module Metadata = Store.Metadata
  module Dir = Store.Tree

  (* blobs are idempotents *)
  (* FIXME: move into Irmin.Merge *)
  let merge_idempotent dt =
    let (=) = Irmin.Type.equal dt in
    let default = Irmin.Merge.default dt in
    let f ~old x y =
      if x = y then Irmin.Merge.ok x
      else Irmin.Merge.f default ~old x y
    in
    Irmin.Merge.v dt f

  let merge_file =
    let blob = merge_idempotent Irmin.Type.(pair Ivfs_blob.t Metadata.t) in
    Irmin.Merge.(option blob)

  let map tree = Dir.list tree Store.Key.empty >|= String.Map.of_list

  let merge ~ours ~theirs ~base result =
    let conflicts = ref PathSet.empty in
    let note_conflict path leaf msg =
      conflicts := !conflicts |> PathSet.add (Store.Key.rcons path leaf);
      let f = Ivfs_blob.of_string (Printf.sprintf "** Conflict **\n%s\n" msg) in
      RW.update_force result path leaf (f, `Normal)
    in
    let empty = Store.Tree.empty () in
    let as_dir = function
      | None   -> empty
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
          let sub_path = Store.Key.rcons path leaf in
          let step = Path.v [leaf] in
          match ty with
          | `Conflict -> note_conflict path leaf "File vs dir"
          | `Node ->
            Dir.find_tree ours step >|= as_dir >>= fun ours ->
            Dir.find_tree theirs step >|= as_dir >>= fun theirs ->
            Dir.find_tree base step >|= as_dir >>= fun base ->
            merge_dir ~ours ~theirs ~base sub_path
          | `Contents ->
            Dir.find_all ours step >>= fun ours ->
            Dir.find_all theirs step >>= fun theirs ->
            let old () =
              Dir.find_all base step >|= fun f ->
              Ok (Some f)
            in
            Irmin.Merge.f merge_file ~old ours theirs >>= function
            | Ok (Some x) -> RW.update_force result path leaf x
            | Ok None -> RW.remove_force result path leaf
            | Error (`Conflict "default") ->
              note_conflict path leaf "Changed on both branches"
            | Error (`Conflict x)         -> note_conflict path leaf x
        )
    in
    Store.tree ours >>= fun ours ->
    Store.tree theirs >>= fun theirs ->
    begin match base with
      | None      -> Lwt.return empty
      | Some base -> Store.tree base
    end >>= fun base ->
    merge_dir ~ours ~theirs ~base Store.Key.empty >>= fun () ->
    Lwt.return !conflicts
end
