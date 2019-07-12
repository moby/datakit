open Astring
open Lwt.Infix

type path = Path.t

type step = Path.step

type blob = Blob.t

type perm = Metadata.t

module type RW = sig
  type t

  val update_force : t -> path -> string -> blob * perm -> unit Lwt.t

  val remove_force : t -> path -> string -> unit Lwt.t
end

module Make (Store : Store.S) (RW : RW) = struct
  module Metadata = Store.Metadata
  module Dir = Store.Tree

  let merge_file =
    let blob = Irmin.Merge.idempotent Irmin.Type.(pair Blob.t Metadata.t) in
    Irmin.Merge.(option blob)

  let map tree = Dir.list tree Store.Key.empty >|= String.Map.of_list

  let merge ~ours ~theirs ~base result =
    let conflicts = ref Path.Set.empty in
    let note_conflict path leaf msg =
      conflicts := !conflicts |> Path.Set.add (Store.Key.rcons path leaf);
      let f = Blob.string (Printf.sprintf "** Conflict **\n%s\n" msg) in
      RW.update_force result path leaf (f, `Normal)
    in
    let as_dir = function None -> Store.Tree.empty | Some v -> v in
    let rec merge_dir ~ours ~theirs ~base path =
      map ours >>= fun our_files ->
      map theirs >>= fun their_files ->
      (* Types tells us the type the result will have, if successful,
         or [`Conflict] if we know it won't work. *)
      let types =
        String.Map.merge
          (fun _leaf ours theirs ->
            match (ours, theirs) with
            | Some `Node, Some `Node -> Some `Node
            | Some `Contents, Some `Contents -> Some `Contents
            | Some _, Some _ -> Some `Conflict
            | Some `Contents, None | None, Some `Contents -> Some `Contents
            | Some `Node, None | None, Some `Node -> Some `Node
            | None, None -> assert false)
          our_files their_files
      in
      String.Map.bindings types
      |> Lwt_list.iter_s (fun (leaf, ty) ->
             let sub_path = Store.Key.rcons path leaf in
             let step = Path.v [ leaf ] in
             match ty with
             | `Conflict -> note_conflict path leaf "File vs dir"
             | `Node ->
                 Dir.find_tree ours step >|= as_dir >>= fun ours ->
                 Dir.find_tree theirs step >|= as_dir >>= fun theirs ->
                 Dir.find_tree base step >|= as_dir >>= fun base ->
                 merge_dir ~ours ~theirs ~base sub_path
             | `Contents -> (
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
                 | Error (`Conflict x) -> note_conflict path leaf x ))
    in
    Store.tree ours >>= fun ours ->
    Store.tree theirs >>= fun theirs ->
    ( match base with
    | None -> Lwt.return Store.Tree.empty
    | Some base -> Store.tree base )
    >>= fun base ->
    merge_dir ~ours ~theirs ~base Store.Key.empty >>= fun () ->
    Lwt.return !conflicts
end
