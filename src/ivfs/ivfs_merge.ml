open Astring
open Lwt.Infix

module PathSet = Set.Make(Irmin.Path.String_list)

let blob = Tc.biject (module Tc.Cstruct)
    Ivfs_blob.of_ro_cstruct
    Ivfs_blob.to_ro_cstruct

module Blob = (val blob : Tc.S0 with type t = Ivfs_blob.t)

module type RW = sig
  type t
  val update_force : t -> Ivfs_tree.path -> string -> Ivfs_blob.t * Ivfs_tree.perm -> unit Lwt.t
  val remove_force : t -> Ivfs_tree.path -> string -> unit Lwt.t
end

module Make
    (Store : Ivfs_tree.STORE)
    (RW : RW)
= struct
  module Tree = Ivfs_tree.Make(Store)
  module Metadata = Store.Private.Node.Val.Metadata
  module ContentsMeta = Tc.Pair(Blob)(Metadata)

  let as_file = function
    | `File (f, perm) -> Tree.File.content f >|= fun c -> Some (c, perm)
    | `Directory _ | `None -> Lwt.return None

  let merge_blob = Irmin.Merge.default (module Blob)

  let merge_file =
    Irmin.Merge.option (module ContentsMeta)
      (Irmin.Merge.pair (module Blob) (module Metadata) merge_blob Metadata.merge)

  let merge ~ours ~theirs ~base result =
    let conflicts = ref PathSet.empty in
    let note_conflict path leaf msg =
      conflicts := !conflicts |> PathSet.add (Irmin.Path.String_list.rcons path leaf);
      let f = Ivfs_blob.of_string (Printf.sprintf "** Conflict **\n%s\n" msg) in
      RW.update_force result path leaf (f, `Normal) in
    let repo = Store.repo ours in
    let empty = Tree.Dir.empty repo in
    let as_dir = function
      | `Directory x -> x
      | `File _ -> empty
      | `None -> empty in
    let rec merge_dir ~ours ~theirs ~base (path : string list) =
      Tree.Dir.map ours >>= fun our_files ->
      Tree.Dir.map theirs >>= fun their_files ->
      (* Types tells us the type the result will have, if successful, or [`Conflict] if we
         know it won't work. *)
      let types =
        String.Map.merge (fun _leaf ours theirs ->
            match ours, theirs with
            | Some (`Directory _), Some (`Directory _) -> Some `Directory
            | Some (`File _), Some (`File _) -> Some `File
            | Some _, Some _ -> Some `Conflict
            | Some (`File _), None | None, Some (`File _) -> Some `File
            | Some (`Directory _), None | None, Some (`Directory _) -> Some `Directory
            | None, None -> assert false
          ) our_files their_files in
      String.Map.bindings types |> Lwt_list.iter_s (fun (leaf, ty) ->
          let sub_path = Irmin.Path.String_list.rcons path leaf in
          match ty with
          | `Conflict -> note_conflict path leaf "File vs dir"
          | `Directory ->
            Tree.Dir.lookup ours leaf >|= as_dir >>= fun ours ->
            Tree.Dir.lookup theirs leaf >|= as_dir >>= fun theirs ->
            Tree.Dir.lookup base leaf >|= as_dir >>= fun base ->
            merge_dir ~ours ~theirs ~base sub_path
          | `File ->
            Tree.Dir.lookup ours leaf >>= as_file >>= fun ours ->
            Tree.Dir.lookup theirs leaf >>= as_file >>= fun theirs ->
            let old () =
              Tree.Dir.lookup base leaf >>= fun hash ->
              as_file hash >|= fun f ->
              `Ok (Some f) in
            merge_file ~old ours theirs >>= function
            | `Ok (Some x) -> RW.update_force result path leaf x
            | `Ok None -> RW.remove_force result path leaf
            | `Conflict "default" -> note_conflict path leaf "Changed on both branches"
            | `Conflict x -> note_conflict path leaf x
        ) in
    Tree.snapshot ours >>= fun ours ->
    Tree.snapshot theirs >>= fun theirs ->
    begin match base with
      | None -> Lwt.return (Tree.Dir.empty repo)
      | Some base -> Tree.snapshot base
    end >>= fun base ->
    merge_dir ~ours ~theirs ~base [] >>= fun () ->
    Lwt.return !conflicts
end
