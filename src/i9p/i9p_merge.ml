open Lwt.Infix

module PathSet = Set.Make(Irmin.Path.String_list)

module Make
    (Store : I9p_tree.STORE)
    (View : Irmin.VIEW with type db = Store.t
                        and type key = string list
                        and type value = string)
= struct
  module LeafMap = Map.Make(String)
  module Tree = I9p_tree.Make(Store)

  let as_map =
    List.fold_left (fun acc (ty, leaf) ->
        LeafMap.add leaf ty acc
      ) LeafMap.empty

  let as_file contents_t = function
    | `File f -> Store.Private.Contents.read contents_t f
    | `Directory _ | `None -> Lwt.return None

  let merge_file = Irmin.Merge.(option (module Tc.String) string)

  let merge ~ours ~theirs ~base result =
    let conflicts = ref PathSet.empty in
    let note_conflict path msg =
      conflicts := !conflicts |> PathSet.add path;
      View.update result path (Printf.sprintf "** Conflict **\n%s\n" msg) in
    let repo = Store.repo ours in
    let empty = Tree.empty repo in
    let contents_t = Store.Private.Repo.contents_t repo in
    let as_dir = function
      | `Directory x -> x
      | `File _ -> empty
      | `None -> empty in
    let rec merge_dir ~ours ~theirs ~base (path : string list) =
      Tree.ls ours >|= as_map >>= fun our_files ->
      Tree.ls theirs >|= as_map >>= fun their_files ->
      let types =
        LeafMap.merge (fun _leaf our_ty their_ty ->
            match our_ty, their_ty with
            | Some `Directory, Some `Directory -> Some `Directory
            | Some `File, Some `File -> Some `File
            | Some _, Some _ -> Some `Conflict
            | Some `File, _ | _, Some `File -> Some `File
            | Some `Directory, _ | _, Some `Directory -> Some `Directory
            | None, None -> assert false
          ) our_files their_files in
      LeafMap.bindings types |> Lwt_list.iter_s (fun (leaf, ty) ->
          let path = Irmin.Path.String_list.rcons path leaf in
          match ty with
          | `Conflict -> note_conflict path "File vs dir"
          | `Directory ->
            Tree.node ours [leaf] >|= as_dir >>= fun ours ->
            Tree.node theirs [leaf] >|= as_dir >>= fun theirs ->
            Tree.node base [leaf] >|= as_dir >>= fun base ->
            merge_dir ~ours ~theirs ~base path
          | `File ->
            Tree.node ours [leaf] >>= as_file contents_t >>= fun ours ->
            Tree.node theirs [leaf] >>= as_file contents_t >>= fun theirs ->
            let old () =
              Tree.node base [leaf] >>= fun hash ->
              as_file contents_t hash >|= fun f ->
              `Ok (Some f) in
            merge_file ~old ours theirs >>= function
            | `Ok (Some x) -> View.update result path x
            | `Ok None -> View.remove result path
            | `Conflict "default" -> note_conflict path "Changed on both branches"
            | `Conflict x -> note_conflict path x
        ) in
    Tree.snapshot ours >>= fun ours ->
    Tree.snapshot theirs >>= fun theirs ->
    begin match base with
      | None -> Lwt.return (Tree.empty repo)
      | Some base -> Tree.snapshot base
    end >>= fun base ->
    merge_dir ~ours ~theirs ~base [] >>= fun () ->
    Lwt.return !conflicts
end
