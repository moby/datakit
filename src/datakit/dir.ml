open Lwt.Infix
open Result

type path = Path.t

type perm = Metadata.t

type blob = Blob.t

let ( >>*= ) x f = x >>= function Ok y -> f y | Error _ as e -> Lwt.return e

type impossible

let doesnt_fail = function Ok x -> x | Error (_ : impossible) -> assert false

module Make (Store : Store.S) = struct
  type t = {
    repo : Store.Repo.t;
    mutable root : Store.tree;
    mutex : Lwt_mutex.t;
  }

  let v repo root = { repo; root; mutex = Lwt_mutex.create () }

  let root t = t.root

  (* Walk to [t.root/path] and process the resulting directory with
     [fn dir], then update all the parents back to the root. *)
  let update_dir ~file_on_path t path fn =
    Lwt_mutex.with_lock t.mutex @@ fun () ->
    let empty = Store.Tree.empty in
    let rec aux base path =
      match Store.Key.decons path with
      | None -> fn base
      | Some (p, ps) ->
          let step = Store.Key.v [ p ] in
          (Store.Tree.find_tree base step >>= function
           | None -> aux empty ps
           | Some (`Node subdir) -> aux (Store.Tree.of_node subdir) ps
           | Some (`Contents f) ->
               file_on_path f >>*= fun () ->
               aux empty ps)
          >>*= fun new_subdir ->
          Store.Tree.add_tree base step new_subdir >|= fun x ->
          Ok x
    in
    aux t.root path >>*= fun new_root ->
    t.root <- new_root;
    Lwt.return (Ok ())

  let err_not_a_directory (_ : Store.contents * _) =
    Lwt.return (Error `Not_a_directory)

  let replace_with_dir (_ : Store.contents * _) = Lwt.return (Ok ())

  let update t path leaf (value, perm) =
    let step = Store.Key.v [ leaf ] in
    update_dir ~file_on_path:err_not_a_directory t path @@ fun dir ->
    let update ~old_perm =
      let perm = match perm with #Metadata.t as p -> p | `Keep -> old_perm in
      Store.Tree.add dir step ~metadata:perm value >|= fun new_dir ->
      Ok new_dir
    in
    Store.Tree.find_tree dir step >>= function
    | Some (`Node _) -> Lwt.return (Error `Is_a_directory)
    | Some (`Contents (_, old_perm)) -> update ~old_perm
    | None -> update ~old_perm:`Normal

  let chmod t path leaf perm =
    let step = Store.Key.v [ leaf ] in
    update_dir ~file_on_path:err_not_a_directory t path @@ fun dir ->
    Store.Tree.find_tree dir step >>= function
    | None -> Lwt.return (Error `No_such_item)
    | Some (`Node _) when perm = `Exec -> Lwt.return (Ok dir)
    | Some (`Node _) -> Lwt.return (Error `Is_a_directory)
    | Some (`Contents (f, _old_perm)) ->
        let file =
          match perm with
          | (`Normal | `Exec) as perm -> `Contents (f, perm)
          | `Link target -> `Contents (Blob.string target, `Link)
        in
        Store.Tree.add_tree dir step file >|= fun new_dir ->
        Ok new_dir

  let remove t path leaf =
    let step = Store.Key.v [ leaf ] in
    update_dir ~file_on_path:err_not_a_directory t path @@ fun dir ->
    Store.Tree.remove dir step >|= fun new_dir ->
    Ok new_dir

  let update_force t path leaf (value, perm) =
    let step = Store.Key.v [ leaf ] in
    update_dir ~file_on_path:replace_with_dir t path (fun dir ->
        Store.Tree.add dir step ~metadata:perm value >|= fun new_dir ->
        Ok new_dir)
    >|= doesnt_fail

  let remove_force t path leaf =
    let step = Store.Key.v [ leaf ] in
    update_dir ~file_on_path:replace_with_dir t path (fun dir ->
        Store.Tree.remove dir step >|= fun new_dir ->
        Ok new_dir)
    >|= doesnt_fail

  let rename t path ~old_name ~new_name =
    let old_step = Store.Key.v [ old_name ] in
    let new_step = Store.Key.v [ new_name ] in
    update_dir ~file_on_path:err_not_a_directory t path (fun dir ->
        Store.Tree.find_tree dir old_step >>= function
        | None -> Lwt.return (Error `No_such_item)
        | Some ((`Contents _ | `Node _) as value) -> (
            Store.Tree.find_tree dir new_step >>= function
            | Some (`Node _) -> Lwt.return (Error `Is_a_directory)
            | None | Some (`Contents _) ->
                Store.Tree.remove dir old_step >>= fun dir' ->
                Store.Tree.add_tree dir' new_step value >|= fun new_dir ->
                Ok new_dir ))
end
