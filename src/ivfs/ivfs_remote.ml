open Lwt.Infix

let src = Logs.Src.create "ivfs-remote" ~doc:"Irmin VFS: remote repositories"
module Log = (val Logs.src_log src: Logs.LOG)

let err_no_head =  Vfs.error "error: no head to fetch"
let err_fetch_error = Vfs.error "error: cannot fetch %s"
let err_no_url = Vfs.error "error: remote url is not defined"

module Make (Store : Ivfs_tree.STORE) = struct

  module Sync = Irmin.Sync(Store)

  type remote = {
    name: string;
    url : string option;
  }

  type state = {
    remote_name: string;
    remote_url : unit -> string option;
    update_head: Store.commit_id option -> unit;
    fetch      : string -> unit Vfs.or_err;
  }

  type t = {
    mutable state: (state * Vfs.Inode.t) list;
    mutable root : Vfs.Dir.t;
    repo         : Store.Repo.t;
  }

  let list t =
    List.map (fun (s, _) ->
      { name = s.remote_name; url = s.remote_url () }
    ) t.state

  let mk_head session =
    let pp_o ppf = function
      | None   -> Fmt.string ppf ""
      | Some x -> Fmt.pf ppf "%s\n" (Store.Hash.to_hum x)
    in
    let stream () =
      Vfs.File.Stream.create pp_o session
      |> Lwt.return
    in
    let file = Vfs.File.of_stream stream in
    file, fun x -> Vfs.File.Stream.publish session x

  (* /remotes/<name>/url *)
  let mk_url default =
    let file, fn = Vfs.File.rw_of_string default in
    file, (function () -> match fn () with "" -> None | s -> Some s)

  let dummy_task _ = Irmin.Task.none ()

  (* /remotes/<name>/fetch *)
  let mk_fetch ~remote_url ~update_head repo =
    let handler branch =
      match remote_url () with
      | None     -> err_no_url
      | Some url ->
        let r = Irmin.remote_uri url in
        Store.of_branch_id dummy_task branch repo >>= fun store ->
        Sync.fetch (store "fetch") r >>= function
        | `No_head -> err_no_head
        | `Error   -> err_fetch_error url
        | `Head h  -> update_head (Some h); Vfs.ok ""
    in
    (fun str ->
       handler str >>= function
       | Result.Ok ""   -> Vfs.ok ()
       | Result.Ok _    -> Vfs.error "Non-empty OK!"
       | Result.Error e -> Lwt.return (Result.Error e)
    ), Vfs.File.command handler

  (* /remotes/<name>/ *)
  let mk_remote ?(url="") name repo =
    let session = Vfs.File.Stream.session None in
    let url_file, remote_url = mk_url url in
    let head_file, update_head = mk_head session in
    let fetch, fetch_file = mk_fetch ~remote_url ~update_head repo in
    let files = [
      Vfs.Inode.file "url"   url_file;
      Vfs.Inode.file "head"  head_file;
      Vfs.Inode.file "fetch" fetch_file;
    ] in
    let s = { remote_name = name; remote_url; update_head; fetch } in
    s, Vfs.Dir.of_list (fun () -> files)

  let has_name name (s, _) = s.remote_name = name
  let find_inode name t = List.find (has_name name) t.state |> snd

  let inode_of_remote t ?url name =
    try find_inode name t
    with Not_found ->
      let s, files = mk_remote ?url name t.repo in
      let inode = Vfs.Inode.dir name files in
      t.state <- (s, inode) :: t.state;
      inode

  let create ?(init=[]) repo =
    Log.debug (fun l -> l "create");
    let t = { root = Vfs.Dir.of_list (fun () -> []); state = []; repo } in
    let ls () = Vfs.ok (List.map snd t.state) in
    let lookup n =
      try Vfs.ok (find_inode n t)
      with Not_found -> Vfs.Dir.err_no_entry
    in
    let mkdir n =
      if List.exists (has_name n) t.state then Vfs.Dir.err_already_exists
      else Vfs.ok (inode_of_remote t n)
    in
    let remove _ = Vfs.Dir.err_dir_only in
    let rename i new_name =
      let old_name = Vfs.Inode.basename i in
      Vfs.Inode.set_basename i new_name;
      t.state <- List.map (fun (s, i as x) ->
          if s.remote_name = old_name then { s with remote_name = new_name }, i
          else x
        ) t.state;
      Vfs.ok ()
    in
    let root = Vfs.Dir.dir_only ~ls ~mkdir ~remove ~rename ~lookup in
    List.iter (fun {name; url} ->
        let (_:Vfs.Inode.t) = inode_of_remote t ?url name in ()
      ) init;
    t.root <- root;
    t

  let root t = t.root

  let add t { name; url } =
    t.state <- List.filter (fun s -> not @@ has_name name s) t.state;
    let (_:Vfs.Inode.t) = inode_of_remote t ?url name in
    ()

  let fetch t ~name ~branch =
    try
      let (s, _) = List.find (has_name name) t.state in
      s.fetch branch
    with Not_found ->
      Vfs.Dir.err_no_entry

end
