open Lwt.Infix

let err_no_head =  Vfs.error "error: no head to fetch"
let err_fetch_error = Vfs.error "error: cannot fetch %s"
let err_no_url = Vfs.error "error: remote url is not defined"

module Make (Store : I9p_tree.STORE) = struct

  module Sync = Irmin.Sync(Store)

  type t = {
    remote_url : unit -> string option;
    update_head: string option -> unit;
  }

  let mk_head () =
    let stream, push = Lwt_stream.create () in
    let current : string option ref = ref None in
    let wait old =
      if old <> !current then Lwt.return !current
      else Lwt_stream.next stream >|= fun s -> current := s; s
    in
    let pp ppf = function
      | None      -> Fmt.string ppf ""
      | Some hash -> Fmt.pf ppf "%s\n" hash
    in
    let stream () = Vfs.File.Stream.watch pp ~init:!current ~wait in
    let file = Vfs.File.of_stream @@ fun () -> Lwt.return (stream ()) in
    file, fun x -> push (Some x)

  (* /remotes/<name>/url *)
  let mk_url default =
    let file, fn = Vfs.File.rw_of_string default in
    file, (function () -> match fn () with "" -> None | s -> Some s)

  (* /remotes/<name>/fetch *)
  let mk_fetch t make_task repo =
    let line h = Store.Hash.to_hum h ^ "\n" in
    let handler branch =
      match t.remote_url () with
      | None     -> err_no_url
      | Some url ->
        let r = Irmin.remote_uri url in
        Store.of_branch_id make_task branch repo >>= fun s ->
        Sync.fetch (s "fetch") r >>= function
        | `No_head -> err_no_head
        | `Error   -> err_fetch_error url
        | `Head h  -> t.update_head (Some (line h)); Vfs.ok ""
    in
    Vfs.File.command handler

  (* /remotes/<name>/ *)
  let mk_remote ?(url="") make_task repo =
    let url_file, remote_url = mk_url url in
    let head_file, update_head = mk_head () in
    let t = { remote_url; update_head } in
    let fetch_file = mk_fetch t make_task repo in
    let files = [
      Vfs.Inode.file "url" url_file;
      Vfs.Inode.file "head" head_file;
      Vfs.Inode.file "fetch" fetch_file;
    ] in
    Vfs.Dir.of_list (fun () -> files)

  let create ?(init=[]) make_task repo =
    let remote ?url name = Vfs.Inode.dir name (mk_remote ?url make_task repo) in
    let init = List.map (fun (name, url) -> name, remote ~url name) init in
    let remotes = ref init in
    let ls () = Vfs.ok (List.map snd !remotes) in
    let lookup n =
      try Vfs.ok (List.assoc n !remotes)
      with Not_found -> Vfs.Dir.err_enoent
    in
    let mkdir n =
      if List.mem_assoc n !remotes then Vfs.Dir.err_already_exists
      else
        let i = remote n in
        remotes := (n, i) :: !remotes;
        Vfs.ok ( i)
    in
    let remove _ = Vfs.Dir.err_dir_only in
    let rename i new_name =
      let old_name = Vfs.Inode.basename i in
      Vfs.Inode.set_basename i new_name;
      let rs = List.filter (fun (n,_) -> n <>old_name) !remotes in
      remotes := (new_name, i) :: rs;
      Vfs.ok ()
    in
    Vfs.Dir.dir_only ~ls ~mkdir ~remove ~rename ~lookup

end
