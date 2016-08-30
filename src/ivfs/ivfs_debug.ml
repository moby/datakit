open Result

let level s =
  (* [empty] should really be per-open-file, but no easy way to do that. *)
  let empty = ref false in
  let read () =
    if !empty then Vfs.ok (Some (Cstruct.create 0))
    else (
      let l = Logs.Src.level s in
      Vfs.ok (Some (Cstruct.of_string (Logs.level_to_string l ^ "\n")))
    )
  in
  let write data =
    match String.trim (Cstruct.to_string data) with
    | "" -> empty := true; Vfs.ok ()
    | data ->
      empty := false;
      match Logs.level_of_string data with
      | Ok l ->
        Logs.Src.set_level s l;
        Vfs.ok ()
      | Error (`Msg msg) -> Vfs.error "%s" msg
  in
  let chmod _ = Lwt.return Vfs.Error.perm in
  let remove () = Lwt.return Vfs.Error.perm in
  Vfs.File.of_kv ~read ~write ~stat:(Vfs.File.stat_of ~read) ~remove ~chmod

let src s =
  let items = [
      Vfs.Inode.file "doc" (Vfs.File.ro_of_string (Logs.Src.doc s ^ "\n"));
      Vfs.Inode.file "level" (level s);
    ] in
  Vfs.Dir.of_list (fun () -> Vfs.ok items)

let srcs =
  let logs = Hashtbl.create 100 in
  let get_dir s =
    let name = Logs.Src.name s in
    try Hashtbl.find logs name
    with Not_found ->
      let dir = Vfs.Inode.dir name (src s) in
      Hashtbl.add logs name dir;
      dir
  in
  Vfs.Dir.of_list (fun () -> Logs.Src.list () |> List.map get_dir |> Vfs.ok)

let fs =
  let dirs = Vfs.ok [
      Vfs.Inode.dir "src" srcs;
    ] in
  Vfs.Dir.of_list (fun () -> dirs)
