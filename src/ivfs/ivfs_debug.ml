open Result

let level s =
  let read () =
    let l = Logs.Src.level s in
    let data = Fmt.strf "%a@." (Fmt.option Logs.pp_level) l in
    Vfs.ok (Some (Cstruct.of_string data))
  in
  let write data =
    let data = String.trim (Cstruct.to_string data) in
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
  let items =
    Logs.Src.list ()
    |> List.map (fun s ->
        Vfs.Inode.dir (Logs.Src.name s) (src s)
      )
  in
  Vfs.Dir.of_list (fun () -> Vfs.ok items)

let fs =
  let dirs = Vfs.ok [
      Vfs.Inode.dir "src" srcs;
    ] in
  Vfs.Dir.of_list (fun () -> dirs)
