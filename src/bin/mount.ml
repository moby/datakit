open Cmdliner

let mopts = "MOUNT OPTIONS"

let host =
  let doc =
    Arg.info ~doc:"The 9p server hostname (or ip)" ~docv:"HOST" ~docs:mopts
      ["h"; "host"]
  in
  Arg.(value & opt string "172.17.0.2" doc)

let port =
  let doc =
    Arg.info ~doc:"The 9p server port"~docv:"PORT" ~docs:mopts  ["p"; "port"]
  in
  Arg.(value & opt int 5640 doc)

let mnt =
  let doc =
    Arg.info ~doc:"The destination mount point." ~docv:"DIR" ~docs:mopts []
  in
  Arg.(value & pos 0 string "/db" doc)

let mount ip port mnt =
  let uid = Unix.getuid () in
  let gid = Unix.getgid () in
  let user = try Unix.((getpwuid uid).pw_name) with Not_found -> "user" in
  if not (Sys.file_exists mnt) then (
    let i = Sys.command (Printf.sprintf "sudo mkdir -p %s" mnt) in
    if i <> 0 then exit i
  );
  let cmd =
    Printf.sprintf
      "sudo mount -t 9p -o trans=tcp,port=%d,name=%s,uname=%s,noextend,nodev,\
       uid=%d,gid=%d,dfltuid=%d,dfltgid=%d %s %s"
      port user user uid gid uid gid ip mnt
  in
  exit (Sys.command cmd)

let term =
  let doc = "Mount a Datakit volume on the filesystem over 9p." in
  let man = [
    `S "DESCRIPTION";
    `P "$(i, datakit-mount) mounts datakit volumes on the local filesystem."
  ] in
  Term.(pure mount $ host $ port $ mnt),
  Term.info "datakit-mount" ~version:"%%VERSION%%" ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
