open Cmdliner

let socket =
  let doc = Arg.info ~doc:"The socket of the Irmin server" ["--socket"] in
  Arg.(value & opt string "/var/tmp/com.docker.db.socket" doc)

let mnt =
  let doc = Arg.info ~doc:"The destination mount point." [] in
  Arg.(required & pos 0 (some string) None doc)

let mount socket mnt =
  let uid = Unix.getuid () in
  let gid = Unix.getgid () in
  let user = try Unix.((getpwuid uid).pw_name) with Not_found -> "user" in
  let cmd =
    Printf.sprintf
      "mount -t 9p -o trans=unix,name=%s,uname=%s,noextend,nodev,uid=%d,\
       gid=%d,dfltuid=%d,dfltgid=%d %s %s" user user uid gid uid gid socket mnt
  in
  exit (Sys.command cmd)

let term =
  let doc = "Mount an Irmin volume on the filesystem over 9p." in
  let man = [
    `S "DESCRIPTION";
    `P "$(i, irmin-mount) is a small tool to mount Irmin volumes \
        as a 9p mount point."
  ] in
  Term.(pure mount $ socket $ mnt),
  Term.info "irmin-mount" ~version:"0.1" ~doc ~man

let () = match Term.eval term with
  | `Error _ -> exit 1
  | _        -> ()
