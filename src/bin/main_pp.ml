let src = Logs.Src.create "vgithub" ~doc:"Datakit Github bindings"
module Log = (val Logs.src_log src : Logs.LOG)

#ifdef HAVE_GITHUB

let token () =
  let cookie = "datakit" in
  Lwt_unix.run (
    let open Lwt.Infix in
    Github_cookie_jar.init () >>= fun jar ->
    Github_cookie_jar.get jar ~name:cookie >|= function
    | Some t -> Github.Token.of_string t.Github_t.auth_token
    | None   ->
      Printf.eprintf "Missing cookie: use git-jar to create cookie `%s`.\n%!"
        cookie;
      exit 1
  )

let subdirs () =
  Log.debug (fun l -> l "Datakit uses the Github bindings");
  [Vgithub.create token]

#else

let subdirs () =
  Log.debug (fun l -> l "Datakit does not use the Github bindings");
  []

#endif
