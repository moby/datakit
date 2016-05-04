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

#ifdef HAVE_NAMED_PIPE

let rec named_pipe_accept_forever path callback =
  let open Lwt.Infix in
  let p = Named_pipe_lwt.Server.create path in
  Named_pipe_lwt.Server.connect p
  >>= function
  | false ->
    Log.err (fun f -> f "Named-pipe connection failed on %s" path);
    Lwt.return ()
  | true ->
    let _ = (* background thread *)
      let fd = Named_pipe_lwt.Server.to_fd p in
      callback fd
      >>= fun () ->
      Named_pipe_lwt.Server.disconnect p;
      Named_pipe_lwt.Server.destroy p;
      Lwt.return () in
    named_pipe_accept_forever path callback

#else

let named_pipe_accept_forever _ _ = failwith "Not linked"
#endif
