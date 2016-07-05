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

let src = Logs.Src.create "vgithub-9p-hack" ~doc:"Vgithub 9p hack access"
module Log9p = (val Logs.src_log src : Logs.LOG)
module Client9p = Client9p_unix.Make(Log9p)
module DK = Datakit_client_9p.Make(Client9p)

let vgithub_hack () =
  let address =
    "/var/tmp/datakit" ^ (string_of_int @@ Random.int 1024) ^ ".sock"
  in
  (* FIXME(samoht): file vs. unix is so confusing! *)
  let listen_url = "file://" ^ address in
  let proto = "unix" in
  let open Result in
  let open Lwt.Infix in
  let client () =
    Log9p.debug (fun l -> l "connect %s %s" proto address);
    Client9p.connect proto address () >|= function
    | Error (`Msg e) ->
      Log.info (fun l -> l "Vgithub hack cannot connect: %s" e);
      Error "vgithub hack"
    | Ok conn        ->
      let dk = DK.connect conn in
      Ok (Vgithub.Hack.E ((module DK), dk))
    in
    Vgithub.Hack.init client;
    [listen_url]

#else

let subdirs () =
  Log.debug (fun l -> l "Datakit does not use the Github bindings");
  []

let vgithub_hack _ = []

#endif
