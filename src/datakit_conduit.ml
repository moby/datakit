open Lwt.Infix
open Result
open Astring

module UnixServer = Fs9p.Make(Flow_lwt_unix)
module HyperVServer = Fs9p.Make(Flow_lwt_unix_hvsock)

let src = Logs.Src.create "conduit" ~doc:"Datakit conduit handling"
module Log = (val Logs.src_log src : Logs.LOG)

module Pervasives = struct
  module Unix = Unix
end

type t = [
  | `NamedPipe of string
  | `Fd of int
  | `File of string
  | `Tcp of string * int
  | `HyperV_connect of Uri.t
  | `HyperV_accept of Uri.t
]

let parse ~default_tcp_port str: [`Ok of t | `Error of string] =
  let named_pipe _ = `Ok (`NamedPipe str) in
  let fd i = match String.to_int i with
    | None ->
      let err = Fmt.strf "Failed to parse command-line argument [%s]" str in
      `Error err
    | Some x -> `Ok (`Fd x)
  in
  let file f = `Ok (`File f) in
  let tcp s =
    match String.cut ~rev:true ~sep:":" s with
    | None              -> `Ok (`Tcp (s, default_tcp_port))
    | Some (host, port) ->
      try `Ok (`Tcp (host, int_of_string port))
      with Failure _ -> `Error "use tcp://host:port"
  in
  let hyperv_connect _ = `Ok (`HyperV_connect (Uri.of_string str)) in
  let hyperv_accept _  = `Ok (`HyperV_accept (Uri.of_string str)) in
  let choices = [
    "\\\\"             , named_pipe;
    "fd://"            , fd;
    "file://"          , file;
    "tcp://"           , tcp;
    "hyperv-connect://", hyperv_connect;
    "hyperv-accept://" , hyperv_accept;
  ] in
  List.fold_left (fun acc (s, f) ->
      match acc with
      | Some s -> Some s
      | None   ->
        match String.cut ~sep:s str with
        | Some ("", s) -> Some (f s)
        | _            -> None
    ) None choices
  |> function
  | None   -> `Error "invalid endpoint"
  | Some t -> t

let pp ppf = function
  | `NamedPipe p      -> Fmt.string ppf p
  | `Fd i             -> Fmt.pf ppf "fd://%d" i
  | `File f           -> Fmt.pf ppf "file://%s" f
  | `Tcp (h, p)       -> Fmt.pf ppf "tcp://%s:%d" h p
  | `HyperV_connect u -> Uri.pp_hum ppf u
  | `HyperV_accept u  -> Uri.pp_hum ppf u

let str = Fmt.to_to_string pp

module Unix = struct

  let of_fd x =
    (* Ideally we would use something like unix-type-representations for this,
       but unfortunately that library will refuse to install on Win32. *)
    let file_descr_of_int (x: int) : Unix.file_descr = Obj.magic x in
    if Sys.os_type <> "Unix"
    then Lwt.fail (Failure "Inheriting a listening socket is only supported on Unix")
    else Lwt.return (file_descr_of_int x)

  let of_path path =
    Lwt.catch
      (fun () -> Lwt_unix.unlink path)
      (function
        | Unix.Unix_error(Unix.ENOENT, _, _) -> Lwt.return ()
        | e -> Lwt.fail e)
    >>= fun () ->
    let s = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    Lwt_unix.Versioned.bind_2 s (Lwt_unix.ADDR_UNIX path) >>= fun () ->
    Lwt.return s

  let handle ~make_root t fd =
    let msg = str t in
    Lwt.catch (fun () ->
         let flow = Flow_lwt_unix.connect fd in
         (* Re-build the filesystem for each client because command files
            need per-client state. *)
         let root = make_root () in
         UnixServer.accept ~root ~msg flow >|= function
         | Error (`Msg msg) ->
           Log.debug (fun l -> l "Error handling client connection: %s" msg)
         | Ok () -> ()
      ) (fun e ->
          Log.err (fun l ->
              l "Caught %s: closing connection" (Printexc.to_string e));
          Lwt.return_unit
        )

  let accept_forever ?(backlog=128) socket callback =
    Lwt_unix.listen socket backlog;
    let rec aux () =
      Lwt_unix.accept socket >>= fun (client, _addr) ->
      let _ = (* background thread *)
        (* the callback will close the connection when its done *)
        callback client in
      aux ()
    in aux ()

end

module HyperV = struct

  let of_uri ~serviceid uri =
    (* hyperv://vmid/servivceid *)
    let vmid = match Uri.host uri with
      | None   -> Hvsock.Loopback
      | Some x -> Hvsock.Id x
    in
    let serviceid =
      let p = Uri.path uri in
      if p = "" then serviceid
      else
        (* trim leading / *)
        String.drop ~sat:((=) '/') ~max:1 p
    in
    { Hvsock.vmid; serviceid }

  let handle ~make_root uri fd =
    Log.debug (fun l -> l "New Hyper-V client");
    Lwt.catch
      (fun () ->
         let flow = Flow_lwt_unix_hvsock.connect fd in
         (* Re-build the filesystem for each client because command files
            need per-client state. *)
         let root = make_root () in
         HyperVServer.accept ~root ~msg:(Uri.to_string uri) flow >|= function
         | Error (`Msg msg) ->
           Log.debug (fun l -> l "Error handling client connection: %s" msg)
         | Ok () -> ()
      ) (fun e ->
          Log.err (fun l ->
              l "Caught %s: closing connection" (Printexc.to_string e)
            );
          Lwt.return ()
        )

  let accept_forever ?(backlog=128) socket callback =
    Flow_lwt_unix_hvsock.Hvsock.listen socket backlog;
    let rec aux () =
      Flow_lwt_unix_hvsock.Hvsock.accept socket >>= fun (client, _addr) ->
      let _ = (* background thread *)
        (* the callback will close the connection when its done *)
        callback client in
      aux ()
    in aux ()

  let connect_forever sockaddr callback =
    let rec aux () =
      let socket = Flow_lwt_unix_hvsock.Hvsock.create () in
      Lwt.catch
        (fun () ->
           Flow_lwt_unix_hvsock.Hvsock.connect socket sockaddr >>= fun () ->
           callback socket
        ) (fun _e ->
            Flow_lwt_unix_hvsock.Hvsock.close socket >>= fun () ->
            Lwt_unix.sleep 1.
          )
      >>= fun () ->
      aux ()
    in aux ()

end

module Named_pipe = struct

  open Lwt.Infix

  let with_connect p ~path f =
    Lwt.catch
      (fun () -> Named_pipe_lwt.Server.connect p >>= f)
      (fun e ->
         Log.err
           (fun f -> f "Named-pipe connection failed on %s: %a" path Fmt.exn e);
         Lwt.return ())

  let rec accept_forever ?backlog path callback =
    let p = Named_pipe_lwt.Server.create path in
    with_connect p ~path (fun () ->
        Lwt.async (fun () ->  (* background thread *)
            let fd = Named_pipe_lwt.Server.to_fd p in
            callback fd
          );
        Lwt.return_unit
      ) >>= fun () ->
    accept_forever ?backlog path callback

end

let accept_forever ?backlog ~serviceid ~make_root t =
  let accept () = match t with
    | `NamedPipe path ->
      Named_pipe.accept_forever path (Unix.handle ~make_root t)
    | `Fd fd ->
      Unix.of_fd fd >>= fun socket ->
      let socket' = Lwt_unix.of_unix_file_descr socket in
      Unix.accept_forever ?backlog socket' (Unix.handle ~make_root t)
    | `File path ->
      Unix.of_path path >>= fun socket ->
      Unix.accept_forever ?backlog socket (Unix.handle ~make_root t)
    | `Tcp (host, port) ->
      let addr =
        Lwt_unix.ADDR_INET (Pervasives.Unix.inet_addr_of_string host, port)
      in
      let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
      (* Makes testing easier *)
      Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
      Lwt_unix.Versioned.bind_2 socket addr >>= fun () ->
      Unix.accept_forever ?backlog socket (Unix.handle ~make_root t)
    | `HyperV_connect uri ->
      HyperV.connect_forever (HyperV.of_uri ~serviceid uri)
        (HyperV.handle ~make_root uri)
    | `HyperV_accept uri ->
      let socket = Flow_lwt_unix_hvsock.Hvsock.create () in
      Flow_lwt_unix_hvsock.Hvsock.bind socket (HyperV.of_uri ~serviceid uri);
      HyperV.accept_forever ?backlog socket (HyperV.handle ~make_root uri)
  in
  Lwt.catch accept
    (fun e ->
       Log.err (fun l -> l "Failed to set up server socket listening on %a: %a"
                   pp t Fmt.exn e);
    Lwt.fail_with "accept_forever")
