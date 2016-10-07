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
    Lwt_unix.bind s (Lwt_unix.ADDR_UNIX path);
    Lwt.return s

  let handle ~make_root fd =
    Log.debug (fun l -> l "New unix client");
    Lwt.catch
      (fun () ->
         let flow = Flow_lwt_unix.connect fd in
         (* Re-build the filesystem for each client because command files
            need per-client state. *)
         let root = make_root () in
         UnixServer.accept ~root flow >|= function
         | Error (`Msg msg) ->
           Log.debug (fun l -> l "Error handling client connection: %s" msg)
         | Ok () -> ()
      ) (fun e ->
          Log.err (fun l ->
              l "Caught %s: closing connection" (Printexc.to_string e));
          Lwt.return ()
        )

  let accept_forever ?(backlog=128) url socket callback =
    Lwt_unix.listen socket backlog;
    let rec aux () =
      Lwt_unix.accept socket >>= fun (client, _addr) ->
      let _ = (* background thread *)
        (* the callback will close the connection when its done *)
        callback client in
      aux () in
    Log.info
      (fun l -> l "Waiting for connections on Unix socket %a" Uri.pp_hum url);
    aux ()

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

  let handle ~make_root fd =
    Log.debug (fun l -> l "New Hyper-V client");
    Lwt.catch
      (fun () ->
         let flow = Flow_lwt_unix_hvsock.connect fd in
         (* Re-build the filesystem for each client because command files
            need per-client state. *)
         let root = make_root () in
         HyperVServer.accept ~root flow >|= function
         | Error (`Msg msg) ->
           Log.debug (fun l -> l "Error handling client connection: %s" msg)
         | Ok () -> ()
      ) (fun e ->
          Log.err (fun l ->
              l "Caught %s: closing connection" (Printexc.to_string e)
            );
          Lwt.return ()
        )

  let accept_forever ?(backlog=128) url socket callback =
    Flow_lwt_unix_hvsock.Hvsock.listen socket backlog;
    let rec aux () =
      Flow_lwt_unix_hvsock.Hvsock.accept socket >>= fun (client, _addr) ->
      let _ = (* background thread *)
        (* the callback will close the connection when its done *)
        callback client in
      aux () in
    Log.info
      (fun l -> l "Waiting for connections on hv socket %a" Uri.pp_hum url);
    aux ()

  let connect_forever url sockaddr callback =
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
      aux () in
    Log.info
      (fun l -> l "Waiting for connections on hv socket %a" Uri.pp_hum url);
    aux ()

end

module Named_pipe = struct

  let rec accept_forever ?backlog path callback =
    let open Lwt.Infix in
    let p = Named_pipe_lwt.Server.create path in
    Named_pipe_lwt.Server.connect p >>= function
    | false ->
      Log.err (fun f -> f "Named-pipe connection failed on %s" path);
      Lwt.return ()
    | true ->
      let _ = (* background thread *)
        let fd = Named_pipe_lwt.Server.to_fd p in
        callback fd
      in
      accept_forever ?backlog path callback

end

let default d = function
  | Some x -> x
  | None -> d

let accept_forever ?backlog ~sandbox ~serviceid ~make_root url =
  Lwt.catch
    (fun () ->
       (* Check if it looks like a UNC name before a URI *)
       if Astring.String.is_prefix ~affix:"\\\\" url then begin
         Log.info (fun f -> f "Accepting connections on named pipe %s" url);
         Named_pipe.accept_forever url (Unix.handle ~make_root)
       end else if String.is_prefix ~affix:"fd:" url then begin
         let i = String.with_range ~first:3 url in
         ( match String.to_int i with
           | None ->
             let err =
               Fmt.strf "Failed to parse command-line argument [%s]" url
             in
             Lwt.fail_with err
           | Some x -> Lwt.return x
         ) >>= fun x ->
         Unix.of_fd x >>= fun socket ->
         let socket' = Lwt_unix.of_unix_file_descr socket in
         Unix.accept_forever ?backlog
           (Uri.of_string url) socket' (Unix.handle ~make_root)
       end else
         let uri = Uri.of_string url in
         match Uri.scheme uri with
         | Some "file" ->
           let prefix = if sandbox then "." else "" in
           Unix.of_path (prefix ^ Uri.path uri) >>= fun socket ->
           Unix.accept_forever ?backlog uri socket (Unix.handle ~make_root)
         | Some "tcp" ->
           begin match Uri.path uri with
             | "" | "/" -> ()
             | path ->
               Printf.fprintf stderr
                 "tcp address should not have a path component (path=%S) - \
                  use tcp://addr:port" path;
               exit 1;
           end;
           let host = Uri.host uri |> default "127.0.0.1" in
           let port = Uri.port uri |> default 5640 in
           let addr =
             Lwt_unix.ADDR_INET (Pervasives.Unix.inet_addr_of_string host, port)
           in
           let socket = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
           (* Makes testing easier *)
           Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
           Lwt_unix.bind socket addr;
           Unix.accept_forever ?backlog uri socket (Unix.handle ~make_root)
         | Some "hyperv-connect" ->
           HyperV.connect_forever uri
             (HyperV.of_uri ~serviceid uri) (HyperV.handle ~make_root)
         | Some "hyperv-accept" ->
           let socket = Flow_lwt_unix_hvsock.Hvsock.create () in
           Flow_lwt_unix_hvsock.Hvsock.bind socket (HyperV.of_uri ~serviceid uri);
           HyperV.accept_forever ?backlog uri socket (HyperV.handle ~make_root)
         | _ ->
           Printf.fprintf stderr
             "Unknown URL schema. Please use file: or tcp:\n";
           exit 1)
    (fun ex ->
       Printf.fprintf stderr
         "Failed to set up server socket listening on %S: %s\n%!"
         url (Printexc.to_string ex);
       exit 1)
