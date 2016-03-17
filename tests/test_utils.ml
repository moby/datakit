open Lwt.Infix
open Result

let () = Printexc.record_backtrace true

let ( ++ ) = Int64.add

let ok x = `Ok x

let ( >>*= ) x f = x >>= function
  | Ok y -> f y
  | Error (`Msg msg) -> Alcotest.fail msg

let ( >>**= ) x f = x >>= function
  | Ok y -> f y
  | Error _ as e -> Lwt.return e

let fd_stderr = Unix.descr_of_out_channel stderr
let real_stderr = Unix.dup fd_stderr
let () =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := (fun ex ->
      Unix.dup2 real_stderr fd_stderr;
      Printf.eprintf "\nasync_exception_hook:\n%!";
      old_hook ex
    )

module Test_flow = struct
  type error = {zero : 'a. 'a}
  type buffer = Cstruct.t
  type 'a io = 'a Lwt.t
  let error_message e = e.zero

  type flow = {
    from_remote : Cstruct.t Lwt_mvar.t;
    to_remote : Cstruct.t Lwt_mvar.t;
  }

  let create () =
    let a = Lwt_mvar.create_empty () in
    let b = Lwt_mvar.create_empty () in
    let flow1 = { from_remote = a; to_remote = b } in
    let flow2 = { from_remote = b; to_remote = a } in
    (flow1, flow2)

  let close _t = Lwt.return_unit
  let write1 t buf = Lwt_mvar.put t.to_remote buf
  let write t buf = write1 t buf >|= ok
  let writev t bufv = Lwt_list.iter_s (write1 t) bufv >|= ok

  let read t = Lwt_mvar.take t.from_remote >|= ok
end

let log = ref []

module Log : Protocol_9p.S.LOG = struct
  let append s = log := s :: !log
  let debug fmt = Fmt.kstrf append fmt
  let info fmt = Fmt.kstrf (fun s -> append ("info: " ^ s)) fmt
  let error fmt = Fmt.kstrf (fun s -> print_endline s; append s) fmt
end
module Store = Irmin_git.Memory(Ir_io.Sync)(Ir_io.Zlib)
    (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

module Server = Fs9p.Make(Log)(Test_flow)
module Filesystem = I9p.Make(Store)

module Client = Protocol_9p.Client.Make(Log)(Test_flow)

let config = Irmin_mem.config ()

let run fn =
  Lwt_main.run begin
    Store.Repo.create config >>= fun repo ->
    Store.Repo.remove_branch repo "master" >>= fun () ->
    let for_client, for_server = Test_flow.create () in
    let make_task msg =
      let date = 0L in
      Irmin.Task.create ~date ~owner:"irmin9p" msg
    in
    let root = Filesystem.create make_task repo in
    let server_thread = Server.accept ~root for_server >>*= Lwt.return in
    Lwt.finalize
      (fun () ->
         Lwt.catch
           (fun () -> Client.connect for_client () >>*= fn repo)
           (fun ex ->
              List.rev !log |> List.iter print_endline;
              Lwt.fail ex))
      (fun () -> Lwt.cancel server_thread; Lwt.return ())
  end

let rwx = [`Read; `Write; `Execute]
let rw = [`Read; `Write]
let rx = [`Read; `Execute]
let r = [`Read]
let rwxr_xr_x = Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx ()
let rw_r__r__ = Protocol_9p.Types.FileMode.make ~owner:rw ~group:r ~other:r ()

let check_dir conn path msg expected =
  Client.readdir conn path >>*= fun items ->
  List.map (fun stat -> stat.Protocol_9p.Types.Stat.name) items
  |> Alcotest.(check (slist string String.compare)) msg expected;
  Lwt.return_unit

let with_file conn path fn =
  Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid path >>*= fun _resp ->
      fn newfid >>= fun result ->
      Client.LowLevel.clunk conn newfid >>*= fun () ->
      Lwt.return (Ok result)
    ) >>*= Lwt.return

let stream conn ?(off=0L) fid =
  let mvar = Lwt_mvar.create_empty () in
  let rec read_line ~saw_flush ~buf ~off =
    let i =
      try Some (String.index buf '\n')
      with Not_found -> None in
    match i with
    | Some i ->
      let line = String.sub buf 0 i in
      Lwt_mvar.put mvar (`Line line) >>= fun () ->
      let buf = String.sub buf (i + 1) (String.length buf - i - 1) in
      read_line ~saw_flush ~buf ~off
    | None ->
      Client.LowLevel.read conn fid off 256l >>*= fun resp ->
      match Cstruct.to_string resp.Protocol_9p.Response.Read.data with
      | "" when saw_flush -> Lwt_mvar.put mvar `Eof
      | "" -> read_line ~saw_flush:true ~buf ~off
      | data ->
        read_line
          ~saw_flush:false
          ~buf:(buf ^ data)
          ~off:(off ++ Int64.of_int (String.length data)) in
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> read_line ~saw_flush:false ~buf:"" ~off)
        (fun ex -> Lwt_mvar.put mvar (`Line (Printexc.to_string ex)))
    );
  fun () -> Lwt_mvar.take mvar

let with_stream conn path fn =
  with_file conn path (fun fid ->
      Client.LowLevel.openfid conn fid Protocol_9p.Types.OpenMode.read_only
      >>*= fun _resp ->
      fn (stream conn fid)
    )

let read_line_exn stream =
  stream () >|= function
  | `Line l -> l
  | `Eof    -> Alcotest.fail "Unexpected end-of-stream"

let create_file conn path leaf contents =
  with_file conn path (fun fid ->
      Client.LowLevel.create
        conn fid leaf rw_r__r__ Protocol_9p.Types.OpenMode.write_only
      >>*= fun _open ->
      Client.LowLevel.write conn fid 0L (Cstruct.of_string contents)
      >>*= fun _resp ->
      Lwt.return_unit
    )

let write_file conn ?(truncate=false) path contents =
  with_file conn path (fun fid ->
      begin
        if not truncate then Lwt.return_unit
        else Client.LowLevel.update ~length:0L conn fid >>*= Lwt.return
      end >>= fun () ->
      Client.LowLevel.openfid conn fid Protocol_9p.Types.OpenMode.write_only
      >>**= fun _open ->
      Client.LowLevel.write conn fid 0L (Cstruct.of_string contents)
      >>**= fun _resp ->
      Lwt.return (Ok ())
    )

let read_file conn path =
  Client.stat conn path >>*= fun info ->
  let len = info.Protocol_9p.Types.Stat.length |> Int64.to_int32 in
  Client.read conn path 0L len >>*= fun datav ->
  Cstruct.concat datav |> Cstruct.to_string |> Lwt.return

let check_file conn path msg expected =
  read_file conn path >|= Alcotest.(check string) msg expected

let echo conn str path =
  Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid path >>*= fun _ ->
      Client.LowLevel.update conn ~length:0L newfid >>*= fun () ->
      Client.LowLevel.openfid
        conn newfid Protocol_9p.Types.OpenMode.write_only >>*= fun _ ->
      Client.LowLevel.write
        conn newfid 0L (Cstruct.of_string (str ^ "\n")) >>*= fun _ ->
      Lwt.return (Ok ())
    ) >>*= Lwt.return

let with_transaction conn ~branch name fn =
  let path = ["branch"; branch; "transactions"] in
  Client.mkdir conn path name rwxr_xr_x >>*= fun () ->
  let path = path @ [name] in
  write_file conn (path @ ["msg"]) name >>*= fun () ->
  Lwt.try_bind
    (fun () -> fn path)
    (fun r ->
       write_file conn (path @ ["ctl"]) "commit" >>*= fun () ->
       Lwt.return r)
    (fun ex ->
       Lwt.try_bind
         (fun () -> write_file conn (path @ ["ctl"]) "close" >>*= Lwt.return)
         (fun () -> Lwt.fail ex)
         (fun ex2 ->
            let err =
              Printf.sprintf
                "Error trying to close transaction:\n%s\n\
                 ... in response to error:\n%s)"
                (Printexc.to_string ex2) (Printexc.to_string ex)
            in
            failwith err))

let head conn branch = read_file conn ["branch"; branch; "head"] >|= String.trim

type history = Commit of string * history list

(* Get the history starting from commit [hash] *)
let rec history conn hash =
  read_file conn ["snapshots"; hash; "parents"] >>= fun parents ->
  let parents = Str.(split (regexp "\n")) parents in
  Lwt_list.map_s (history conn) parents >|= fun histories ->
  Commit (hash, histories)

let rec pp_history fmt (Commit (hash, parents)) =
  Format.fprintf fmt "@[<v2>%s@\n%a@]"
    hash (Format.pp_print_list pp_history) parents

(* Create a new branch. If [src] is given, use this as the starting commit. *)
let make_branch conn ?src name =
  Client.mkdir conn ["branch"] name rwxr_xr_x >>*= fun () ->
  let path = ["branch"; name] in
  match src with
  | None -> Lwt.return ()
  | Some src -> write_file conn (path @ ["fast-forward"]) src >>*= Lwt.return

(* Replace the files currently on [branch] with [files]. *)
let populate conn ~branch files =
  with_transaction conn ~branch "init" (fun t ->
      Client.readdir conn (t @ ["rw"]) >>*= fun existing ->
      existing |> Lwt_list.iter_s (fun stat ->
          let name = stat.Protocol_9p_types.Stat.name in
          Client.remove conn (t @ ["rw"; name]) >>*= Lwt.return
        ) >>= fun () ->
      Client.readdir conn (t @ ["rw"]) >>*= fun existing ->
      existing
      |> List.map (fun e -> e.Protocol_9p_types.Stat.name)
      |> Alcotest.(check (list string)) "rw is empty" [];
      let dirs = Hashtbl.create 2 in
      let rec ensure_dir d =
        if Hashtbl.mem dirs d then Lwt.return_unit
        else (
          match Irmin.Path.String_list.rdecons d with
          | None -> Lwt.return_unit
          | Some (parent, name) ->
            ensure_dir parent >>= fun () ->
            Hashtbl.add dirs d ();
            Client.mkdir conn (t @ ["rw"] @ parent) name rwxr_xr_x >>*=
            Lwt.return
        ) in
      files |> Lwt_list.iter_s (fun (path, value) ->
          match
            Irmin.Path.String_list.of_hum path |> Irmin.Path.String_list.rdecons
          with
          | None -> assert false
          | Some (dir, name) ->
            ensure_dir dir >>= fun () ->
            let dir = t @ ["rw"] @ dir in
            create_file conn dir name value
        )
    )

(* Commit [base] to master. Then fork a branch which replaces this
   with [theirs].  Replace [base] with [ours] on master, then merge
   [theirs]. Calls [fn trans] on the transaction after irmin9p has
   done its part and is waiting for us to resolve the remaining
   conflicts manually. *)
let try_merge conn ~base ~ours ~theirs fn =
  make_branch conn "master" >>= fun () ->
  populate conn ~branch:"master" base >>= fun () ->
  head conn "master" >>= fun base_head ->
  make_branch conn ~src:base_head "theirs" >>= fun () ->
  populate conn ~branch:"master" ours >>= fun () ->
  populate conn ~branch:"theirs" theirs >>= fun () ->
  with_transaction conn ~branch:"master" "merge" (fun t ->
      head conn "theirs" >>= fun theirs_head ->
      write_file conn (t @ ["merge"]) theirs_head >>*= fun () ->
      fn t
    )
