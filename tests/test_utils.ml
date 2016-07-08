open Astring
open Lwt.Infix
open Result

let () = Printexc.record_backtrace true

let p = function
  | "" -> Datakit_path.empty
  | path -> Datakit_path.of_string_exn path

let v = Cstruct.of_string

let ( ++ ) = Int64.add

let ok x = Lwt.return (Ok x)

let ( >>!= ) x f =
  match x with
  | Ok y -> f y
  | Error vfs_error -> Alcotest.fail (Fmt.to_to_string Vfs.Error.pp vfs_error)

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


  let ok x = `Ok x
  let close _t = Lwt.return_unit
  let write1 t buf = Lwt_mvar.put t.to_remote buf
  let write t buf = write1 t buf >|= ok
  let writev t bufv = Lwt_list.iter_s (write1 t) bufv >|= ok

  let read t = Lwt_mvar.take t.from_remote >|= ok
end

let reporter () =
  let pad n x =
    if String.length x > n then x
    else x ^ String.v ~len:(n - String.length x) (fun _ -> ' ')
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.to_us (Mtime.elapsed ()) in
      Fmt.kpf k ppf ("%0+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Debug));
  Logs.set_reporter (reporter ());
  ()

module Store = Irmin_git.Memory(Ir_io.Sync)(Ir_io.Zlib)
    (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

module Tree = Ivfs_tree.Make(Store)
module RW = Ivfs_rw.Make(Tree)

module Server = Fs9p.Make(Test_flow)
module Filesystem = Ivfs.Make(Store)

let src9p = Logs.Src.create "test9p" ~doc:"Datakit/9p tests"
module Log9p = (val Logs.src_log src9p)

module Client = Protocol_9p.Client.Make(Log9p)(Test_flow)
module DK = Datakit_client_9p.Make(Client)

let quiet_9p () =
  Logs.Src.set_level src9p (Some Logs.Info);
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "fs9p" then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let quiet_git () =
  Logs.Src.set_level src9p (Some Logs.Info);
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "git.value" || Logs.Src.name src = "git.memory"
      then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let quiet_irmin () =
  Logs.Src.set_level src9p (Some Logs.Info);
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "irmin.bc"
      || Logs.Src.name src = "irmin.commit"
      || Logs.Src.name src = "irmin.node"
      then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let expect_head branch =
  DK.Branch.head branch >>*= function
  | None      -> Alcotest.fail "Expecting HEAD"
  | Some head -> ok head

let config = Irmin_mem.config ()

let run fn =
  Lwt_main.run begin
    Store.Repo.create config >>= fun repo ->
    Store.Repo.branches repo >>= fun branches ->
    Lwt_list.iter_s (fun branch ->
      Store.Repo.remove_branch repo branch
      ) branches
    >>= fun () ->
    let for_client, for_server = Test_flow.create () in
    let make_task msg =
      let date = 0L in
      Irmin.Task.create ~date ~owner:"irmin9p" msg
    in
    let root = Filesystem.create make_task repo in
    let server_thread = Server.accept ~root for_server >>*= Lwt.return in
    Lwt.finalize
      (fun () -> Client.connect for_client () >>*= fn repo)
      (fun () -> Lwt.cancel server_thread; Lwt.return ())
  end

let rwx = [`Read; `Write; `Execute]
let rw = [`Read; `Write]
let rx = [`Read; `Execute]
let r = [`Read]
let rwxr_xr_x = Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx ()
let rw_r__r__ = Protocol_9p.Types.FileMode.make ~owner:rw ~group:r ~other:r ()
let symlink = Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx
    ~is_symlink:true ()

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
      ok result
    ) >>*= Lwt.return

let stream conn ?(off=0L) fid =
  let mvar = Lwt_mvar.create_empty () in
  let rec read_line ~saw_flush ~buf ~off =
    match String.cut ~sep:"\n" buf with
    | Some (line, buf) ->
      Lwt_mvar.put mvar (`Line line) >>= fun () ->
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

let create_file ?(perm=rw_r__r__) conn path leaf contents =
  with_file conn path (fun fid ->
      if perm.Protocol_9p.Types.FileMode.is_symlink then (
        Client.LowLevel.create ~extension:contents
          conn fid leaf perm Protocol_9p.Types.OpenMode.write_only
        >>*= fun _resp ->
        Lwt.return_unit
      ) else (
        Client.LowLevel.create
          conn fid leaf perm Protocol_9p.Types.OpenMode.write_only
        >>*= fun _open ->
        Client.LowLevel.write conn fid 0L (Cstruct.of_string contents)
        >>*= fun _resp ->
        Lwt.return_unit
      )
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
      ok ()
    )

let chmod conn path perm =
  let mode =
    match perm with
    | `Normal -> rw_r__r__
    | `Executable -> rwxr_xr_x
    | `Link -> Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx
                 ~is_symlink:true ()
  in
  with_file conn path (fun fid ->
      Client.LowLevel.update ~mode conn fid >>*= Lwt.return
  )

let read_file conn path =
  Client.stat conn path >>*= fun info ->
  let len = info.Protocol_9p.Types.Stat.length |> Int64.to_int32 in
  Client.read conn path 0L len >>*= fun datav ->
  Cstruct.concat datav |> Cstruct.to_string |> Lwt.return

let read_perm conn path =
  Client.stat conn path >>*= fun info ->
  let mode = info.Protocol_9p.Types.Stat.mode in
  if mode.Protocol_9p.Types.FileMode.is_symlink then Lwt.return `Link
  else if List.mem `Execute mode.Protocol_9p.Types.FileMode.owner
  then Lwt.return `Executable
  else Lwt.return `Normal

let check_file conn path msg expected =
  read_file conn path >|= Alcotest.(check string) msg expected

module Perm = struct
  type t = [`Normal | `Executable | `Link]
  let pp fmt = function
    | `Normal -> Fmt.string fmt "Normal"
    | `Executable -> Fmt.string fmt "Executable"
    | `Link -> Fmt.string fmt "Link"
  let equal = (=)
end
let perm = (module Perm : Alcotest.TESTABLE with type t = Perm.t)

let check_perm conn path msg expected =
  read_perm conn path >|= Alcotest.(check perm) msg expected

let echo conn str path =
  Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid path >>*= fun _ ->
      Client.LowLevel.update conn ~length:0L newfid >>*= fun () ->
      Client.LowLevel.openfid
        conn newfid Protocol_9p.Types.OpenMode.write_only >>*= fun _ ->
      Client.LowLevel.write
        conn newfid 0L (Cstruct.of_string (str ^ "\n")) >>*= fun _ ->
      ok ()
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

let head conn branch =
  read_file conn ["branch"; branch; "head"] >|= fun s -> String.trim s

type history_node = {
  id : string;
  msg : string;
  parents : history_node list;
}

let compare_history_node a b =
  match compare a.msg b.msg with
  | 0 -> compare a.id b.id
  | x -> x

(* Get the history starting from [commit] *)
let rec history_client commit =
  DK.Commit.parents commit >>*= fun parents ->
  DK.Commit.message commit >>*= fun msg ->
  Lwt_list.map_s history_client parents >|= fun parents ->
  let parents = List.sort compare_history_node parents in
  { id = DK.Commit.id commit; msg = String.trim msg; parents }

(* Get the history starting from commit [hash] *)
let rec history conn id =
  read_file conn ["snapshots"; id; "parents"] >>= fun parents ->
  read_file conn ["snapshots"; id; "msg"] >>= fun msg ->
  let parents = Str.(split (regexp "\n")) parents in
  Lwt_list.map_s (history conn) parents >|= fun parents ->
  let parents = List.sort compare_history_node parents in
  { id; msg = String.trim msg; parents }

let rec pp_history fmt {id; msg; parents} =
  Format.fprintf fmt "@[<v2>%s (%s)@\n%a@]"
    id msg (Format.pp_print_list pp_history) parents

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

let populate_client branch files =
  DK.Branch.with_transaction branch (fun t ->
      DK.Transaction.read_dir t (p "") >>*= fun existing ->
      existing |> Lwt_list.iter_s (fun name ->
          DK.Transaction.remove t (p name) >>*= Lwt.return
        ) >>= fun () ->
      DK.Transaction.read_dir t (p "") >>*= fun existing ->
      Alcotest.(check (list string)) "rw is empty" [] existing;
      let dirs = Hashtbl.create 2 in
      let rec ensure_dir d =
        if Hashtbl.mem dirs d then Lwt.return_unit
        else (
          match Irmin.Path.String_list.rdecons d with
          | None -> Lwt.return_unit
          | Some (parent, name) ->
            ensure_dir parent >>= fun () ->
            Hashtbl.add dirs d ();
            DK.Transaction.create_dir t ~dir:(Datakit_path.of_steps_exn parent)
              name
            >>*= Lwt.return
        ) in
      files |> Lwt_list.iter_s (fun (path, value) ->
          match
            Irmin.Path.String_list.of_hum path |> Irmin.Path.String_list.rdecons
          with
          | None -> assert false
          | Some (dir, name) ->
            ensure_dir dir >>= fun () ->
            let dir = Datakit_path.of_steps_exn dir in
            DK.Transaction.create_file t ~dir name (Cstruct.of_string value)
            >>*= Lwt.return
        ) >>= fun () ->
      DK.Transaction.commit t ~message:"init"
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

let try_merge_client dk ~base ~ours ~theirs fn =
  DK.branch dk "master" >>*= fun master ->
  populate_client master base >>*= fun () ->
  expect_head master >>*= fun base_head ->
  DK.branch dk "theirs" >>*= fun their_branch ->
  DK.Branch.fast_forward their_branch base_head >>*= fun () ->
  populate_client master ours >>*= fun () ->
  populate_client their_branch theirs >>*= fun () ->
  DK.Branch.with_transaction master (fun t ->
      expect_head their_branch >>*= fun theirs_head ->
      DK.Transaction.merge t theirs_head >>*= fun (merge, _conflicts) ->
      fn t merge >>*= fun () ->
      DK.Transaction.commit t ~message:"try_merge_client"
    )
  >>*= Lwt.return

let vfs_error = Alcotest.of_pp Vfs.Error.pp
let vfs_result ok = Alcotest.result ok vfs_error

let reject (type v) =
  let module T = struct
    type t = v
    let pp fmt _ = Fmt.string fmt "reject-all"
    let equal _ _ = false
  end in
  (module T : Alcotest.TESTABLE with type t = v)

let file_event =
  let module T = struct
    type t = [
      | `File of Cstruct.t
      | `Link of string
      | `Exec of Cstruct.t
      | `Dir of DK.Tree.t
    ]
    let pp f = function
      | `File contents -> Fmt.pf f "File:%s" (Cstruct.to_string contents)
      | `Exec contents -> Fmt.pf f "Exec:%s" (Cstruct.to_string contents)
      | `Link target   -> Fmt.pf f "Link:%s" target
      | `Dir _         -> Fmt.pf f "Dir"
    let equal a b =
      match a, b with
      | `File a, `File b -> Cstruct.equal a b
      | `Exec a, `Exec b -> Cstruct.equal a b
      | `Link a, `Link b -> String.equal a b
      | _ -> false
      (* (note: can't compare trees easily, so always false *)
  end in
  (module T : Alcotest.TESTABLE with type t = T.t)

let commit =
  let module T = struct
    type t = DK.Commit.t
    let pp fmt c = Fmt.string fmt (DK.Commit.id c)
    let equal a b = (DK.Commit.id a = DK.Commit.id b)
  end in
  (module T : Alcotest.TESTABLE with type t = DK.Commit.t)

let compare_commit a b =
  String.compare (DK.Commit.id a) (DK.Commit.id b)
