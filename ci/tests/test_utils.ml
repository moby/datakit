module Maker = Irmin_git.Mem.Make (Datakit_io.IO) (Git.Inflate.M)
module Store = Datakit.Make_git (Maker)
open Datakit_github
open Result
open Lwt.Infix
open! Astring
open Datakit_ci
open Datakit_client

let ( / ) = Path.Infix.( / )

(* Chain operations together, returning early if we get an error *)
let ( >>*= ) x f =
  x >>= function
  | Ok x -> f x
  | Error e -> Lwt.fail (Failure (Fmt.to_to_string DK.pp_error e))

let ( >|*= ) x f = x >>*= fun x -> Lwt.return (f x)

let ( >>**= ) x f = x >>= function Ok x -> f x | Error (`Msg x) -> failwith x

let or_fail msg = function None -> Alcotest.fail msg | Some x -> x

let config = Irmin_mem.config ()

(*
module Store = Irmin_unix.Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
let config = Irmin_unix.Irmin_git.config ~root:"unit-test-repo" ()
let () = Irmin_unix.install_dir_polling_listener 1.0
*)

let make_task msg =
  let date = 0L in
  Irmin.Info.v ~date ~author:"datakit-ci-test" msg

module Server = Fs9p.Make (Protocol_9p_unix.Flow_lwt_unix)
module Filesystem = Datakit.Vfs (Store)

let p = Path.of_string_exn

let () =
  CI_log_reporter.init None (Some Logs.Info);
  Logs.Src.list ()
  |> List.iter (fun src ->
         match Logs.Src.name src with
         | "datakit-ci.child" -> Logs.Src.set_level src (Some Logs.Debug)
         | "datakit-ci" -> Logs.Src.set_level src (Some Logs.Debug)
         | "dkt-github" -> Logs.Src.set_level src (Some Logs.Debug)
         | "Client9p" -> Logs.Src.set_level src (Some Logs.Info)
         | "datakit.client" -> Logs.Src.set_level src (Some Logs.Info)
         | "git.memory" | "git.unix" ->
             Logs.Src.set_level src (Some Logs.Warning)
         | name -> ignore name
         (* print_endline name *) )

let with_named_socket fn =
  let socket = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
  let path = Filename.get_temp_dir_name () ^ "/test-datakit-ci.socket" in
  (try Unix.unlink path with _ -> ());
  Lwt.finalize
    (fun () ->
      Lwt_unix.bind socket (Lwt_unix.ADDR_UNIX path) >>= fun () ->
      Lwt_unix.listen socket 2;
      fn (path, socket) )
    (fun () -> Lwt_unix.unlink path)

let with_datakit fn =
  Store.Repo.v config >>= fun repo ->
  Store.Repo.branches repo >>= fun branches ->
  Lwt_list.iter_s (fun branch -> Store.Branch.remove repo branch) branches
  >>= fun () ->
  with_named_socket @@ fun (for_client, for_server) ->
  let root = Filesystem.create ~info:make_task repo in
  Lwt.async (fun () ->
      Lwt_unix.accept for_server >>= fun (client, _addr) ->
      let flow = Protocol_9p_unix.Flow_lwt_unix.connect client in
      Server.accept ~root ~msg:"test connection" flow >|= function
      | Ok x -> x
      | Error (`Msg m) -> failwith m );
  fn for_client

let run fn () =
  Lwt_main.run
    ( with_datakit @@ fun for_client ->
      Private.Client9p.connect "unix" for_client () >>**= fun conn ->
      Lwt.finalize
        (fun () -> fn conn)
        (fun () ->
          Logs.info (fun f -> f "Disconnecting 9p");
          Private.Client9p.disconnect conn ) )

let run_private fn () =
  Lwt_main.run
    ( with_datakit @@ fun for_client ->
      CI_utils.Client9p.connect "unix" for_client () >>**= fun conn ->
      Lwt.finalize
        (fun () -> fn conn)
        (fun () ->
          Logs.info (fun f -> f "Disconnecting 9p");
          CI_utils.Client9p.disconnect conn ) )

let update branch values ~message =
  DK.Branch.with_transaction branch (fun t ->
      values
      |> Lwt_list.iter_s (fun (path, value) ->
             let dir, leaf =
               match String.cut ~rev:true ~sep:"/" path with
               | None -> (Path.empty, path)
               | Some (dir, leaf) -> (Path.of_string_exn dir, leaf)
             in
             DK.Transaction.make_dirs t dir >>*= fun () ->
             DK.Transaction.create_or_replace_file t (dir / leaf)
               (Cstruct.of_string value)
             >>*= Lwt.return )
      >>= fun () -> DK.Transaction.commit t ~message )
  >>*= Lwt.return

let single_line data =
  let s = Cstruct.to_string data in
  if String.is_suffix ~affix:"\n" s then
    String.with_range ~len:(String.length s - 1) s
  else Alcotest.fail (Printf.sprintf "Missing newline in %S" s)

(** Wait until [path] is a file with contents [value ^ "\n"].
    Until then, it must have contents [old ^ "\n"] (or not exist, if [old] is [None]).
    Also fails if it becomes a non-file object or if the switch is turned off. *)
let wait_for_file ?switch branch path ?old expected =
  Logs.info (fun f -> f "wait_for_file %s %s" path expected);
  DK.Branch.wait_for_path ?switch branch (Path.of_string_exn path) (function
    | Some (`File data) -> (
        let data = single_line data in
        if data = expected then Lwt.return (Ok (`Finish ()))
        else
          match old with
          | Some old when data = old -> Lwt.return (Ok `Again)
          | None ->
              Alcotest.fail
                (Printf.sprintf "Expected %S to change None -> %S, but got %S"
                   path expected data)
          | Some old ->
              Alcotest.fail
                (Printf.sprintf "Expected %S to change %S -> %S, but got %S"
                   path old expected data) )
    | Some _ -> Alcotest.fail "Bad type"
    | None -> (
      match old with
      | None -> Lwt.return (Ok `Again)
      | Some old ->
          Alcotest.fail
            (Printf.sprintf "Expected %S to change %S -> %S, but got None" path
               old expected) ) )
  >>*= function
  | `Abort -> Alcotest.fail ("Aborted while waiting for " ^ path)
  | `Finish () -> Lwt.return ()

let assert_file branch path value =
  DK.Branch.head branch >>*= function
  | None ->
      Alcotest.fail (Printf.sprintf "Branch does not exist! Checking %S" path)
  | Some head ->
      DK.Commit.tree head >>*= fun tree ->
      DK.Tree.read_file tree (p path) >>*= fun data ->
      let data = single_line data in
      Alcotest.(check string) (Printf.sprintf "%s=%s" path value) value data;
      Lwt.return ()

let update_ref hooks ~id ~head ~states ~message =
  update hooks ~message
    ( (Printf.sprintf "user/project/ref/%s/head" id, head)
    :: (Printf.sprintf "user/project/ref/%s/state" id, "open")
    :: List.map
         (fun (path, data) ->
           (Printf.sprintf "user/project/commit/%s/status/%s" head path, data)
           )
         states )

let update_pr hooks ~id ~head ~states ~message =
  update hooks ~message
    ( (Printf.sprintf "user/project/pr/%d/head" id, head)
    :: (Printf.sprintf "user/project/pr/%d/state" id, "open")
    :: (Printf.sprintf "user/project/pr/%d/owner" id, "joe")
    :: List.map
         (fun (path, data) ->
           (Printf.sprintf "user/project/commit/%s/status/%s" head path, data)
           )
         states )

let with_handler set_handler ~logs ?pending key fn =
  let finished, waker = Lwt.wait () in
  let pending =
    match pending with None -> Fmt.strf "Running %s" key | Some p -> p
  in
  let branch = "log-branch-for-" ^ key in
  let switch = Lwt_switch.create () in
  let log = Live_log.create ~switch ~pending ~branch ~title:"Title" logs in
  set_handler key
    { result = Error (`Pending (pending, finished)); output = Output.Live log };
  fn ~switch log >|= fun result ->
  Live_log.finish log;
  set_handler key { result; output = Output.Live log };
  Lwt.wakeup waker ()

let repo_root { Repo.user; repo } = Path.(empty / User.name user / repo)

(* [with_ci conn workflow fn] is [fn ~logs ~switch dk with_handler], where:
   - switch is turned off when [fn] ends and will stop the CI
   - dk is a DataKit connection which never fails
   - with_handler can be used to register handlers for jobs the CI receives
 *)
let with_ci ?(repo = Repo.v ~user:(User.v "user") ~repo:"project") conn
    workflow fn =
  let logs = Private.create_logs () in
  let handlers = ref String.Map.empty in
  let check_build key () =
    match String.Map.find key !handlers with
    | None -> Alcotest.fail (Fmt.strf "Test handler for job %s not found!" key)
    | Some x -> Lwt.return x
  in
  let web_ui = Uri.of_string "https://localhost/" in
  let dk = Private.connect conn in
  let ci =
    Private.test_engine ~web_ui
      (fun () -> Lwt.return dk)
      (Repo.Map.singleton repo (fun t ->
           String.Map.singleton "test" (workflow check_build t) ))
  in
  let switch = Lwt_switch.create () in
  let engine_thread =
    Lwt.catch
      (fun () -> Private.listen ci ~switch >>= fun `Abort -> Lwt.return ())
      (fun ex ->
        Logs.err (fun f -> f "Error from engine: %a" CI_utils.pp_exn ex);
        Lwt.fail ex )
  in
  Lwt.finalize
    (fun () ->
      DK.branch dk "github-metadata" >>*= fun hooks ->
      (* Work-around for https://github.com/mirage/irmin/issues/373 *)
      DK.Branch.wait_for_path hooks
        (repo_root repo / ".monitor")
        (function
          | None -> Lwt.return (Ok `Again)
          | Some _ -> Lwt.return (Ok (`Finish ())) )
      >>*= fun _ ->
      let set_handler key value =
        handlers := String.Map.add key value !handlers
      in
      fn ~logs ~switch dk (with_handler set_handler) )
    (fun () -> Lwt_switch.turn_off switch >>= fun () -> engine_thread)

let re_timestamp = Str.regexp "^\\[....-..-.. ..:.....\\] "

let strip_times log = Str.global_replace re_timestamp "" log

module Json = struct
  type t = Yojson.Basic.t

  let pp f v = Yojson.Basic.pretty_print f v

  let equal = ( = )
end

let json = (module Json : Alcotest.TESTABLE with type t = Json.t)

let () =
  let fd_stderr = Unix.descr_of_out_channel stderr in
  let real_stderr = Unix.dup fd_stderr in
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook :=
    fun ex ->
      Unix.dup2 real_stderr fd_stderr;
      Printf.eprintf "\nasync_exception_hook:\n%!";
      old_hook ex
