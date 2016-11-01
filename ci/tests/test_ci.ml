open DataKitCI
open! Astring
open Utils

let ( / ) = Datakit_path.Infix.( / )

let src = Logs.Src.create "datakit-ci.tests" ~doc:"CI Tests"
module Log = (val Logs.src_log src : Logs.LOG)

module Workflows = struct
  module T = DataKitCI.Term
  open T.Infix

  let circle_success_url = T.ci_success_target_url "ci/circleci" 

  let test_circleci_artifact check_build =
    circle_success_url >>= fun url ->
    T.of_lwt_slow (check_build (Uri.to_string url))

  let simple_parallel check_build =
    let a = T.of_lwt_slow (check_build "a") in
    let b = T.of_lwt_slow (check_build "b") in
    T.return (^) $ a $ b

  let test_cross_project _check_build =
    let other = ProjectID.v ~user:"bob" ~project:"bproj" in
    let pr = T.head >|= Github_hooks.Commit.hash in
    let other_master = T.branch_head other "master" >|= Github_hooks.Commit.hash in
    T.return (Fmt.strf "Compile %s with %s") $ pr $ other_master

  let ls = DKCI_git.command ~timeout:60.0 ~label:"ls" ~clone:false [[| "ls" |]]
  let ls_clone = DKCI_git.command ~timeout:60.0 ~label:"ls" ~clone:true [[| "ls" |]]
  let pull_and_run local_repo ~cmd check_build =
    DKCI_git.fetch_head local_repo >>= DKCI_git.run cmd >>= fun () ->
    T.of_lwt_slow (check_build "a")

  let pass check_build =
    T.of_lwt_slow (check_build "pass")
end

open Lwt.Infix

let test_simple conn =
  Test_utils.with_ci conn Workflows.test_circleci_artifact @@ fun ~logs ~switch dk with_handler ->
  DK.branch dk "github-metadata" >>*= fun hooks ->
  let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
  (* A new PR appears *)
  Test_utils.update_pr hooks ~message:"Init" ~id:3907 ~head:"123" ~states:[
  ] >>= fun () ->
  (* PinataCI adds a pending status *)
  wait_for ~commit:"123" "ci/datakit/test/state" "pending" >>= fun () ->
  Test_utils.assert_file hooks "user/project/commit/123/status/ci/datakit/test/description" "Waiting for ci/circleci status to appear" >>= fun () ->
  with_handler ~logs "http://cirlce/build/50" ~pending:"CircleCI ready; enqueued pitfall job" (fun ~switch:_ _log ->
      (* A pending CircleCI build appears *)
      Test_utils.update_pr hooks ~message:"CircleCI pending" ~id:3907 ~head:"123" ~states:[
        "ci/circleci/state", "pending";
        "ci/circleci/target_url", "http://cirlce/build/50";
      ] >>= fun () ->
      wait_for ~commit:"123" "ci/datakit/test/description" ~old:"Waiting for ci/circleci status to appear" "Waiting for ci/circleci to complete" >>= fun () ->
      (* CircleCI finishes the build *)
      Test_utils.update_pr hooks ~message:"CircleCI ready" ~id:3907 ~head:"123" ~states:["ci/circleci/state", "success"] >>= fun () ->
      (* PinataCI updates status to enqueued *)
      wait_for ~commit:"123" "ci/datakit/test/description" ~old:"Waiting for ci/circleci to complete" "CircleCI ready; enqueued pitfall job" >>= fun () ->
      (* The PinataCI job completes *)
      Lwt.return (Ok "Pitfall tests passed!")
    )
  >>= fun () ->
  (* PinataCI updates the status to success *)
  wait_for ~commit:"123" "ci/datakit/test/state" ~old:"pending" "success"

let test_branch conn =
  Test_utils.with_ci conn Workflows.pass @@ fun ~logs ~switch dk with_handler ->
  DK.branch dk "github-metadata" >>*= fun hooks ->
  let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
  (* A new branch appears *)
  with_handler ~logs "pass" ~pending:"Pending" (fun ~switch:_ _log ->
      Test_utils.update ~message:"Init foo" hooks [
        "user/project/ref/heads/foo/head", "123";
      ] >>= fun () ->
      (* PinataCI adds a pending status *)
      wait_for ~commit:"123" "ci/datakit/test/state" "pending" >>= fun () ->
      Lwt.return (Ok "Pass")
    )
  >>= fun () ->
  (* PinataCI updates the status to success *)
  wait_for ~commit:"123" "ci/datakit/test/state" ~old:"pending" "success"
  >>= fun () ->
  (* The branch head moves *)
  with_handler ~logs "pass" ~pending:"Pending" (fun ~switch:_ _log ->
      Test_utils.update ~message:"Init foo" hooks [
        "user/project/ref/heads/foo/head", "456";
      ] >>= fun () ->
      (* PinataCI adds a pending status *)
      wait_for ~commit:"456" "ci/datakit/test/state" "pending" >>= fun () ->
      Lwt.return (Ok "Pass")
    )
  >>= fun () ->
  (* PinataCI updates the status to success *)
  wait_for ~commit:"456" "ci/datakit/test/state" ~old:"pending" "success"

let test_cancel conn =
  Test_utils.with_ci conn Workflows.pass @@ fun ~logs ~switch dk with_handler ->
  DK.branch dk "github-metadata" >>*= fun hooks ->
  let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
  (* A new branch appears *)
  with_handler ~logs "pass" ~pending:"Pending" (fun ~switch _log ->
      Test_utils.update ~message:"Init foo" hooks [
        "user/project/ref/heads/foo/head", "123";
      ] >>= fun () ->
      (* PinataCI adds a pending status *)
      wait_for ~commit:"123" "ci/datakit/test/state" "pending" >>= fun () ->
      (* User cancels build *)
      match Private.lookup_log ~branch:"log-branch-for-pass" logs with
      | None -> Alcotest.fail "Log not registered!"
      | Some found_log ->
        Private.cancel found_log >>= function
        | Error msg -> Alcotest.fail msg
        | Ok () -> Lwt.return (Error (`Failure (Fmt.strf "Switch:%b" (Lwt_switch.is_on switch))))
    )
  >>= fun () ->
  (* PinataCI updates the status to cancelled *)
  wait_for ~commit:"123" "ci/datakit/test/state" ~old:"pending" "failure"

let test_circle_rebuild conn =
  Test_utils.with_ci conn Workflows.test_circleci_artifact @@ fun ~logs ~switch dk with_handler ->
  DK.branch dk "github-metadata" >>*= fun hooks ->
  let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
  (* First CircleCI build *)
  with_handler ~logs "http://cirlce/build/50" ~pending:"CircleCI ready; enqueued pitfall job" (fun ~switch:_ _log ->
      (* A new PR appears with a successful CircleCI build *)
      Test_utils.update_pr hooks ~message:"Init" ~id:3907 ~head:"123" ~states:[
        "ci/circleci/state",      "success";
        "ci/circleci/target_url", "http://cirlce/build/50";
      ] >>= fun () ->
      (* PinataCI runs its tests *)
      wait_for ~commit:"123" "ci/datakit/test/state" "pending" >>= fun () ->
      wait_for ~commit:"123" "ci/datakit/test/description" ~old:"Waiting for ci/circleci to complete" "CircleCI ready; enqueued pitfall job" >>= fun () ->
      Lwt.return (Ok "Pitfall tests passed!")
    )
  >>= fun () ->
  wait_for ~commit:"123" "ci/datakit/test/state" ~old:"pending" "success" >>= fun () ->
  (* CircleCI does a rebuild for the same commit (but new build URL) *)
  with_handler ~logs "http://cirlce/build/51" ~pending:"CircleCI ready; enqueued pitfall job" (fun ~switch:_ _log ->
      Test_utils.update_pr hooks ~message:"Rebuilt CircleCI" ~id:3907 ~head:"123" ~states:[
        "ci/circleci/state", "success";
        "ci/circleci/target_url", "http://cirlce/build/51";
      ] >>= fun () ->
      (* PinataCI triggers new tests for the new CircleCI build *)
      wait_for ~commit:"123" "ci/datakit/test/state" ~old:"success" "pending" >>= fun () ->
      Lwt.return (Error (`Failure "Pitfall tests failed!"))
    )
  >>= fun () ->
  wait_for ~commit:"123"  "ci/datakit/test/state" ~old:"pending" "failure"

let test_logs conn =
  Test_utils.with_ci conn Workflows.simple_parallel @@ fun ~logs ~switch dk with_handler ->
  DK.branch dk "github-metadata" >>*= fun hooks ->
  let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
  with_handler ~logs "a" (fun ~switch:_ a_log ->
      Live_log.log a_log "output from a";
      with_handler ~logs "b" (fun ~switch:_ b_log ->
          Live_log.log b_log "output from b";
          Lwt.return (Ok "b completed")
        )
      >>= fun () ->
      Live_log.log a_log "more output from a";
      Test_utils.update_pr hooks ~message:"Init" ~id:1 ~head:"123" ~states:[] >>= fun () ->
      wait_for ~commit:"123" "ci/datakit/test/state" "pending" >>= fun () ->
      Lwt.return (Error (`Failure "a failed"))
    )
  >>= fun () ->
  wait_for ~commit:"123" "ci/datakit/test/state" ~old:"pending" "failure" >>= fun () ->
  Test_utils.assert_file hooks "user/project/commit/123/status/ci/datakit/test/description" "a failed" >>= fun () ->
  Lwt.return ()

module Builder = struct
  type t = {
    mutable time : int;
    mutable step : unit Lwt_stream.t option;
  }

  module Key = String

  type context = NoContext

  type value = int

  let title _ _key = "Get key"

  let maybe_step t ~log =
    match t.step with
    | None -> Lwt.return ()
    | Some step ->
      Lwt_stream.next step >>= fun () ->
      Live_log.with_pending_reason log "Paused" (fun () ->
          Lwt_stream.next step
        )
      >>= fun () ->
      Lwt_stream.next step

  let generate t ~switch ~log trans NoContext key =
    Utils.cancel_when_off switch @@ fun () ->
    maybe_step t ~log >>= fun () ->
    Live_log.log log "Time=%d" t.time;
    t.time <- t.time + 1;
    let data = Cstruct.of_string key in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "x") data >>*= fun () ->
    Lwt.return @@ Ok (int_of_string key)

  let load _t tr _key =
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/x") >>*= fun data ->
    Lwt.return (int_of_string (Cstruct.to_string data))

  let branch _t key = Printf.sprintf "cache-of-%s" key
end
module Test_cache = Cache.Make(Builder)

let rec read_logs dk = function
  | Step_log.Empty -> Lwt.return ""
  | Step_log.Live l -> Lwt.return (Live_log.contents l)
  | Step_log.Saved saved -> Private.read_log dk saved >>*= Lwt.return
  | Step_log.Pair (a, b) ->
    read_logs dk a >>= fun a ->
    read_logs dk b >>= fun b ->
    Lwt.return (Printf.sprintf "(%s,%s)" a b)

(* Check the cache works. If [regen] is [true], make a cache for each
   operation (testing disk pesistence). Otherwise, use the same one, testing
   memory persistence. *)
let test_cache ~regen =
  let logs = Private.create_logs () in
  let b = { Builder.time = 0; step = None } in
  let get_cache =
    match regen with
    | true -> fun () -> Test_cache.create ~logs b
    | false ->
      let cache = Test_cache.create ~logs b in
      fun () -> cache
  in
  Test_utils.run @@ fun conn ->
  let dk = Private.connect conn in
  let conn () = Lwt.return dk in
  let test_lookup ?(rebuild=false) key =
    let c = get_cache () in
    let rec aux ~rebuild =
      Test_cache.lookup c conn ~rebuild Builder.NoContext key >>= function
      | Ok x, logs ->
        read_logs dk logs >>= fun logs ->
        Lwt.return (x, Test_utils.strip_times logs)
      | Error (`Failure msg), _logs -> Alcotest.fail msg
      | Error (`Pending (msg, ready)), _logs ->
        Log.debug (fun f -> f "Waiting for result to be returned: %s" msg);
        ready >>= fun () ->
        aux ~rebuild:false
    in
    aux ~rebuild
  in
  (* First check of "1" *)
  test_lookup "1" >>= fun (value, log) ->
  Alcotest.(check int) "First 1 lookup" 1 value;
  Alcotest.(check string) "First 1 lookup log" "=== Get key\nStarting...\nTime=0\nSuccess\n" log;
  (* First check of "2" *)
  test_lookup "2" >>= fun (value, log) ->
  Alcotest.(check int) "First 2 lookup" 2 value;
  Alcotest.(check string) "First 2 lookup log" "=== Get key\nStarting...\nTime=1\nSuccess\n" log;
  (* Second check of "1" *)
  test_lookup "1" >>= fun (value, log) ->
  Alcotest.(check int) "Second 1 lookup" 1 value;
  Alcotest.(check string) "Second 1 lookup log" "=== Get key\nStarting...\nTime=0\nSuccess\n" log;
  (* Rebuild "1" *)
  test_lookup ~rebuild:true "1" >>= fun (value, log) ->
  Alcotest.(check int) "Rebuild 1 lookup" 1 value;
  Alcotest.(check string) "Rebuild 1 lookup log" "=== Get key\nStarting...\nTime=2\nSuccess\n" log;
  Lwt.return ()

let expect_pending = function
  | Error (`Pending x) -> x
  | Error (`Failure msg) -> Alcotest.fail ("Expected Pending, got Failure " ^ msg)
  | Ok _ -> Alcotest.fail "Expected Pending, got Success"

let test_pending_updates conn =
  let logs = Private.create_logs () in
  let dk = Private.connect conn in
  let conn () = Lwt.return dk in
  let step, advance = Lwt_stream.create () in
  let b = { Builder.time = 0; step = Some step } in
  let c = Test_cache.create ~logs b in
  let check_pending expected =
    Test_cache.lookup c conn ~rebuild:false Builder.NoContext "1" >>= fun (status, _log) ->
    let (reason, update) = expect_pending status in
    Alcotest.(check string) expected expected reason;
    assert (Lwt.state update = Lwt.Sleep);
    advance (Some ());
    update
  in
  check_pending "Get key" >>= fun () ->
  check_pending "Paused" >>= fun () ->
  check_pending "Get key" >>= fun () ->
  Test_cache.lookup c conn ~rebuild:false Builder.NoContext "1" >>= function
  | Ok 1, _ -> Lwt.return ()
  | _ -> Alcotest.fail "Expected success"

let test_git_dir conn ~clone =
  let logs = Private.create_logs () in
  let run args = Process.run ~output:print_string ("", Array.of_list args) in
  let ( / ) = Filename.concat in
  let old_cwd = Sys.getcwd () in
  let cmd =
    if clone then Workflows.ls_clone ~logs
    else Workflows.ls ~logs
  in
  Lwt.finalize
    (fun () ->
       Utils.with_tmpdir (fun tmpdir ->
           (* Set up the "remote" Git repository *)
           Sys.chdir tmpdir;
           run ["git"; "init"; "my-repo"] >>= fun () ->
           Sys.chdir "my-repo";
           Lwt_io.with_file ~mode:Lwt_io.output "src" (fun ch ->
               Lwt_io.write ch "Test"
             )
           >>= fun () ->
           (* Add a test file *)
           run ["git"; "add"; "src"] >>= fun () ->
           run ["git"; "commit"; "-m"; "Initial commit"] >>= fun () ->
           Lwt_process.pread_line ("", [| "git"; "rev-parse"; "HEAD" |]) >>= fun hash ->
           (* Clone a "local" copy *)
           run ["git"; "clone"; tmpdir / "my-repo"; tmpdir / "clone"] >>= fun () ->
           let local_repo = DKCI_git.connect ~logs ~dir:(tmpdir / "clone") in
           (* Start the CI *)
           Test_utils.with_ci conn (Workflows.pull_and_run local_repo ~cmd) @@ fun ~logs ~switch dk with_handler ->
           DK.branch dk "github-metadata" >>*= fun hooks ->
           let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
           (* Create a pull request *)
           with_handler ~logs "a" (fun ~switch:_ _log ->
               Test_utils.update_pr hooks ~message:"Init" ~id:1 ~head:hash ~states:[] >>= fun () ->
               wait_for ~commit:hash "ci/datakit/test/state" "pending" >>= fun () ->
               Lwt.return (Ok "Success!")
             )
           >>= fun () ->
           wait_for ~commit:hash "ci/datakit/test/state" ~old:"pending" "success" >>= fun () ->
           DK.branch dk (Printf.sprintf "shell-of-ls-on-%s" hash) >>*= fun results ->
           DK.Branch.head results >>*= function
           | None -> Alcotest.fail "Missing results branch!"
           | Some head ->
             let tree = DK.Commit.tree head in
             DK.Tree.read_file tree (Datakit_path.of_string_exn "log") >>*= fun log ->
             let log = Cstruct.to_string log in
             if not (String.is_infix ~affix:"Running \"ls\"...\nsrc" log) then
               Alcotest.fail "Missing 'src' in log output"
             else
               Lwt.return ()
         )
    )
    (fun () ->
       Sys.chdir old_cwd;
       Lwt.return ()
    )

let test_cross_project conn =
  Test_utils.with_ci conn Workflows.test_cross_project @@ fun ~logs ~switch dk _with_handler ->
  ignore logs;
  DK.branch dk "github-metadata" >>*= fun hooks ->
  let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
  (* Set bob/bproj/master *)
  Test_utils.update ~message:"Init Bob" hooks [
    "bob/bproj/ref/heads/master/head", "987";
  ] >>= fun () ->
  (* A new PR appears *)
  Test_utils.update_pr hooks ~message:"Init" ~id:3907 ~head:"123" ~states:[
  ] >>= fun () ->
  (* PinataCI adds a pending status *)
  wait_for ~commit:"123" "ci/datakit/test/state" "success" >>= fun () ->
  Test_utils.assert_file hooks "user/project/commit/123/status/ci/datakit/test/description" "Compile 123 with 987" >>= fun () ->
  (* Master changes *)
  Test_utils.update ~message:"Init Bob" hooks [
    "bob/bproj/ref/heads/master/head", "654";
  ] >>= fun () ->
  Test_utils.wait_for_file ~switch hooks "user/project/commit/123/status/ci/datakit/test/description" "Compile 123 with 654"
    ~old:"Compile 123 with 987"
  >>= fun () ->
  (* PR changes *)
  Test_utils.update_pr hooks ~message:"Init" ~id:3907 ~head:"456" ~states:[] >>= fun () ->
  Test_utils.wait_for_file ~switch hooks "user/project/commit/456/status/ci/datakit/test/description" "Compile 456 with 654"

let test_auth () =
  Lwt_main.run begin
    Utils.with_tmpdir (fun tmp ->
        let path = Filename.concat tmp "passwords.sexp" in
        let ch = open_out path in
        output_string ch "((admin((prf SHA1)(salt\"\\172~\\212>|'\\154\\202\\224\\128?\\158\\160\\245\\243j\")(hashed_password\"_\\231gB\\136\\221\\159!\\164%\\024\\\"0H\\\"\\230\\172\\142\\166\\138\")(count 5000)(dk_len 20))))";
        close_out ch;
        CI_web_utils.Auth.create path >|= fun auth ->
        match CI_web_utils.Auth.lookup auth ~user:"foo" ~password:"bar" with
        | Some _ -> Alcotest.fail "Invalid user!"
        | None ->
          match CI_web_utils.Auth.lookup auth ~user:"admin" ~password:"fred!" with
          | Some _ -> Alcotest.fail "Invalid password!"
          | None ->
            match CI_web_utils.Auth.lookup auth ~user:"admin" ~password:"fred" with
            | None -> Alcotest.fail "Valid user!"
            | Some u ->
              Alcotest.(check string) "Got user" "admin" (CI_web_utils.User.name u)
      )
  end

let test_set = [
  "Simple",         `Quick, Test_utils.run test_simple;
  "Branch",         `Quick, Test_utils.run test_branch;
  "Circle rebuild", `Quick, Test_utils.run test_circle_rebuild;
  "Logs",           `Quick, Test_utils.run test_logs;
  "Cancel",         `Quick, Test_utils.run test_cancel;
  "Cache (disk)",   `Quick, test_cache ~regen:true;
  "Cache (memory)", `Quick, test_cache ~regen:false;
  "Test pending",   `Quick, Test_utils.run test_pending_updates;
  "Git",            `Quick, Test_utils.run (test_git_dir ~clone:false);
  "Git (clone)",    `Quick, Test_utils.run (test_git_dir ~clone:true);
  "Cross-project",  `Quick, Test_utils.run test_cross_project;
  "Auth",           `Quick, test_auth;
]

let () =
  Alcotest.run "datakit-ci" [
    "ci", test_set;
  ]
