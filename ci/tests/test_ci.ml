open Datakit_github
open Datakit_ci
open! Astring
open Utils.Infix

let ( / ) = Datakit_client.Path.Infix.( / )

let src = Logs.Src.create "datakit-ci.tests" ~doc:"CI Tests"
module Log = (val Logs.src_log src : Logs.LOG)

module Workflows = struct
  module T = Term
  open T.Infix

  let circle_success_url = T.ci_success_target_url ["ci"; "circleci"]

  let test_circleci_artifact check_build target =
    circle_success_url target >>= fun url ->
    T.of_lwt_slow (check_build (Uri.to_string url))

  let simple_parallel check_build _target =
    let a = T.of_lwt_slow (check_build "a") in
    let b = T.of_lwt_slow (check_build "b") in
    T.return (^) $ a $ b

  let test_cross_project _check_build target =
    let other = Repo.v ~user:"bob" ~repo:"bproj" in
    let pr = T.head target >|= Commit.hash in
    let other_master = T.branch_head other "master" >|= Commit.hash in
    T.return (Fmt.strf "Compile %s with %s") $ pr $ other_master

  let ls = Git.command ~timeout:60.0 ~label:"ls" ~clone:false [[| "ls" |]]
  let ls_clone = Git.command ~timeout:60.0 ~label:"ls" ~clone:true [[| "ls" |]]
  let pull_and_run local_repo ~cmd check_build target =
    Git.fetch_head local_repo target >>= Git.run cmd >>= fun () ->
    T.of_lwt_slow (check_build "a")

  let pass check_build _target =
    T.of_lwt_slow (check_build "pass")

  let fetch_only ~local_repo check_build t =
    Git.fetch_head local_repo t >>= fun _ ->
    T.of_lwt_slow (check_build "a")
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
  with_handler ~logs "http://circle/build/50" ~pending:"CircleCI ready; enqueued pitfall job" (fun ~switch:_ _log ->
      (* A pending CircleCI build appears *)
      Test_utils.update_pr hooks ~message:"CircleCI pending" ~id:3907 ~head:"123" ~states:[
        "ci/circleci/state", "pending";
        "ci/circleci/target_url", "http://circle/build/50";
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
  >>= fun () ->
  DK.branch dk "status-user-project-pr-3907" >>*= DK.Branch.head >>*= function
  | None -> Alcotest.fail "Missing status branch!"
  | Some head ->
    DK.Commit.tree head >>*= fun tree ->
    DK.Tree.read_file tree (Datakit_client.Path.of_string_exn "job/test/output")
    >>*= fun data ->
    Alcotest.check Test_utils.json "Status JSON" (
      `Assoc [
        "result", `Assoc [
          "status", `String "success";
          "descr", `String "Pitfall tests passed!";
        ];
        "logs", `Assoc [
          "branch", `String "log-branch-for-http://circle/build/50";
        ]
      ]
    ) (Yojson.Basic.from_string (Cstruct.to_string data));
    Lwt.return ()

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

  let name _ = "test-builder"

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
    DK.Tree.read_file tr (Datakit_client.Path.of_string_exn "value/x") >>*= fun data ->
    Lwt.return (int_of_string (Cstruct.to_string data))

  let branch _t key = Printf.sprintf "cache-of-%s" key
end
module Test_cache = Cache.Make(Builder)

let rec read_logs dk = function
  | Output.Empty -> Lwt.return ""
  | Output.Live l -> Lwt.return (Live_log.contents l)
  | Output.Saved saved -> Private.read_log dk saved >>*= Lwt.return
  | Output.Pair (a, b) ->
    read_logs dk a >>= fun a ->
    read_logs dk b >>= fun b ->
    Lwt.return (Printf.sprintf "(%s,%s)" a b)

let rec rebuild_logs = function
  | Output.Empty -> Lwt.return ()
  | Output.Live _ -> Lwt.return ()
  | Output.Saved saved -> Private.rebuild saved
  | Output.Pair (a, b) -> Lwt.join [rebuild_logs a; rebuild_logs b]

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
  let test_lookup key =
    let c = get_cache () in
    let rec aux () =
      Test_cache.lookup c conn Builder.NoContext key >>= function
      | { result = Ok x; output } ->
        read_logs dk output >>= fun logs ->
        Lwt.return (x, Test_utils.strip_times logs, output)
      | { result = Error (`Failure msg); _ } -> Alcotest.fail msg
      | { result = Error (`Pending (msg, ready)); _ } ->
        Log.debug (fun f -> f "Waiting for result to be returned: %s" msg);
        ready >>= fun () ->
        aux ()
    in
    aux ()
  in
  (* First check of "1" *)
  test_lookup "1" >>= fun (value, log, _) ->
  Alcotest.(check int) "First 1 lookup" 1 value;
  Alcotest.(check string) "First 1 lookup log" "=== Get key\nStarting...\nTime=0\nSuccess\n" log;
  (* First check of "2" *)
  test_lookup "2" >>= fun (value, log, _) ->
  Alcotest.(check int) "First 2 lookup" 2 value;
  Alcotest.(check string) "First 2 lookup log" "=== Get key\nStarting...\nTime=1\nSuccess\n" log;
  (* Second check of "1" *)
  test_lookup "1" >>= fun (value, log, out) ->
  Alcotest.(check int) "Second 1 lookup" 1 value;
  Alcotest.(check string) "Second 1 lookup log" "=== Get key\nStarting...\nTime=0\nSuccess\n" log;
  (* Rebuild "1" *)
  rebuild_logs out >>= fun () ->
  test_lookup "1" >>= fun (value, log, out) ->
  Alcotest.(check int) "Rebuild 1 lookup" 1 value;
  Alcotest.(check string) "Rebuild 1 lookup log" "=== Get key\nStarting...\nTime=2\nSuccess\n" log;
  (* Parallel rebuilds *)
  if not regen then (
    let stream, push  = Lwt_stream.create () in
    b.Builder.step <- Some stream;
    rebuild_logs out >>= fun () ->
    let a = test_lookup "1" in
    rebuild_logs out >>= fun () ->
    let b = test_lookup "1" in
    push (Some ());
    push (Some ());
    push (Some ());
    a >>= fun (ra, la, _) ->
    b >>= fun (rb, lb, _) ->
    assert (ra = rb);
    assert (la = lb);
    Lwt.return ()
  ) else (
    (* Can't test in-memory locking if we create a new cache each time. *)
    Lwt.return ()
  )

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
    Test_cache.lookup c conn Builder.NoContext "1" >>= fun s ->
    let (reason, update) = expect_pending s.result in
    Alcotest.(check string) expected expected reason;
    assert (Lwt.state update = Lwt.Sleep);
    advance (Some ());
    update
  in
  check_pending "Get key" >>= fun () ->
  check_pending "Paused" >>= fun () ->
  check_pending "Get key" >>= fun () ->
  Test_cache.lookup c conn Builder.NoContext "1" >>= function
  | { result = Ok 1; _ } -> Lwt.return ()
  | _ -> Alcotest.fail "Expected success"

let run args = Process.run ~output:print_string ("", Array.of_list args)

let with_git_remote fn =
  let ( / ) = Filename.concat in
  let old_cwd = Sys.getcwd () in
  Lwt.finalize
    (fun () ->
       Utils.with_tmpdir (fun tmpdir ->
           (* Set up the "remote" Git repository *)
           Sys.chdir tmpdir;
           let remote_dir = tmpdir / "my-repo" in
           run ["git"; "init"; remote_dir] >>= fun () ->
           Sys.chdir remote_dir;
           (* Add a test file *)
           Lwt_io.with_file ~mode:Lwt_io.output "src" (fun ch ->
               Lwt_io.write ch "Test"
             )
           >>= fun () ->
           run ["git"; "add"; "src"] >>= fun () ->
           run ["git"; "commit"; "-m"; "Initial commit"] >>= fun () ->
           (* Clone a "local" copy *)
           let local_clone = tmpdir / "clone" in
           fn ~remote_dir ~local_clone
         )
    )
    (fun () ->
       Sys.chdir old_cwd;
       Lwt.return ()
    )

let test_git_dir conn ~clone =
  let logs = Private.create_logs () in
  let cmd =
    if clone then Workflows.ls_clone ~logs
    else Workflows.ls ~logs
  in
  with_git_remote @@ fun ~remote_dir ~local_clone ->
  Sys.chdir remote_dir;
  Lwt_process.pread_line ("", [| "git"; "rev-parse"; "HEAD" |]) >>= fun hash ->
  let local_repo = Git.v ~remote:remote_dir ~logs local_clone in
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
    DK.Commit.tree head >>*= fun tree ->
    DK.Tree.read_file tree (Datakit_client.Path.of_string_exn "log") >>*= fun log ->
    let log = Cstruct.to_string log in
    if not (String.is_infix ~affix:"Running \"ls\"...\nsrc" log) then
      Alcotest.fail "Missing 'src' in log output"
    else
      Lwt.return ()

let test_git_tag conn =
  let logs = Private.create_logs () in
  with_git_remote @@ fun ~remote_dir ~local_clone ->
  let local_repo = Git.v ~remote:remote_dir ~logs local_clone in
  (* Start the CI *)
  Test_utils.with_ci conn Workflows.(fetch_only ~local_repo) @@ fun ~logs ~switch dk with_handler ->
  DK.branch dk "github-metadata" >>*= fun hooks ->
  let wait_for ~commit path = Test_utils.wait_for_file ~switch hooks (Printf.sprintf "user/project/commit/%s/status/%s" commit path) in
  (* Create a tag *)
  Sys.chdir remote_dir;
  run ["git"; "commit"; "--allow-empty"; "-m"; "Release 0.1"] >>= fun () ->
  Lwt_process.pread_line ("", [| "git"; "rev-parse"; "HEAD" |]) >>= fun hash ->
  run ["git"; "tag"; "-a"; "-m"; "Release 0.1"; "v0.1"; hash] >>= fun () ->
  with_handler ~logs "a" (fun ~switch:_ _log ->
      Test_utils.update_ref hooks ~message:"Tag" ~id:"v0.1" ~head:hash ~states:[] >>= fun () ->
      wait_for ~commit:hash "ci/datakit/test/state" "pending" >>= fun () ->
      Lwt.return (Ok "Success")
    )
  >>= fun () ->
  wait_for ~old:"pending" ~commit:hash "ci/datakit/test/state" "success" >>= fun () ->
  Lwt.return ()

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

let with_test_auth fn =
  (* Configures a user "admin"/"fred". *)
  Utils.with_tmpdir (fun tmp ->
      let path = Filename.concat tmp "passwords.sexp" in
      let ch = open_out path in
      output_string ch "((admin((prf SHA1)(salt\"\\172~\\212>|'\\154\\202\\224\\128?\\158\\160\\245\\243j\")(hashed_password\"_\\231gB\\136\\221\\159!\\164%\\024\\\"0H\\\"\\230\\172\\142\\166\\138\")(count 5000)(dk_len 20))))";
      close_out ch;
      let github = CI_secrets.const None in
      CI_web_utils.Auth.create ~github ~github_scopes_needed:[] ~web_ui:(Uri.of_string "https://localhost") path >>= fn
    )

let test_auth () =
  Lwt_main.run begin
    with_test_auth @@ fun auth ->
    match CI_web_utils.Auth.lookup auth ~user:"foo" ~password:"bar" with
    | Some _ -> Alcotest.fail "Invalid user!"
    | None ->
      match CI_web_utils.Auth.lookup auth ~user:"admin" ~password:"fred!" with
      | Some _ -> Alcotest.fail "Invalid password!"
      | None ->
        match CI_web_utils.Auth.lookup auth ~user:"admin" ~password:"fred" with
        | None -> Alcotest.fail "Valid user!"
        | Some u ->
          Alcotest.(check string) "Got user" "admin" (CI_web_utils.User.name u);
          Lwt.return ()
  end

let status_code = Alcotest.of_pp (Fmt.of_to_string Cohttp.Code.string_of_status)

let test_roles conn =
  let tests = Repo.Map.of_list [
  ] in
  let dk = CI_utils.DK.connect conn in
  let web_ui = Uri.of_string "http://localhost/" in
  let ci = CI_engine.create ~web_ui (fun () -> Lwt.return dk) tests in
  let logs = CI_live_log.create_manager () in
  with_test_auth @@ fun auth ->
  let server ~public =
    let can_read = if public then CI_ACL.everyone else CI_ACL.username "admin" in
    let can_build = CI_ACL.username "admin" in
    let web_config = CI_web_templates.config ~name:"test-ci" ~can_read ~can_build () in
    let server = CI_web_utils.server ~web_config ~auth ~session_backend:`Memory ~public_address:web_ui in
    let routes = CI_web.routes ~server ~logs ~ci ~dashboards:(CI_target.map_of_list []) in
    fun ~expect path ->
      let request = Cohttp.Request.make (Uri.make ~path ()) in
      CI_web_utils.Wm.dispatch' routes ~request ~body:`Empty >|= function
      | None -> Alcotest.fail "No response!"
      | Some (code, _header, _body, _path) ->
        Alcotest.(check status_code) "Web response" expect code
  in
  let test_private_server = server ~public:false in
  let test_public_server = server ~public:true in
  test_private_server "/" ~expect:`See_other >>= fun () ->
  test_public_server "/" ~expect:`OK

let read_to stream expect =
  let rec aux ~sofar =
    Lwt_stream.get stream >>= function
    | None -> Alcotest.fail (Fmt.strf "End-of-stream waiting for %S@.Got: %s" expect sofar)
    | Some next ->
      let sofar = sofar ^ next in
      if String.is_infix ~affix:expect sofar then Lwt.return ()
      else aux ~sofar
  in
  aux ~sofar:""

let read_to_end stream =
  let rec aux () =
    Lwt_stream.get stream >>= function
    | None -> Lwt.return ()
    | Some _ -> aux ()
  in
  aux ()

let test_live_logs conn =
  let ( >>*= ) x f =
    x >>= function
    | Ok x -> f x
    | Error e -> failwith (Fmt.to_to_string CI_utils.DK.pp_error e)
  in
  let tests = Repo.Map.of_list [
  ] in
  let dk = CI_utils.DK.connect conn in
  let web_ui = Uri.of_string "http://localhost/" in
  let ci = CI_engine.create ~web_ui (fun () -> Lwt.return dk) tests in
  let logs = CI_live_log.create_manager () in
  with_test_auth @@ fun auth ->
  let can_read = CI_ACL.everyone in
  let can_build = CI_ACL.everyone in
  let web_config = CI_web_templates.config ~name:"test-ci" ~can_read ~can_build () in
  let server = CI_web_utils.server ~web_config ~auth ~session_backend:`Memory ~public_address:web_ui in
  let routes = CI_web.routes ~server ~logs ~ci ~dashboards:(CI_target.map_of_list []) in
  let get path =
    let request = Cohttp.Request.make (Uri.make ~path ()) in
    CI_web_utils.Wm.dispatch' routes ~request ~body:`Empty >|= function
    | None -> Alcotest.fail "No response!"
    | Some (code, _header, body, _path) ->
      Alcotest.(check status_code) "Web response" `OK code;
      body
  in
  let branch = "test/log" in
  let log = CI_live_log.create ~pending:"Building" ~branch ~title:"Test" logs in
  CI_live_log.printf log "TEST-\027[1;31mOUTPUT\027[m-1\n";
  let path = "/log/live/test%2flog" in
  get path >|= Cohttp_lwt_body.to_stream >>= fun log_page ->
  read_to log_page "TEST-<span class='fg-bright-red bold'>OUTPUT</span>-1" >>= fun () ->
  CI_live_log.printf log "TEST-OUTPUT-<&2>\n";
  read_to log_page "TEST-OUTPUT-&lt;&amp;2" >>= fun () ->
  CI_live_log.finish log;
  read_to_end log_page >>= fun () ->
  (* Once log is built, check we redirect to the saved log *)
  CI_utils.DK.branch dk branch >>*= fun dk_branch ->
  CI_utils.DK.Branch.with_transaction dk_branch (fun trans ->
      let data = Cstruct.of_string (CI_live_log.contents log) in
      CI_utils.DK.Transaction.create_file trans CI_cache.Path.log data >>*= fun () ->
      CI_utils.DK.Transaction.commit trans ~message:"Log saved"
    )
  >>*= fun () ->
  let request = Cohttp.Request.make (Uri.make ~path ()) in
  CI_web_utils.Wm.dispatch' routes ~request ~body:`Empty >>= function
  | None -> Alcotest.fail "No response!"
  | Some (code, header, _body, _path) ->
    Alcotest.(check status_code) "Web response" `Temporary_redirect code;
    let path = Cohttp.Header.get header "location" |> Test_utils.or_fail "Missing location" in
    get path >>= Cohttp_lwt_body.to_string >>= fun body ->
    if not (String.is_infix ~affix:"TEST-OUTPUT-&lt;&amp;" body) then
      Alcotest.fail ("Missing saved data in: " ^body);
    Lwt.return ()

module Jobs = struct
  type t = string CI_output.t String.Map.t
  let equal = String.Map.equal CI_output.equal
  let pp = String.Map.dump (CI_output.pp String.pp)
end

module DK_Commit = struct
  include CI_utils.DK.Commit
  let equal a b = (id a = id b)
end

let test_history conn =
  let module DK = CI_utils.DK in
  let ( >>*= ) x f =
    x >>= function
    | Ok x -> f x
    | Error e -> failwith (Fmt.to_to_string DK.pp_error e)
  in
  let logs = CI_live_log.create_manager () in
  let state_t = (module CI_history.State : Alcotest.TESTABLE with type t = CI_history.State.t) in
  let jobs_t = (module Jobs : Alcotest.TESTABLE with type t = Jobs.t) in
  let target_t = (module CI_target : Alcotest.TESTABLE with type t = CI_target.t) in
  let commit_t = (module DK_Commit : Alcotest.TESTABLE with type t = DK_Commit.t) in
  let h = CI_history.create () in
  let dk = DK.connect conn in
  let repo = Repo.v ~user:"me" ~repo:"repo" in
  let source_commit = "123" in
  let head = Commit.v repo source_commit in
  let master = `Ref (repo, ["heads"; "master"]) in
  CI_history.lookup h dk master >>= fun master_h ->
  (* Initially empty *)
  Alcotest.check (Alcotest.option state_t) "Empty head" None (CI_history.head master_h);
  CI_history.builds_of_commit dk head >|= CI_target.Map.bindings >>= fun builds ->
  Alcotest.(check (list (pair target_t commit_t))) "Empty index" [] builds;
  (* Record s1 *)
  let live = CI_live_log.create ~pending:"Pending" ~branch:"log-123" ~title:"Build" logs in
  let saved = { CI_output.title = "Build"; commit = "567"; branch = "build-of-123"; failed = false;
                rebuild = `Rebuildable (lazy Lwt.return_unit) } in
  let s1 = String.Map.of_list [
      "one", (Ok "Success", CI_output.Empty);
      "two", (Error (`Failure "Failed"), CI_output.Saved saved);
      "three", (Error (`Pending "Testing"), CI_output.Live live);
    ]
  in
  DK.commit dk "abc" >>*= fun metadata_commit ->
  CI_history.record master_h dk ~source_commit metadata_commit s1 >>= fun () ->
  (* Check s1 *)
  CI_history.head master_h |> Test_utils.or_fail "No head state!" |> CI_history.State.jobs |> Alcotest.check jobs_t "Initial commit" s1;
  CI_history.record master_h dk ~source_commit metadata_commit s1 >>= fun () ->
  CI_history.head master_h |> Test_utils.or_fail "No head state!" |> CI_history.State.parents |> Alcotest.(check (list string)) "No changes" [];
  (* Record s2 *)
  let s2 = String.Map.of_list [
      "one", (Ok "Success", CI_output.Empty);
      "two", (Error (`Failure "Failed2"), CI_output.Saved saved);
    ]
  in
  CI_history.record master_h dk ~source_commit metadata_commit s2 >>= fun () ->
  (* Check s2 *)
  let head2 = CI_history.head master_h |> Test_utils.or_fail "No head state!" in
  let head1 =
    match CI_history.State.parents head2 with
    | [p] -> p
    | x -> Alcotest.fail (Fmt.strf "Expected one parent, not %a" (Fmt.Dump.list Fmt.string) x)
  in
  Alcotest.check jobs_t "Updated commit" s2 (CI_history.State.jobs head2);
  (* Check saved s1 *)
  DK.commit dk head1 >>*= fun tree ->
  CI_history.load tree >>= fun loaded1 ->
  Alcotest.(check (option string)) "Source commit" (Some "123") (CI_history.State.source_commit loaded1);
  let s1_expected = s1 |> String.Map.add "three" (Error (`Pending "Testing"), CI_output.Empty) in
  Alcotest.check jobs_t "Loaded" s1_expected (CI_history.State.jobs loaded1);
  (* Check index *)
  CI_history.builds_of_commit dk head >|= CI_target.Map.bindings >>= fun builds ->
  DK.branch dk (CI_target.status_branch master) >>*= DK.Branch.head >>*= fun expected_head ->
  let expected_head = expected_head |> Test_utils.or_fail "Missing status branch" in
  Alcotest.(check (list (pair target_t commit_t))) "Non-empty index" [master, expected_head] builds;
  Lwt.return ()

let test_process () =
  Lwt_main.run begin
    let out = Buffer.create 40 in
    let err = Buffer.create 40 in
    let cmd = "/bin/sh", [| "/bin/sh"; "-c"; "echo output; echo error >&2" |] in
    Datakit_ci.Process.run cmd
      ~output:(Buffer.add_string out)
      ~stderr:(Buffer.add_string err)
    >>= fun () ->
    Alcotest.(check string) "stdout" "output\n" (Buffer.contents out);
    Alcotest.(check string) "stderr" "error\n" (Buffer.contents err);
    Datakit_ci.Process.run cmd
      ~output:(Buffer.add_string out)
    >>= fun () ->
    Alcotest.(check string) "stdout" "output\noutput\nerror\n" (Buffer.contents out);
    Lwt.return ()
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
  "Git tag",        `Quick, Test_utils.run test_git_tag;
  "Cross-project",  `Quick, Test_utils.run test_cross_project;
  "Auth",           `Quick, test_auth;
  "Roles",          `Quick, Test_utils.run_private test_roles;
  "Live logs",      `Quick, Test_utils.run_private test_live_logs;
  "History",        `Quick, Test_utils.run_private test_history;
  "Process",        `Quick, test_process;
]

let () =
  Alcotest.run "datakit-ci" [
    "ci", test_set;
  ]
