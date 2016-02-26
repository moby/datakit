open Lwt.Infix
open Test_utils
open Result

let return = Lwt.return

let transaction () =
  run (fun repo conn ->
    check_dir conn [] "Root entries" ["branch"; "snapshots"; "trees"]
    >>= fun () ->

    Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
    check_dir conn ["branch"] "Check master exists" ["master"] >>= fun () ->

    check_dir conn ["branch"; "master"; ".."; ".."]
      "Check .. works" ["branch"; "snapshots"; "trees"] >>= fun () ->

    Client.mkdir conn ["branch"; "master"; "transactions"] "init"
      rwxr_xr_x >>*= fun () ->
    Client.mkdir conn ["branch"; "master"; "transactions"; "init"; "rw"]
      "src" rwxr_xr_x >>*= fun () ->
    create_file conn ["branch"; "master"; "transactions"; "init"; "rw"; "src"]
      "Makefile" "all: build test" >>= fun () ->
    write_file conn ["branch"; "master"; "transactions"; "init"; "msg"]
      "My commit" >>*= fun () ->
    check_file conn ["branch"; "master"; "transactions"; "init"; "parents"] "Parents" "" >>= fun () ->
    echo conn "commit" ["branch"; "master"; "transactions"; "init"; "ctl"] >>= fun () ->

    Client.stat conn ["branch"; "master"; "ro"; "src"; "Makefile"]
    >>*= fun info ->
    let length = Int64.to_int info.Protocol_9p.Types.Stat.length in
    Alcotest.(check int) "File size" 15 length;

    Store.master Irmin.Task.none repo >>= fun master ->
    Store.head_exn (master ()) >>=
    Store.Repo.task_of_commit_id repo >>= fun task ->
    let msg = Irmin.Task.messages task in
    Alcotest.(check (list string)) "Message" ["My commit"] msg;

    Lwt.return ()
  )

let test_parents () =
  run (fun _repo conn ->
    let check_parents ~branch expected =
      read_file conn ["branch"; branch; "head"] >>= function
      | "\n" -> Alcotest.(check string) "Parents" expected "no-commit"; Lwt.return ()
      | hash ->
          read_file conn ["snapshots"; String.trim hash; "parents"] >|= fun parents ->
          Alcotest.(check string) "Parents" expected parents in
    let check_fails name parents =
      Lwt.catch
        (fun () ->
          with_transaction conn ~branch:"dev" name (fun dir ->
            write_file conn (dir @ ["parents"]) parents >>*= Lwt.return
          ) >>= fun () ->
          Alcotest.fail "Should have been rejected"
        )
        (fun _ex -> Lwt.return ()) in

    Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
    check_parents ~branch:"master" "no-commit" >>= fun () ->

    with_transaction conn ~branch:"master" "test1" (fun dir ->
      create_file conn (dir @ ["rw"]) "file" "data"
    ) >>= fun () ->
    check_parents ~branch:"master" "" >>= fun () ->
    read_file conn ["branch"; "master"; "head"] >>= fun master_head ->

    with_transaction conn ~branch:"master" "test1" (fun dir ->
      create_file conn (dir @ ["rw"]) "file" "data2"
    ) >>= fun () ->
    check_parents ~branch:"master" master_head >>= fun () ->
    read_file conn ["branch"; "master"; "head"] >>= fun master_head ->

    Client.mkdir conn ["branch"] "dev" rwxr_xr_x >>*= fun () ->
    with_transaction conn ~branch:"dev" "test2" (fun dir ->
      create_file conn (dir @ ["rw"]) "file" "dev" >>= fun () ->
      write_file conn (dir @ ["parents"]) master_head >>*= Lwt.return
    ) >>= fun () ->
    check_parents ~branch:"dev" master_head >>= fun () ->

    check_fails "Invalid hash" "hello" >>= fun () ->
    check_fails "Missing hash" "a3827c5d1a2ba8c6a40eded5598dba8d3835fb35" >>= fun () ->

    read_file conn ["branch"; "dev"; "head"] >>= fun dev_head ->
    with_transaction conn ~branch:"master" "test3" (fun t1 ->
      with_transaction conn ~branch:"master" "test4" (fun t2 ->
        create_file conn (t2 @ ["rw"]) "from_inner" "inner"
      ) >>= fun () ->
      read_file conn ["branch"; "master"; "head"] >>= fun after_inner ->
      create_file conn (t1 @ ["rw"]) "from_outer" "outer" >>= fun () ->
      read_file conn (t1 @ ["parents"]) >>= fun real_parent ->
      write_file conn (t1 @ ["parents"]) (real_parent ^ dev_head) >>*= fun () ->
      Lwt.return (real_parent, after_inner)
    ) >>= fun (orig_parent, after_inner) ->
    (* Now we should have two merges: master is a merge of t1 and t2,
       and t1 is a merge of master and dev. *)
    read_file conn ["branch"; "master"; "head"] >|= String.trim >>= fun new_head ->
    history conn new_head >>= fun history ->
    let a, b, c =
      match history with
      | Commit (_, [
          Commit (_, [
            Commit (b, _);
            Commit (c, _)
          ]);
          Commit (a, _);
        ]) -> a, b, c
      | x -> Alcotest.fail (Format.asprintf "Bad history:@\n%a" pp_history x) in
    Alcotest.(check string) "First parent" after_inner (a ^ "\n");
    Alcotest.(check string) "Dev parent" dev_head (b ^ "\n");
    Alcotest.(check string) "Orig parent" orig_parent (c ^ "\n");
    Lwt.return ()
  )

let test_merge () =
  run (fun _repo conn ->
    (* Put "from-master" on master branch *)
    Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
    with_transaction conn ~branch:"master" "init" (fun t ->
      create_file conn (t @ ["rw"]) "file" "from-master"
    ) >>= fun () ->
    (* Fork and put "from-master+pr" on pr branch *)
    Client.mkdir conn ["branch"] "pr" rwxr_xr_x >>*= fun () ->
    head conn "master" >>= fun merge_a ->
    write_file conn ["branch"; "pr"; "fast-forward"] merge_a >>*= fun () ->
    with_transaction conn ~branch:"pr" "mod" (fun t ->
      read_file conn (t @ ["rw"; "file"]) >>= fun old ->
      write_file conn (t @ ["rw"; "file"]) (old ^ "+pr") >>*= return
    ) >>= fun () ->
    head conn "pr" >>= fun merge_b ->
    (* Merge pr into master *)
    with_transaction conn ~branch:"master" "merge" (fun t ->
      create_file conn (t @ ["rw"]) "mine" "pre-merge" >>= fun () ->
      write_file conn (t @ ["merge"]) merge_b >>*= fun () ->
      check_file conn (t @ ["ours"; "mine"]) "Ours" "pre-merge" >>= fun () ->
      check_file conn (t @ ["theirs"; "file"]) "Theirs" "from-master+pr" >>= fun () ->
      check_file conn (t @ ["base"; "file"]) "Base" "from-master"
    ) >>= fun () ->
    head conn "master" >>= fun merge_commit ->
    read_file conn ["snapshots"; merge_commit; "parents"] >>= fun parents ->
    Alcotest.(check string) "Merge parents"
      (Printf.sprintf "%s\n%s\n" merge_b merge_a) parents;
    read_file conn ["branch"; "master"; "ro"; "file"] >>= fun merged ->
    Alcotest.(check string) "Merge result" "from-master+pr" merged;
    Lwt.return ()
  )

let test_conflicts () =
  run (fun _repo conn ->
    try_merge conn
      ~base:[
        "a", "a-from-base";
        "b", "b-from-base";
        "c", "c-from-base";
        "d", "d-from-base";
        "e", "e-from-base";
        "f", "f-from-base";
        "dir/a", "a-from-base";
      ]
      ~ours:[
        "a", "a-ours";          (* edit a *)
        "b", "b-from-base";
        "c", "c-ours";          (* edit c *)
        (* delete d *)
        "e", "e-from-base";
        (* delete f *)
        "g", "g-same";          (* create g *)
        "h", "h-ours";          (* create h *)
        "dir", "ours-now-file"; (* convert dir to file *)
      ]
      ~theirs:[
        "a", "a-theirs";     (* edit a *)
        "b", "b-theirs";     (* edit b *)
        "c", "c-from-base";
        "d", "d-from-base";
        (* delete e *)
        "f", "f-theirs";
        "g", "g-same";          (* create g *)
        "h", "h-theirs";        (* create h *)
        "dir/a", "a-theirs";    (* edit dir/a *)
      ]
      (fun t ->
        write_file conn (t @ ["ctl"]) "commit" >>= function
        | Ok () -> Alcotest.fail "Commit should have failed due to conflicts"
        | Error (`Msg x) ->
        Alcotest.(check string) "Conflict error" "conflicts file is not empty" x;
        Client.readdir conn (t @ ["rw"]) >>*= fun items ->
        let items = items |> List.map (fun i -> i.Protocol_9p_types.Stat.name) |> List.sort String.compare in
        Alcotest.(check (list string)) "Resolved files"
         ["a"; "b"; "c"; "dir"; "f"; "g"; "h"] items;
        check_file conn (t @ ["rw"; "a"]) "a" "** Conflict **\nChanged on both branches\n" >>= fun () ->
        check_file conn (t @ ["rw"; "b"]) "b" "b-theirs" >>= fun () ->
        check_file conn (t @ ["rw"; "c"]) "c" "c-ours" >>= fun () ->
        check_file conn (t @ ["rw"; "f"]) "f" "** Conflict **\noption: add/del\n" >>= fun () ->
        check_file conn (t @ ["rw"; "g"]) "g" "g-same" >>= fun () ->
        check_file conn (t @ ["rw"; "h"]) "f" "** Conflict **\ndefault: add/add and no common ancestor\n" >>= fun () ->
        check_file conn (t @ ["rw"; "dir"]) "dir" "** Conflict **\nFile vs dir\n" >>= fun () ->
        check_file conn (t @ ["conflicts"]) "conflicts" "a\ndir\nf\nh\n" >>= fun () ->
        Client.remove conn (t @ ["rw"; "a"]) >>*= fun () ->
        Client.remove conn (t @ ["rw"; "dir"]) >>*= fun () ->
        read_file conn (t @ ["theirs"; "f"]) >>= write_file ~truncate:true conn (t @ ["rw"; "f"]) >>*= fun () ->
        check_file conn (t @ ["conflicts"]) "conflicts" "h\n" >>= fun () ->
        Client.remove conn (t @ ["rw"; "h"]) >>*= fun () ->
        Lwt.return ()
      ) >>= fun () ->
      check_dir conn ["branch"; "master"; "ro"] "Final merge result" ["b"; "c"; "f"; "g"] >>= fun () ->
      check_file conn ["branch"; "master"; "ro"; "f"] "f" "f-theirs" >>= fun () ->
      return ()
  )

let test_bad_range () =
  run (fun _repo conn ->
    Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
    Client.mkdir conn ["branch"; "master"; "transactions"] "init" rwxr_xr_x >>*= fun () ->
    Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid
        ["branch"; "master"; "transactions"; "init"; "origin"] >>*= fun _ ->
      Client.LowLevel.openfid conn newfid Protocol_9p.Types.OpenMode.read_only >>*= fun _ ->
      Client.LowLevel.read conn newfid 100L 10l >>= function
      | Ok _ -> Alcotest.fail "out-of-range"
      | Error (`Msg msg) ->
      Alcotest.(check string) "Out-of-range" "Offset 100 beyond end-of-file (len = 0)" msg;
      Lwt.return ()
    )
  )

let test_qids () =
  let open Protocol_9p.Types in
  run (fun _repo conn ->
      Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
      Client.with_fid conn (fun newfid ->
          Client.walk_from_root conn newfid ["branch"; "master"; "head"]
          >>*= fun resp ->
          List.map (fun q ->
              List.mem Qid.Directory q.Qid.flags
            ) resp.Protocol_9p.Response.Walk.wqids
          |> Alcotest.(check (list bool)) "Correct Qid flags" [true; true; false];
          Lwt.return ()
        )
    )

let test_watch () =
  run (fun repo conn ->
      Store.master (fun () -> Irmin.Task.empty) repo >>= fun master ->
      let master = master () in
      Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
      with_stream conn ["branch"; "master"; "watch"; "tree.live"] @@ fun top ->
      read_line_exn top >>= fun top_init ->
      Alcotest.(check string) "Root tree hash initially empty" "" top_init;
      with_stream conn ["branch"; "master"; "watch"; "doc.node"; "tree.live"]
      @@ fun doc ->
      read_line_exn doc >>= fun doc_init ->
      Alcotest.(check string) "Doc tree hash initially empty" "" doc_init;
      Store.update master ["src"; "Makefile"] "all: build" >>= fun () ->
      with_stream conn
        ["branch"; "master"; "watch"; "src.node"; "Makefile.node"; "tree.live"]
      @@ fun makefile ->
      read_line_exn makefile >>= fun makefile_init ->
      Alcotest.(check string) "Makefile hash"
        "F-79f665b460015f226a432bc39e8f9d43053fa05b" makefile_init;
      (* Modify file under doc *)
      let next_make = makefile () in
      Store.update master ["doc"; "README"] "Instructions" >>= fun () ->
      read_line_exn doc >>= fun doc_new ->
      Alcotest.(check string) "Doc update"
        "D-1d8b9cdb86a24803f934575b356d7e2cb3e61a68" doc_new;
      check_dir conn ["trees"; "D-1d8b9cdb86a24803f934575b356d7e2cb3e61a68"]
        "Check /trees/D..." ["README"] >>= fun () ->
      read_line_exn top >>= fun top_new ->
      Alcotest.(check string) "Top update"
        "D-845ffd8bc63ff2f8c9e18a4fdd9b962a01bdf1ef" top_new;
      Alcotest.(check bool) "No Makefile update" true
        (Lwt.state next_make = Lwt.Sleep);
      Lwt.return ()
    )

let test_rename () =
  run (fun repo conn ->
    Store.of_branch_id Irmin.Task.none "old" repo >>= fun old ->
    let old = old () in
    Store.update old ["key"] "value" >>= fun () ->
    Client.mkdir conn ["branch"] "old" rwxr_xr_x >>*= fun () ->
    Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid ["branch"; "old"] >>*= fun _ ->
      Client.LowLevel.update conn ~name:"new" newfid >|= function
      | Error (`Msg x) -> Alcotest.fail x
      | Ok () -> ()
    ) >>= fun () ->
    check_dir conn ["branch"] "New branches" ["new"] >>= fun () ->
    Client.stat conn ["branch"; "new"] >>*= fun info ->
    Alcotest.(check string) "Inode name" "new" info.Protocol_9p.Types.Stat.name;
    let refs = Store.Private.Repo.ref_t repo in
    Store.Private.Ref.mem refs "old" >>= fun old_exists ->
    Store.Private.Ref.mem refs "new" >>= fun new_exists ->
    Alcotest.(check bool) "Old gone" false old_exists;
    Alcotest.(check bool) "New appeared" true new_exists;
    Client.remove conn ["branch"; "new"] >>*= fun () ->
    Store.Private.Ref.mem refs "new" >>= fun new_exists ->
    Alcotest.(check bool) "New gone" false new_exists;
    Lwt.return ()
  )

let test_truncate () =
  let path = ["branch"; "master"; "transactions"; "init"; "rw"; "file"] in
  run (fun _repo conn ->
    let check msg expected =
      read_file conn path >|= Alcotest.(check string) msg expected in
    Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
    Client.mkdir conn ["branch"; "master"; "transactions"] "init" rwxr_xr_x >>*= fun () ->
    Client.create conn ["branch"; "master"; "transactions"; "init"; "rw"] "file" rwxr_xr_x >>*= fun () ->
    Client.write conn path 0L (Cstruct.of_string "Hello") >>*= fun () ->
    Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid path >>*= fun _ ->
      Client.LowLevel.update conn ~length:4L newfid >>*= fun () ->
      check "Truncate to 4" "Hell" >>= fun () ->
      Client.LowLevel.update conn ~length:4L newfid >>*= fun () ->
      check "Truncate to 4 again" "Hell" >>= fun () ->
      Client.LowLevel.update conn ~length:6L newfid >>*= fun () ->
      check "Extend to 6" "Hell\x00\x00" >>= fun () ->
      Client.LowLevel.update conn ~length:0L newfid >>*= fun () ->
      check "Truncate to 0" ""
    )
  )

let test_writes () =
  let ( >>*= ) x f =
    x >>= function
    | Ok y -> f y
    | Error _ -> Alcotest.fail "9p protocol error" in
  let v = ref "" in
  let read () = Lwt.return (Ok (Some (Cstruct.of_string !v))) in
  let write x = v := Cstruct.to_string x; Lwt.return (Ok ()) in
  let remove _ = failwith "delete" in
  let file = I9p_file.read_write ~read ~write ~remove in
  Lwt_main.run begin
    I9p_file.open_ file >>*= fun h ->
    let check src off expect =
      I9p_file.write h ~offset:(Int64.of_int off) (Cstruct.of_string src) >>*= fun () ->
      Alcotest.(check string) (Printf.sprintf "After %S at %d" src off) expect !v;
      Lwt.return () in
    check "hello" 0 "hello" >>= fun () ->
    check "hi" 0 "hillo" >>= fun () ->
    check "E" 1 "hEllo" >>= fun () ->
    check "!" 5 "hEllo!" >>= fun () ->
    check "!" 7 "hEllo!\x00!" >>= fun () ->
    Lwt.return ()
  end

let test_set = [
  "Transaction", `Quick, transaction;
  "Qids", `Quick, test_qids;
  "Watch", `Quick, test_watch;
  "Range", `Quick, test_bad_range;
  "Writes", `Quick, test_writes;
  "Rename", `Quick, test_rename;
  "Truncate", `Quick, test_truncate;
  "Parents", `Quick, test_parents;
  "Merge", `Quick, test_merge;
  "Conflicts", `Quick, test_conflicts;
]

let () =
  Alcotest.run "irmin9p" [
    "tests", test_set;
  ]
