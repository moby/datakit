open Lwt.Infix
open Test_utils
open Result

let root_entries = ["branch"; "snapshots"; "trees"; "remotes"]

let test_transaction repo conn =
  check_dir conn [] "Root entries" root_entries >>= fun () ->
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  check_dir conn ["branch"] "Check master exists" ["master"] >>= fun () ->
  check_dir conn ["branch"; "master"; ".."; ".."] "Check .. works" root_entries
  >>= fun () ->

  Client.mkdir conn ["branch"; "master"; "transactions"] "init" rwxr_xr_x
  >>*= fun () ->
  Client.mkdir conn ["branch"; "master"; "transactions"; "init"; "rw"]
    "src" rwxr_xr_x
  >>*= fun () ->
  create_file conn ["branch"; "master"; "transactions"; "init"; "rw"; "src"]
    "Makefile" "all: build test"
  >>= fun () ->
  write_file conn ["branch"; "master"; "transactions"; "init"; "msg"]
    "My commit"
  >>*= fun () ->
  check_file conn ["branch"; "master"; "transactions"; "init"; "parents"]
    "Parents" ""
  >>= fun () ->
  echo conn "commit" ["branch"; "master"; "transactions"; "init"; "ctl"]
  >>= fun () ->

  Client.stat conn ["branch"; "master"; "ro"; "src"; "Makefile"]
  >>*= fun info ->
  let length = Int64.to_int info.Protocol_9p.Types.Stat.length in
  Alcotest.(check int) "File size" 15 length;

  Store.master Irmin.Task.none repo >>= fun master ->
  Store.head_exn (master ()) >>=
  Store.Repo.task_of_commit_id repo >>= fun task ->
  let msg = Irmin.Task.messages task in
  Alcotest.(check (list string)) "Message" ["My commit"] msg;

  Lwt.return_unit

let test_parents _repo conn =
  let check_parents ~branch expected =
    read_file conn ["branch"; branch; "head"] >>= function
    | "\n" ->
      Alcotest.(check string) "Parents" expected "no-commit";
      Lwt.return_unit
    | hash ->
      read_file conn ["snapshots"; String.trim hash; "parents"]
      >|= fun parents ->
      Alcotest.(check string) "Parents" expected parents in
  let check_fails name parents =
    Lwt.try_bind
      (fun () ->
         with_transaction conn ~branch:"dev" name (fun dir ->
             write_file conn (dir @ ["parents"]) parents >>*= Lwt.return
           )
      )
      (fun () -> Alcotest.fail "Should have been rejected")
      (fun _ex -> Lwt.return_unit)
  in

  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  check_parents ~branch:"master" "no-commit" >>= fun () ->

  with_transaction conn ~branch:"master" "commit1" (fun dir ->
      create_file conn (dir @ ["rw"]) "file" "data"
    ) >>= fun () ->
  check_parents ~branch:"master" "" >>= fun () ->
  read_file conn ["branch"; "master"; "head"] >>= fun master_head ->

  with_transaction conn ~branch:"master" "commit2" (fun dir ->
      create_file conn (dir @ ["rw"]) "file" "data2"
    ) >>= fun () ->
  check_parents ~branch:"master" master_head >>= fun () ->
  read_file conn ["branch"; "master"; "head"] >>= fun master_head ->

  Client.mkdir conn ["branch"] "dev" rwxr_xr_x >>*= fun () ->
  with_transaction conn ~branch:"dev" "commit3" (fun dir ->
      create_file conn (dir @ ["rw"]) "file" "dev" >>= fun () ->
      write_file conn (dir @ ["parents"]) master_head >>*= Lwt.return
    ) >>= fun () ->
  check_parents ~branch:"dev" master_head >>= fun () ->

  check_fails "Invalid hash" "hello" >>= fun () ->
  check_fails "Missing hash" "a3827c5d1a2ba8c6a40eded5598dba8d3835fb35"
  >>= fun () ->

  read_file conn ["branch"; "dev"; "head"] >>= fun dev_head ->
  with_transaction conn ~branch:"master" "From outer" (fun t1 ->
      with_transaction conn ~branch:"master" "From inner" (fun t2 ->
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
  let inner, c2, c3 =
    match history with
    | {id = _; msg = _; parents = [
        {id = inner; msg = "From inner"; parents = [_]};
        {id = _; msg = "From outer"; parents = [
             {id = c2; msg = "commit2"; parents = [_]};
             {id = c3; msg = "commit3"; parents = [_]};
           ]
        };
      ]} -> inner, c2, c3
    | x -> Alcotest.fail (Format.asprintf "Bad history:@\n%a" pp_history x)
  in
  Alcotest.(check string) "First parent" after_inner (inner ^ "\n");
  Alcotest.(check string) "Dev parent" dev_head (c3 ^ "\n");
  Alcotest.(check string) "Orig parent" orig_parent (c2 ^ "\n");
  Lwt.return ()

let test_merge _repo conn =
  (* Put "from-master" on master branch *)
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  with_transaction conn ~branch:"master" "init" (fun t ->
      create_file conn (t @ ["rw"]) "file" "from-master"
    ) >>= fun () ->
  (* Fork and put "from-master+pr" on pr branch *)
  Client.mkdir conn ["branch"] "pr" rwxr_xr_x >>*= fun () ->
  head conn "master" >>= fun merge_a ->
  write_file conn ["branch"; "pr"; "fast-forward"]
    "a3827c5d1a2ba8c6a40eded5598dba8d3835fb35"
  >>= function
  | Ok () -> Alcotest.fail "Commit not in store!"
  | Error _ ->
  write_file conn ["branch"; "pr"; "fast-forward"] merge_a >>*= fun () ->
  with_transaction conn ~branch:"pr" "mod" (fun t ->
      read_file conn (t @ ["rw"; "file"]) >>= fun old ->
      write_file conn (t @ ["rw"; "file"]) (old ^ "+pr") >>*=
      Lwt.return
    ) >>= fun () ->
  head conn "pr" >>= fun merge_b ->
  (* Merge pr into master *)
  with_transaction conn ~branch:"master" "merge" (fun t ->
      create_file conn (t @ ["rw"]) "mine" "pre-merge" >>= fun () ->
      write_file conn (t @ ["merge"]) merge_b >>*= fun () ->
      check_file conn (t @ ["ours"; "mine"]) "Ours" "pre-merge" >>= fun () ->
      check_file conn (t @ ["theirs"; "file"]) "Theirs" "from-master+pr"
      >>= fun () ->
      check_file conn (t @ ["base"; "file"]) "Base" "from-master"
    ) >>= fun () ->
  head conn "master" >>= fun merge_commit ->
  read_file conn ["snapshots"; merge_commit; "parents"] >>= fun parents ->
  let parents = Str.(split (regexp "\n")) parents in
  Alcotest.(check @@ slist string String.compare) "Merge parents"
    [merge_b; merge_a] parents;
  read_file conn ["branch"; "master"; "ro"; "file"] >>= fun merged ->
  Alcotest.(check string) "Merge result" "from-master+pr" merged;
  Lwt.return ()

let test_merge_metadata _repo conn =
  (* Put "from-master" on master branch *)
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  with_transaction conn ~branch:"master" "init" (fun t ->
      create_file conn (t @ ["rw"]) "a" "from-master" ~perm:rwxr_xr_x >>= fun () ->
      check_perm conn (t @ ["rw"; "a"]) "Create a" `Executable >>= fun () ->
      create_file conn (t @ ["rw"]) "b" "from-master" ~perm:symlink >>= fun () ->
      create_file conn (t @ ["rw"]) "c" "from-master" ~perm:rwxr_xr_x >>= fun () ->
      (* Base: exec, link, exec *)
      Lwt.return ()
    ) >>= fun () ->
  (* Fork and make some changes on pr branch *)
  Client.mkdir conn ["branch"] "pr" rwxr_xr_x >>*= fun () ->
  head conn "master" >>= fun merge_a ->
  with_transaction conn ~branch:"pr" "mod" (fun t ->
      write_file conn (t @ ["merge"]) merge_a >>*= fun () ->
      check_perm conn (t @ ["rw"; "a"]) "pr/rw/a" `Executable >>= fun () ->
      chmod conn (t @ ["rw"; "b"]) `Executable >>= fun () ->
      Client.remove conn (t @ ["rw"; "c"]) >>*= fun () ->
      create_file conn (t @ ["rw"]) "c" ~perm:symlink "foo" >>= fun () ->
      Lwt.return ()
    ) >>= fun () ->
  head conn "pr" >>= fun merge_b ->
  (* Merge pr into master *)
  with_transaction conn ~branch:"master" "merge" (fun t ->
      chmod conn (t @ ["rw"; "c"]) `Normal >>= fun () ->
      (* Begin the merge *)
      write_file conn (t @ ["merge"]) merge_b >>*= fun () ->
      (* Ours: exec, link, normal *)
      check_perm conn (t @ ["ours"; "a"]) "ours/a" `Executable >>= fun () ->
      check_perm conn (t @ ["ours"; "b"]) "ours/b" `Link >>= fun () ->
      (* Theirs: exec, exec, link *)
      check_perm conn (t @ ["theirs"; "b"]) "theirs/b" `Executable >>= fun () ->
      (* RW: exec, exec, conflict(normal) *)
      check_perm conn (t @ ["rw"; "b"]) "rw/b" `Executable >>= fun () ->
      check_perm conn (t @ ["rw"; "c"]) "rw/c" `Normal >>= fun () ->
      check_file conn (t @ ["rw"; "c"]) "Conflict"
        "** Conflict **\nChanged on both branches\n"
      >>= fun () ->
      write_file conn (t @ ["rw"; "c"]) "Resolved" >>*= fun () ->
      chmod conn (t @ ["rw"; "c"]) `Executable >>= fun () ->
      Lwt.return ()
    ) >>= fun () ->
  check_perm conn ["branch"; "master"; "ro"; "c"] "ro/c" `Executable
  >>= fun () ->
  Lwt.return ()

(* Irmin.Git.Commit: a commit with an empty filesystem... this is not
   supported by Git! *)
let test_merge_empty _repo conn =
  (* Put "from-master" on master branch *)
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  with_transaction conn ~branch:"master" "init" (fun t ->
      create_file conn (t @ ["rw"]) "file" "from-master"
    ) >>= fun () ->
  read_file conn ["branch"; "master"; "head"] >>= fun head ->
  (* Create a new branch, and merge from this *)
  Client.mkdir conn ["branch"] "new" rwxr_xr_x >>*= fun () ->
  with_transaction conn ~branch:"new" "init" (fun t ->
      write_file conn (t @ ["merge"]) head
    ) >>*= fun () ->
  check_dir conn ["branch"; "new"; "ro"] "Final result" ["file"]

let test_conflicts _repo conn =
  try_merge conn
    ~base:[
      "a", "a-from-base";
      "b", "b-from-base";
      "c", "c-from-base";
      "d", "d-from-base";
      "e", "e-from-base";
      "f", "f-from-base";
      "dir/a", "a-from-base";
      "dir2/b", "b-from-base";
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
      "dir2/b", "b-theirs";   (* edit dir2/b *)
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
      "dir2", "theirs-now-file"; (* convert dir2 to file *)
    ]
    (fun t ->
       write_file conn (t @ ["ctl"]) "commit" >>= function
       | Ok () -> Alcotest.fail "Commit should have failed due to conflicts"
       | Error (`Msg _x) ->
         (* Alcotest.(check string) "Conflict error"
            "conflicts file is not empty" x; *)
         Client.readdir conn (t @ ["rw"]) >>*= fun items ->
         let items =
           items
           |> List.map (fun i -> i.Protocol_9p_types.Stat.name)
           |> List.sort String.compare
         in
         Alcotest.(check (list string)) "Resolved files"
           ["a"; "b"; "c"; "dir"; "dir2"; "f"; "g"; "h"] items;
         check_file conn (t @ ["rw"; "a"]) "a"
           "** Conflict **\nChanged on both branches\n"
         >>= fun () ->
         check_file conn (t @ ["rw"; "b"]) "b" "b-theirs" >>= fun () ->
         check_file conn (t @ ["rw"; "c"]) "c" "c-ours" >>= fun () ->
         check_file conn (t @ ["rw"; "f"]) "f" "** Conflict **\noption: add/del\n"
         >>= fun () ->
         check_file conn (t @ ["rw"; "g"]) "g" "g-same" >>= fun () ->
         check_file conn (t @ ["rw"; "h"]) "f"
           "** Conflict **\ndefault: add/add and no common ancestor\n"
         >>= fun () ->
         check_file conn (t @ ["rw"; "dir"]) "dir" "** Conflict **\nFile vs dir\n"
         >>= fun () ->
         check_file conn (t @ ["conflicts"]) "conflicts" "a\ndir\ndir2\nf\nh\n"
         >>= fun () ->
         Client.remove conn (t @ ["rw"; "a"]) >>*= fun () ->
         Client.remove conn (t @ ["rw"; "dir"]) >>*= fun () ->
         Client.remove conn (t @ ["rw"; "dir2"]) >>*= fun () ->
         read_file conn (t @ ["theirs"; "f"]) >>=
         write_file ~truncate:true conn (t @ ["rw"; "f"]) >>*= fun () ->
         check_file conn (t @ ["conflicts"]) "conflicts" "h\n" >>= fun () ->
         Client.remove conn (t @ ["rw"; "h"]) >>*= fun () ->
         Lwt.return ()
    ) >>= fun () ->
  check_dir conn ["branch"; "master"; "ro"] "Final merge result"
    ["b"; "c"; "f"; "g"]
  >>= fun () ->
  check_file conn ["branch"; "master"; "ro"; "f"] "f" "f-theirs" >>= fun () ->
  Lwt.return_unit

let test_bad_range _repo conn =
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  Client.mkdir conn ["branch"; "master"; "transactions"] "init" rwxr_xr_x
  >>*= fun () ->
  Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid
        ["branch"; "master"; "transactions"; "init"; "origin"] >>*= fun _ ->
      Client.LowLevel.openfid conn newfid Protocol_9p.Types.OpenMode.read_only
      >>*= fun _ ->
      Client.LowLevel.read conn newfid 100L 10l >>= function
      | Ok _ -> Alcotest.fail "out-of-range"
      | Error (`Msg msg) ->
        Alcotest.(check string) "Out-of-range"
          "Offset 100 beyond end-of-file (len = 0)" msg;
        Lwt.return (Ok ())
    ) >>*=
  Lwt.return

let test_qids _repo conn =
  let open Protocol_9p.Types in
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid ["branch"; "master"; "head"]
      >>*= fun resp ->
      List.map (fun q ->
          List.mem Qid.Directory q.Qid.flags
        ) resp.Protocol_9p.Response.Walk.wqids
      |> Alcotest.(check (list bool)) "Correct Qid flags" [true; true; false];
      Lwt.return (Ok ())
    ) >>*=
  Lwt.return

let test_watch repo conn =
  Store.master (fun () -> Irmin.Task.empty) repo >>= fun master ->
  let master = master () in
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  with_stream conn ["branch"; "master"; "watch"; "tree.live"] @@ fun top ->
  read_line_exn top >>= fun top_init ->
  Alcotest.(check string)
    "Root tree initially empty" "D-4b825dc642cb6eb9a060e54bf8d69288fbee4904"
    top_init;
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
    "F-d81e367f87ee314bcd3e449b1c6641efda5bc269" makefile_init;
  (* Modify file under doc *)
  let next_make = makefile () in
  Store.update master ["doc"; "README"] "Instructions" >>= fun () ->
  read_line_exn doc >>= fun doc_new ->
  Alcotest.(check string) "Doc update"
    "D-a3e8adf6d194bfbdec2ca73aebe0990edee2ddbf" doc_new;
  check_dir conn ["trees"; "D-a3e8adf6d194bfbdec2ca73aebe0990edee2ddbf"]
    "Check /trees/D..." ["README"] >>= fun () ->
  read_line_exn top >>= fun top_new ->
  Alcotest.(check string) "Top update"
    "D-acaa8ac97706d94e012da475e8a63d647011a72c" top_new;
  Alcotest.(check bool) "No Makefile update" true
    (Lwt.state next_make = Lwt.Sleep);
  Lwt.return_unit

let test_rename_branch repo conn =
  Store.of_branch_id Irmin.Task.none "old" repo >>= fun old ->
  let old = old () in
  Store.update old ["key"] "value" >>= fun () ->
  Client.mkdir conn ["branch"] "old" rwxr_xr_x >>*= fun () ->
  Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid ["branch"; "old"] >>*= fun _ ->
      Client.LowLevel.update conn ~name:"new" newfid >|= function
      | Error (`Msg x) -> Alcotest.fail x
      | Ok () -> Ok ()
    ) >>*= fun () ->
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
  Lwt.return_unit

let test_rename_file _repo conn =
  make_branch conn "master" >>= fun () ->
  with_transaction conn ~branch:"master" "rename" (fun t ->
    create_file conn (t @ ["rw"]) "old" "data" >>= fun () ->
    Client.with_fid conn (fun newfid ->
      Client.walk_from_root conn newfid (t @ ["rw"; "old"]) >>*= fun _ ->
      Client.LowLevel.update conn ~name:"new" newfid >>*= fun () ->
      check_dir conn (t @ ["rw"]) "New files" ["new"] >>= fun () ->
      (* Check rename detects errors
         Note: we currently allow overwriting an empty directory *)
      Client.mkdir conn (t @ ["rw"]) "dir" rwxr_xr_x >>*= fun () ->
      create_file conn (t @ ["rw"; "dir"]) "precious" "data" >>= fun () ->
      Client.LowLevel.update conn ~name:"dir" newfid >>= function
      | Ok () -> Alcotest.fail "Shouldn't be able to rename over a directory"
      | Error (`Msg e) ->
      Alcotest.(check string) "Rename over dir" "Is a directory" e;
      (* Rename when source has been deleted. Ideally, we should also
         check it hasn't been replaced by something with the same
         name. *)
      Client.remove conn (t @ ["rw"; "new"]) >>*= fun () ->
      Client.LowLevel.update conn ~name:"reborn" newfid >>= function
      | Ok () -> Alcotest.fail "Shouldn't be able to rename a missing source"
      | Error (`Msg e) ->
      Alcotest.(check string) "Source deleted" "No such file or directory" e;
      Lwt.return (Ok ())
    )
  ) >>*= fun () ->
  check_dir conn ["branch"; "master"; "ro"] "New files" ["dir"]

let test_truncate _repo conn =
  let path = ["branch"; "master"; "transactions"; "init"; "rw"; "file"] in
  let check msg expected =
    read_file conn path >|= Alcotest.(check string) msg expected
  in
  Client.mkdir conn ["branch"] "master" rwxr_xr_x >>*= fun () ->
  Client.mkdir conn ["branch"; "master"; "transactions"] "init" rwxr_xr_x
  >>*= fun () ->
  Client.create conn ["branch"; "master"; "transactions"; "init"; "rw"]
    "file" rwxr_xr_x >>*= fun () ->
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
      check "Truncate to 0" "" >>= fun () ->
      Lwt.return (Ok ())
    ) >>*=
  Lwt.return

(* FIXME: automaticall run ./scripts/git-dumb-server *)
let test_remotes _repo conn =
  check_dir conn [] "Root entries" root_entries >>= fun () ->
  Client.mkdir conn ["remotes"] "origin" rwxr_xr_x  >>*= fun () ->
  check_dir conn ["remotes"] "Remotes entries" ["origin"] >>= fun () ->
  check_dir conn ["remotes";"origin"] "Remote files" ["url"; "fetch"; "head"]
  >>= fun () ->
  write_file conn ["remotes";"origin";"url"] "git://localhost/" >>*= fun () ->
  write_file conn ["remotes";"origin";"fetch"] "master" >>*= fun () ->
  let remote_head = "ecf6b63a94681222b1be76c0f95159122ce80db1" in
  with_stream conn ["remotes"; "origin"; "head"] (fun head ->
      read_line_exn head >|= fun head ->
      Alcotest.(check string) "remote head 1" remote_head head
    ) >>= fun () ->
  with_stream conn ["remotes"; "origin"; "head"] (fun head ->
      read_line_exn head >|= fun head ->
      Alcotest.(check string) "remote head 2" remote_head head;
    ) >>= fun () ->
  check_dir conn ["snapshots"; remote_head; "ro"] "Remote entries"
    ["foo";"x"] >>= fun () ->
  Lwt.return_unit

let test_writes () =
  let ( >>*= ) x f =
    x >>= function
    | Ok y -> f y
    | Error _ -> Alcotest.fail "9p protocol error" in
  let v = ref "" in
  let read () = Lwt.return (Ok (Some (Cstruct.of_string !v))) in
  let write x = v := Cstruct.to_string x; Lwt.return (Ok ()) in
  let remove _ = failwith "delete" in
  let chmod _ = failwith "chmod" in
  let file =
    Vfs.File.of_kv ~read ~write ~remove ~stat:(Vfs.File.stat_of ~read) ~chmod
  in
  Lwt_main.run begin
    Vfs.File.open_ file >>*= fun h ->
    let check src off expect =
      Vfs.File.write h ~offset:(Int64.of_int off) (Cstruct.of_string src)
      >>*= fun () ->
      Alcotest.(check string) (Printf.sprintf "After %S at %d" src off)
        expect !v;
      Lwt.return_unit
    in
    check "hello" 0 "hello" >>= fun () ->
    check "hi" 0 "hillo" >>= fun () ->
    check "E" 1 "hEllo" >>= fun () ->
    check "!" 5 "hEllo!" >>= fun () ->
    check "!" 7 "hEllo!\x00!" >>= fun () ->
    Lwt.return_unit
  end

let test_stable_inodes _repo conn =
  let inode x = Int64.to_string Protocol_9p.(x.Types.Stat.qid.Types.Qid.id) in
  make_branch conn "master" >>= fun () ->
  with_transaction conn ~branch:"master" "stable" (fun t ->
    create_file conn (t @ ["rw"]) "file" "data" >>= fun () ->
    Client.stat conn
      ["branch"; "master"; "transactions"; "stable"; "rw"; "file"]
    >>*= fun info1 ->
    Client.stat conn
      ["branch"; "master"; "transactions"; "stable"; "rw"; "file"]
    >>*= fun info2 ->
    Alcotest.(check string) "Inode same" (inode info1) (inode info2);
    Lwt.return (Ok ())
  ) >>*= Lwt.return

module Unit = struct
  type t = unit
  let pp fmt () = Fmt.string fmt "()"
  let equal = (=)
end

module RW_err = struct
  type t = [`Not_a_directory | `Is_a_directory]
  let pp fmt = function
    | `Not_a_directory -> Fmt.string fmt "Not_a_directory"
    | `Is_a_directory -> Fmt.string fmt "Is_a_directory"
  let equal = (=)
end

module RW_err1 = struct
  type t = [`Not_a_directory]
  let pp fmt = function
    | `Not_a_directory -> Fmt.string fmt "Not_a_directory"
  let equal = (=)
end

let test_rw () =
  let v x = Ivfs_blob.of_string x, `Normal in
  let err = (module RW_err : Alcotest.TESTABLE with type t = RW_err.t) in
  let err1 = (module RW_err1 : Alcotest.TESTABLE with type t = RW_err1.t) in
  let unit = (module Unit : Alcotest.TESTABLE with type t = unit) in
  Lwt_main.run begin
    Store.Repo.create config >>= fun repo ->
    let rw = RW.of_dir (Tree.Dir.empty repo) in

    RW.update rw [] "foo" (v "a")
    >|= Alcotest.(check (result unit err)) "Write /a" (Ok ()) >>= fun () ->

    RW.update rw ["sub"; "bar"] "baz" (v "b")
    >|= Alcotest.(check (result unit err)) "Write /sub/bar/baz" (Ok ())
    >>= fun () ->

    (* /foo is a file *)
    RW.update rw ["foo"; "bar"] "baz" (v "b")
    >|= Alcotest.(check (result unit err)) "Write /foo/bar/baz"
      (Error `Not_a_directory)
    >>= fun () ->

    RW.remove rw ["foo"] "bar"
    >|= Alcotest.(check (result unit err1)) "rm /foo/bar"
      (Error `Not_a_directory)
    >>= fun () ->

    RW.update_force rw ["foo"; "bar"] "baz" (v "b") >>= fun () ->

    RW.update rw ["foo"] "bar" (v "b")
    >|= Alcotest.(check (result unit err)) "Write /foo/bar"
      (Error `Is_a_directory)
    >>= fun () ->

    RW.remove rw ["foo"; "bar"] "baz"
    >|= Alcotest.(check (result unit err1)) "rm /foo/bar/baz" (Ok ())
    >>= fun () ->

    let root = RW.root rw in

    Tree.Dir.ls root
    >|= List.map snd
    >|= Alcotest.(check (slist string String.compare)) "ls /" ["foo"; "sub"]
    >>= fun () ->

    Lwt.return ()
  end

let test_blobs_fast_path () =
  let correct = ref (Cstruct.create 0) in
  let blob = ref Ivfs_blob.empty in
  for _ = 1 to 100 do
    let data = Cstruct.create (Random.int 10) in
    for j = 0 to Cstruct.len data - 1 do
      Cstruct.set_uint8 data j (Random.int 26 + 65)
    done;
    correct := Cstruct.append !correct data;
    Ivfs_blob.write !blob ~offset:(Ivfs_blob.len !blob) data >>!= fun b ->
    blob := b
  done;
  let correct = Cstruct.to_string !correct in
  let actual = Ivfs_blob.to_string !blob in
  Alcotest.(check string) "Fast-append worked" correct actual

let test_blobs_random () =
  let int64 = Alcotest.of_pp Fmt.int64 in
  let str b = Ivfs_blob.to_string b in
  let read_ok b ~offset ~count =
    Ivfs_blob.read b ~offset ~count >>!= Cstruct.to_string
  in
  let write_ok b ~offset data =
    Ivfs_blob.write b ~offset (Cstruct.of_string data) >>!= fun x -> x
  in
  let truncate_ok b len = Ivfs_blob.truncate b len >>!= fun x -> x in
  (* Empty *)
  let b = Ivfs_blob.empty in
  Alcotest.check int64 "Empty" 0L (Ivfs_blob.len b);
  (* Negative offset write *)
  let bad_write = Ivfs_blob.write b ~offset:(-2L) (Cstruct.of_string "bad") in
  Alcotest.check (vfs_result reject) "Negative offset"
    (Vfs.Error.negative_offset (-2L)) bad_write;
  (* Write *)
  let b = write_ok b ~offset:2L "1st" in
  Alcotest.check int64 "1st write" 5L (Ivfs_blob.len b);
  Alcotest.(check string) "Append with gap" "\x00\x001st" (str b);
  let b = write_ok b ~offset:0L "AB" in
  Alcotest.(check string) "Overwrite" "AB1st" (str b);
  Alcotest.(check string) "Overwrite 2" "ABCDt" (str (write_ok b ~offset:2L "CD"));
  Alcotest.(check string) "Overwrite extend" "AB1sEF"
    (str (write_ok b ~offset:4L "EF"));
  (* Truncate *)
  Alcotest.(check string) "Truncate extend" "AB1st\x00" (str (truncate_ok b 6L));
  Alcotest.(check string) "Truncate same" "AB1st" (str (truncate_ok b 5L));
  Alcotest.(check string) "Truncate short" "AB1" (str (truncate_ok b 3L));
  Alcotest.(check string) "Truncate zero" "" (str (truncate_ok b 0L));
  Alcotest.check (vfs_result reject) "Truncate negative"
    (Vfs.Error.negative_offset (-1L)) (Ivfs_blob.truncate b (-1L));
  (* Read *)
  Alcotest.(check string) "Read neg" "" (read_ok b ~offset:2L ~count:(-3));
  Alcotest.(check string) "Read zero" "" (read_ok b ~offset:2L ~count:0);
  Alcotest.(check string) "Read short" "1s" (read_ok b ~offset:2L ~count:2);
  Alcotest.(check string) "Read full" "1st" (read_ok b ~offset:2L ~count:3);
  Alcotest.(check string) "Read long" "1st" (read_ok b ~offset:2L ~count:4);
  Alcotest.(check string) "Read EOF" "" (read_ok b ~offset:5L ~count:4);
  Alcotest.check (vfs_result reject) "Read negative"
    (Vfs.Error.negative_offset (-1L)) (Ivfs_blob.read b ~offset:(-1L) ~count:1);
  Alcotest.check (vfs_result reject) "Read after EOF"
    (Vfs.Error.offset_too_large ~offset:6L 5L) (Ivfs_blob.read b ~offset:6L ~count:1);
  ()

let test_streams () =
  let ( >>*= ) x f =
    x >>= function
    | Ok y -> f y
    | Error _ -> Alcotest.fail "VFS error" in
  Lwt_main.run begin
    let session = Vfs.File.Stream.session 0 in
    let s = Vfs.File.Stream.create Fmt.int session in
    let f = Vfs.File.of_stream (fun () -> Lwt.return s) in
    Vfs.File.open_ f >>*= fun fd ->
    let offset = ref 0L in
    let rec read ?(saw_flush=false) expect =
      Vfs.File.read fd ~offset:!offset ~count:1000 >>*= fun data ->
      match Cstruct.to_string data with
      | "" when saw_flush -> Alcotest.fail "End-of-file!"
      | "" -> read ~saw_flush:true expect
      | data ->
      offset := Int64.add !offset (Int64.of_int (String.length data));
      Alcotest.(check string) "read" expect data;
      Lwt.return () in
    read "0" >>= fun () ->
    Vfs.File.Stream.publish session 1;
    read "1" >>= fun () ->
    Vfs.File.Stream.publish session 2;
    Vfs.File.Stream.publish session 3;
    read "3" >>= fun () ->
    let th = read "4" in
    Vfs.File.Stream.publish session 4;
    th >>= fun () ->
    Lwt.return ()
  end

let run f () = Test_utils.run f

let test_set = [
  "Transaction", `Quick, run test_transaction;
  "Qids"       , `Quick, run test_qids;
  "Watch"      , `Quick, run test_watch;
  "Range"      , `Quick, run test_bad_range;
  "Writes"     , `Quick, test_writes;
  "Rename branch", `Quick, run test_rename_branch;
  "Rename file", `Quick, run test_rename_file;
  "Truncate"   , `Quick, run test_truncate;
  "Parents"    , `Quick, run test_parents;
  "Merge"      , `Quick, run test_merge;
  "Merge_metadata", `Quick, run test_merge_metadata;
  "Merge empty", `Quick, run test_merge_empty;
  "Conflicts"  , `Quick, run test_conflicts;
  "Stable inodes", `Quick, run test_stable_inodes;
  "RW"         , `Quick, test_rw;
  "Remotes"    , `Slow , run test_remotes;
  "Blobs fast" , `Quick , test_blobs_fast_path;
  "Blobs random", `Quick , test_blobs_random;
  "Streams",    `Quick , test_streams;
]

let () =
  Alcotest.run "datakit" [
    "server", test_set;
    "client", Test_client.test_set;
    "github", Test_github.test_set;
  ]
