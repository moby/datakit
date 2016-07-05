open Lwt.Infix
open Test_utils
open Result

let test_transaction dk =
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun tr ->
      DK.Transaction.create_dir tr ~dir:(p "") "src" >>*= fun () ->
      let makefile = Cstruct.of_string "all: build test" in
      DK.Transaction.create_file tr ~executable:false ~dir:(p "src")
        "Makefile" makefile >>*= fun () ->
      DK.Transaction.parents tr >>*= fun parents ->
      Alcotest.(check (list pass)) "Parents" [] parents;
      DK.Transaction.commit tr ~message:"My commit"
    ) >>*= fun () ->
  DK.Branch.head master >>*= function
  | None -> Alcotest.fail "Branch does not exist!"
  | Some head ->
  let root = DK.Commit.tree head in
  DK.Tree.stat root (p "src/Makefile") >>*= function
  | None -> Alcotest.fail "Missing Makefile!"
  | Some {Datakit_S.size; _} ->
  Alcotest.(check int) "File size" 15 (Int64.to_int size);
  DK.Commit.message head >>*= fun msg ->
  Alcotest.(check string) "Message" "My commit\n" msg;
  Lwt.return_unit

let test_parents dk =
  let check_parents name branch expected =
    let expected = List.map DK.Commit.id expected in
    DK.Branch.head branch >>*= begin function
    | None -> Lwt.return []
    | Some commit ->
      DK.Commit.parents commit >>*= fun parents ->
      Lwt.return (List.map DK.Commit.id parents)
    end
    >|= Alcotest.(check (list string)) name expected in
  DK.branch dk "dev" >>*= fun dev ->
  let check_fails name parents =
    let parents = List.map (DK.commit dk) parents in
    DK.Branch.with_transaction dev (fun tr ->
        DK.Transaction.set_parents tr parents >>*= fun () ->
        DK.Transaction.commit tr ~message:name >>= function
        | Ok () -> Alcotest.fail ("Should have been rejected: " ^ name)
        | Error _ -> DK.Transaction.abort tr >|= fun () -> Ok ()
      )
  in

  DK.branch dk "master" >>*= fun master ->
  check_parents "master empty" master [] >>= fun () ->

  DK.Branch.with_transaction master (fun tr ->
      DK.Transaction.create_file tr ~executable:false ~dir:(p "")
        "file" (v "data")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"commit1"
    ) >>*= fun () ->
  check_parents "master no parent" master [] >>= fun () ->
  expect_head master >>*= fun master_commit1 ->

  DK.Branch.with_transaction master (fun tr ->
      DK.Transaction.create_file tr ~executable:false ~dir: (p "")
        "file" (v "data2")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"commit2"
    ) >>*= fun () ->
  check_parents "master second commit" master [master_commit1] >>= fun () ->
  expect_head master >>*= fun master_commit2 ->

  DK.Branch.with_transaction dev (fun tr ->
      DK.Transaction.create_file tr ~executable:false ~dir: (p "")
        "file" (v "dev")
      >>*= fun () ->
      DK.Transaction.set_parents tr [master_commit2] >>*= fun () ->
      DK.Transaction.commit tr ~message:"commit3"
    ) >>*= fun () ->
  check_parents "dev from master" dev [master_commit2] >>= fun () ->

  check_fails "Invalid hash" ["hello"] >>*= fun () ->
  check_fails "Missing hash" ["a3827c5d1a2ba8c6a40eded5598dba8d3835fb35"]
  >>*= fun () ->

  expect_head dev >>*= fun dev_head ->
  DK.Branch.with_transaction master (fun t1 ->
      DK.Branch.with_transaction master (fun t2 ->
          DK.Transaction.create_file t2 ~executable:false ~dir: (p "")
            "from_inner" (v "inner")
          >>*= fun () ->
          DK.Transaction.commit t2 ~message:"From inner"
        ) >>*= fun () ->
      expect_head master >>*= fun after_inner ->
      DK.Transaction.create_file t1 ~executable:false ~dir: (p "")
        "from_outer" (v "outer")
      >>*= fun () ->
      DK.Transaction.parents t1 >>*= function
      | [] | _::_::_ -> Alcotest.fail "Expected one parent"
      | [real_parent] ->
      DK.Transaction.set_parents t1 [real_parent; dev_head] >>*= fun () ->
      DK.Transaction.commit t1 ~message:"From outer" >>*= fun () ->
      Lwt.return (Ok (real_parent, after_inner))
    ) >>*= fun (orig_parent, after_inner) ->
  (* Now we should have two merges: master is a merge of t1 and t2,
     and t1 is a merge of master and dev. *)
  expect_head master >>*= fun new_head ->
  history_client new_head >>= fun history ->
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
  Alcotest.(check string) "First parent" (DK.Commit.id after_inner) inner;
  Alcotest.(check string) "Dev parent" (DK.Commit.id dev_head) c3;
  Alcotest.(check string) "Orig parent" (DK.Commit.id orig_parent) c2;
  Lwt.return ()

let check_file msg expected read =
  read >>*= fun actual ->
  Alcotest.(check string) msg expected (Cstruct.to_string actual);
  Lwt.return ()

let pp_kind f = function
  | `File -> Fmt.string f "File"
  | `Link -> Fmt.string f "Link"
  | `Dir -> Fmt.string f "Dir"
  | `Exec -> Fmt.string f "Exec"

let check_kind msg expected stat =
  stat >>*= function
  | None -> Alcotest.fail "Missing file"
  | Some actual ->
  Alcotest.(check (of_pp pp_kind)) msg expected actual.Datakit_S.kind;
  Lwt.return ()

let test_merge dk =
  (* Put "from-master" on master branch *)
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun tr ->
      DK.Transaction.create_file tr ~dir:(p "") "file" (v "from-master")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"init"
    ) >>*= fun () ->
  (* Fork and put "from-master+pr" on pr branch *)
  DK.branch dk "pr" >>*= fun pr ->
  expect_head master >>*= fun merge_a ->
  let bad_commit = DK.commit dk "a3827c5d1a2ba8c6a40eded5598dba8d3835fb35" in
  DK.Branch.fast_forward pr bad_commit >>= function
  | Ok () -> Alcotest.fail "Commit not in store!"
  | Error _ ->
  DK.Branch.fast_forward pr merge_a >>*= fun () ->
  DK.Branch.with_transaction pr (fun tr ->
      DK.Transaction.read_file tr (p "file") >>*= fun old ->
      DK.Transaction.replace_file tr ~dir:(p "") "file"
        (Cstruct.append old (v "+pr"))
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"mod"
    ) >>*= fun () ->
  expect_head pr >>*= fun merge_b ->
  (* Merge pr into master *)
  DK.Branch.with_transaction master (fun tr ->
      DK.Transaction.create_file tr ~dir:(p "") "mine" (v "pre-merge") >>*= fun () ->
      DK.Transaction.merge tr merge_b
      >>*= fun ({DK.Transaction.ours; theirs; base}, conflicts) ->
      if conflicts <> [] then Alcotest.fail "Conflicts on merge!";
      DK.Tree.read_file ours   (p "mine") |> check_file "Ours" "pre-merge"
      >>= fun () ->
      DK.Tree.read_file theirs (p "file") |> check_file "Theirs" "from-master+pr"
      >>= fun () ->
      DK.Tree.read_file base   (p "file") |> check_file "Base" "from-master"
      >>= fun () ->
      DK.Transaction.commit tr ~message:"merge"
    ) >>*= fun () ->
  expect_head master >>*= fun merge_commit ->
  DK.Commit.parents merge_commit >>*= fun parents ->
  Alcotest.(check @@ slist commit compare_commit) "Merge parents"
    [merge_b; merge_a] parents;
  DK.Tree.read_file (DK.Commit.tree merge_commit) (p "file") >>*= fun merged ->
  Alcotest.(check string) "Merge result"
    "from-master+pr" (Cstruct.to_string merged);
  Lwt.return ()

let test_merge_metadata dk =
  (* Put "from-master" on master branch *)
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.create_file t ~executable:true ~dir:(p "")
        "a" (v "from-master")
      >>*= fun () ->
      DK.Transaction.stat t (p "a") |> check_kind "Should be executable" `Exec
      >>= fun () ->
      DK.Transaction.create_symlink t ~dir:(p "") "b" "from-master"
      >>*= fun () ->
      DK.Transaction.create_file t ~executable:true ~dir:(p "")
        "c" (v "from-master")
      >>*= fun () ->
      (* Base: exec, link, exec *)
      DK.Transaction.commit t ~message:"init"
    ) >>*= fun () ->
  (* Fork and make some changes on pr branch *)
  DK.branch dk "pr" >>*= fun pr ->
  expect_head master >>*= fun merge_a ->
  DK.Branch.with_transaction pr (fun t ->
      DK.Transaction.merge t merge_a >>*= fun _ ->
      DK.Transaction.stat t (p "a") |> check_kind "Should be executable" `Exec
      >>= fun () ->
      DK.Transaction.set_executable t (p "b") true >>*= fun () ->
      DK.Transaction.remove t (p "c") >>*= fun () ->
      DK.Transaction.create_symlink t ~dir:(p "") "c" "foo" >>*= fun () ->
      DK.Transaction.commit t ~message:"mod"
    ) >>*= fun () ->
  expect_head pr >>*= fun merge_b ->
  (* Merge pr into master *)
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.set_executable t (p "c") false >>*= fun () ->
      (* Begin the merge *)
      DK.Transaction.merge t merge_b
      >>*= fun ({DK.Transaction.ours; theirs; base = _}, _) ->
      (* Ours: exec, link, normal *)
      DK.Tree.stat ours (p "a") |> check_kind "ours/a" `Exec >>= fun () ->
      DK.Tree.stat ours (p "b") |> check_kind "ours/b" `Link >>= fun () ->
      (* Theirs: exec, exec, link *)
      DK.Tree.stat theirs (p "b") |> check_kind "theirs/b" `Exec >>= fun () ->
      (* RW: exec, exec, conflict(normal) *)
      DK.Transaction.stat t (p "b") |> check_kind "rw/b" `Exec >>= fun () ->
      DK.Transaction.stat t (p "c") |> check_kind "rw/c" `File >>= fun () ->
      DK.Transaction.read_file t (p "c")
      |> check_file "Conflict" "** Conflict **\nChanged on both branches\n"
      >>= fun () ->
      DK.Transaction.replace_file t ~dir:(p "") "c" (v "Resolved") >>*= fun () ->
      DK.Transaction.set_executable t (p "c") true >>*= fun () ->
      DK.Transaction.commit t ~message:"merge"
    ) >>*= fun () ->
  expect_head master >>*= fun head ->
  DK.Tree.stat (DK.Commit.tree head) (p "c") |> check_kind "ro/c" `Exec

(* Irmin.Git.Commit: a commit with an empty filesystem... this is not
   supported by Git! *)
let test_merge_empty dk =
  (* Put "from-master" on master branch *)
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun tr ->
      DK.Transaction.create_file tr ~dir:(p "") "file" (v "from-master")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"init"
    ) >>*= fun () ->
  expect_head master >>*= fun head ->
  (* Create a new branch, and merge from this *)
  DK.branch dk "new" >>*= fun new_br ->
  DK.Branch.with_transaction new_br (fun tr ->
      DK.Transaction.merge tr head >>*= fun _ ->
      DK.Transaction.commit tr ~message:"init"
    ) >>*= fun () ->
  expect_head master >>*= fun head ->
  DK.Tree.read_dir (DK.Commit.tree head) (p "") >>*= fun files ->
  Alcotest.(check (slist string String.compare)) "Final result" ["file"] files;
  Lwt.return ()

let test_conflicts dk =
  try_merge_client dk
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
    (fun t {DK.Transaction.theirs; _} ->
       DK.Transaction.commit t ~message:"commit" >>= function
       | Ok () -> Alcotest.fail "Commit should have failed due to conflicts"
       | Error (`Msg _x) ->
         (* Alcotest.(check string) "Conflict error"
            "conflicts file is not empty" x; *)
         DK.Transaction.read_dir t (p "") >>*= fun items ->
         Alcotest.(check (slist string String.compare)) "Resolved files"
           ["a"; "b"; "c"; "dir"; "dir2"; "f"; "g"; "h"] items;
         DK.Transaction.read_file t (p "a") |> check_file "a"
           "** Conflict **\nChanged on both branches\n"
         >>= fun () ->
         DK.Transaction.read_file t (p "b") |> check_file "b" "b-theirs"
         >>= fun () ->
         DK.Transaction.read_file t (p "c") |> check_file "c" "c-ours"
         >>= fun () ->
         DK.Transaction.read_file t (p "f")
         |> check_file "f" "** Conflict **\noption: add/del\n"
         >>= fun () ->
         DK.Transaction.read_file t (p "g") |> check_file "g" "g-same"
         >>= fun () ->
         DK.Transaction.read_file t (p "h") |> check_file "f"
           "** Conflict **\ndefault: add/add and no common ancestor\n"
         >>= fun () ->
         DK.Transaction.read_file t (p "dir")
         |> check_file "dir" "** Conflict **\nFile vs dir\n"
         >>= fun () ->
         DK.Transaction.conflicts t >>*= fun conflicts ->
         let conflicts = List.map Datakit_path.to_hum conflicts in
         Alcotest.(check (list string)) "conflicts"
           ["a"; "dir"; "dir2"; "f"; "h"] conflicts;
         DK.Transaction.remove t (p "a") >>*= fun () ->
         DK.Transaction.remove t (p "dir") >>*= fun () ->
         DK.Transaction.remove t (p "dir2") >>*= fun () ->
         DK.Tree.read_file theirs (p "f") >>*=
         DK.Transaction.replace_file t ~dir:(p "") "f" >>*= fun () ->
         DK.Transaction.conflicts t >>*= fun conflicts ->
         let conflicts = List.map Datakit_path.to_hum conflicts in
         Alcotest.(check (list string)) "conflicts" ["h"] conflicts;
         DK.Transaction.remove t (p "h")
    ) >>= fun () ->
  DK.branch dk "master" >>*= fun master ->
  expect_head master >>*= fun head ->
  DK.Tree.read_dir (DK.Commit.tree head) (p "") >>*= fun files ->
  Alcotest.(check (slist string String.compare)) "Final merge result"
    ["b"; "c"; "f"; "g"] files;
  DK.Tree.read_file (DK.Commit.tree head) (p "f") |> check_file "f" "f-theirs"

let test_make_dirs dk =
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun tr ->
      DK.Transaction.make_dirs tr (p "") >>*= fun () ->
      DK.Transaction.make_dirs tr (p "foo/bar/baz") >>*= fun () ->
      DK.Transaction.stat tr (p "foo/bar/baz") |> check_kind "Dir exists" `Dir
      >>= fun () ->
      DK.Transaction.make_dirs tr (p "foo/bar/baz/new") >>*= fun () ->
      DK.Transaction.stat tr (p "foo/bar/baz/new") |> check_kind "Dir exists" `Dir
      >>= fun () ->
      DK.Transaction.make_dirs tr (p "foo/bar/baz") >>*= fun () ->
      DK.Transaction.commit tr ~message:"mkdirs"
    ) >>*= Lwt.return

let watch_thread ~switch branch path =
  let events, push = Lwt_stream.create () in
  let th =
    DK.Branch.wait_for_path branch ~switch path (fun node ->
        Logs.warn (fun f -> f "Update: %a" Datakit_path.pp path);
        push (Some node);
        Lwt.return (Ok `Again)
      ) >>*= Lwt.return in
  Lwt.on_failure th (fun ex ->
      Logs.err (fun f -> f "Watch thread failed: %s" (Printexc.to_string ex))
    );
  (events, th)

let with_events branch path fn =
  let switch = Lwt_switch.create () in
  Lwt.finalize
    (fun () ->
       let events, th = watch_thread branch ~switch path in
       fn events >>= fun () ->
       Lwt.return (Ok th)
    )
    (fun () -> Lwt_switch.turn_off switch)
  >>*= fun th ->
  th >>= function
  | `Finish `Impossible -> assert false
  | `Abort -> Lwt.return ()

let create_in_trans branch dir leaf contents =
  DK.Branch.with_transaction branch (fun tr ->
      DK.Transaction.make_dirs tr dir >>= fun _ ->
      DK.Transaction.create_file tr ~dir leaf contents >>*= fun () ->
      DK.Transaction.commit tr ~message:"Add file"
    )

let expect_dir_event msg events expected =
  Lwt_stream.next events >>= function
  | None | Some (`Link _|`Exec _|`File _) -> Alcotest.fail "Expected dir"
  | Some (`Dir tree) ->
  DK.Tree.read_dir tree (p "") >>*= fun items ->
  Alcotest.(check (slist string String.compare)) msg expected items;
  Lwt.return ()

let expect_file_event msg events expected =
  Lwt_stream.next events >>= fun event ->
  Alcotest.(check (option file_event)) msg expected event;
  Lwt.return ()

let test_watch dk =
  DK.branch dk "master" >>*= fun master ->
  (* Start monitoring / *)
  with_events master (p "") @@ fun root_events ->
  Lwt_stream.next root_events >>= begin function
    | Some (`Dir tree) ->
      (* XXX: should this be None? compare with other tests. *)
      DK.Tree.read_dir tree (p "") >>*= fun init ->
      Alcotest.(check (list string)) "Root tree should initially be empty"
        [] init;
      Lwt.return ()
    | _ ->
      Alcotest.fail "Root tree should initially be Dir"
  end >>= fun () ->
  (* Start monitoring /doc (doesn't exist yet) *)
  with_events master (p "doc") @@ fun doc_events ->
  Lwt_stream.next doc_events >>= function
  | Some _ -> Alcotest.fail "Docs tree should initially be None"
  | None ->
  (* Create /src/Makefile *)
  create_in_trans master (p "src") "Makefile" (v "all: build") >>*= fun () ->
  expect_dir_event "Top event" root_events ["src"] >>= fun () ->
  (* Start monitoring /src/Makefile *)
  with_events master (p "src/Makefile") @@ fun makefile_events ->
  expect_file_event "Makefile" makefile_events (Some (`File (v "all: build")))
  >>= fun () ->
  (* Modify file under doc *)
  create_in_trans master (p "doc") "README" (v "Instructions") >>*= fun () ->
  expect_dir_event "Doc event" doc_events ["README"] >>= fun () ->
  expect_dir_event "Top event" root_events ["doc"; "src"] >>= fun () ->
  let next_makefile_event = Lwt_stream.next makefile_events in
  Alcotest.(check bool) "No Makefile update" true
    (Lwt.state next_makefile_event = Lwt.Sleep);
  (* Make executable *)
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.set_executable t (p "src/Makefile") true >>*= fun () ->
      DK.Transaction.commit t ~message:"exec"
    )
  >>*= fun () ->
  next_makefile_event >>= fun event ->
  Alcotest.(check (option file_event)) "Makefile"
    (Some (`Exec (v "all: build"))) event;
  (* Make symlink *)
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.remove t (p "src/Makefile") >>*= fun () ->
      DK.Transaction.create_symlink t ~dir:(p "src") "Makefile" "my-target"
      >>*= fun () ->
      DK.Transaction.commit t ~message:"symlink"
    )
  >>*= fun () ->
  expect_file_event "Makefile" makefile_events (Some (`Link "my-target"))
  >>= fun () ->
  (* Remove *)
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.remove t (p "src/Makefile") >>*= fun () ->
      DK.Transaction.commit t ~message:"symlink"
    )
  >>*= fun () ->
  expect_file_event "Makefile" makefile_events None >>= fun () ->
  Lwt.return_unit

let test_rename_branch dk =
  DK.branch dk "old" >>*= fun branch ->
  create_in_trans branch (p "") "key" (v "value") >>*= fun () ->
  DK.Branch.rename branch "new" >>*= fun () ->
  DK.branches dk >>*= fun branches ->
  Alcotest.(check (list string)) "New branches" ["new"] branches;
  DK.branch dk "new" >>*= fun new_branch ->
  expect_head new_branch >>*= fun head ->
  DK.Tree.read_file (DK.Commit.tree head) (p "key")
  |> check_file "Moved" "value" >>= fun () ->
  DK.Branch.remove branch >>*= fun () ->
  DK.branches dk >>*= fun branches ->
  Alcotest.(check (list string)) "New gone" [] branches;
  Lwt.return_unit

let test_rename_dir dk =
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun t ->
    DK.Transaction.create_dir t ~dir:(p "") "old" >>*= fun () ->
    DK.Transaction.rename t (p "old") "new" >>*= fun () ->
    DK.Transaction.read_dir t (p "") >>*= fun items ->
    Alcotest.(check (list string)) "New files" ["new"] items;
    DK.Transaction.abort t >>= fun () ->
    Lwt.return (Ok ())
  ) >>*= fun () ->
  Lwt.return ()

let test_rename_file dk =
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun t ->
    DK.Transaction.create_file t ~dir:(p "") "old" (v "data") >>*= fun () ->
    DK.Transaction.rename t (p "old") "new" >>*= fun () ->
    DK.Transaction.read_dir t (p "") >>*= fun items ->
    Alcotest.(check (list string)) "New files" ["new"] items;
    (* Check rename detects errors
       Note: we currently allow overwriting an empty directory *)
    DK.Transaction.create_dir t ~dir:(p "") "dir" >>*= fun () ->
    DK.Transaction.create_file t ~dir:(p "dir") "precious" (v "data")
    >>*= fun () ->
    DK.Transaction.rename t (p "new") "dir" >>= function
    | Ok () -> Alcotest.fail "Shouldn't be able to rename over a directory"
    | Error (`Msg e) ->
    Alcotest.(check string) "Rename over dir" "Is a directory" e;
    (* Rename when source has been deleted. Ideally, we should also
       check it hasn't been replaced by something with the same
       name. *)
    DK.Transaction.remove t (p "new") >>*= fun () ->
    DK.Transaction.rename t (p "new") "reborn" >>= function
    | Ok () -> Alcotest.fail "Shouldn't be able to rename a missing source"
    | Error (`Msg e) ->
    Alcotest.(check string) "Source deleted" "No such file or directory" e;
    DK.Transaction.commit t ~message:"rename"
  ) >>*= fun () ->
  expect_head master >>*= fun head ->
  DK.Tree.read_dir (DK.Commit.tree head) (p "") >>*= fun items ->
  Alcotest.(check (list string)) "New files" ["dir"] items;
  Lwt.return_unit

let test_truncate dk =
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun t ->
      let check msg expected =
        DK.Transaction.read_file t (p "file") |> check_file msg expected
      in
      DK.Transaction.create_file t ~executable:true ~dir:(p "")
        "file" (v "Hello")
      >>*= fun () ->
      DK.Transaction.truncate t (p "file") 4L >>*= fun () ->
      check "Truncate to 4" "Hell" >>= fun () ->
      DK.Transaction.truncate t (p "file") 4L >>*= fun () ->
      check "Truncate to 4 again" "Hell" >>= fun () ->
      DK.Transaction.truncate t (p "file") 6L >>*= fun () ->
      check "Extend to 6" "Hell\x00\x00" >>= fun () ->
      DK.Transaction.truncate t (p "file") 0L >>*= fun () ->
      check "Truncate to 0" "" >>= fun () ->
      DK.Transaction.commit t ~message:"truncate"
    )
  >>*= Lwt.return

(* FIXME: automaticall run ./scripts/git-dumb-server *)
let test_remotes dk =
  DK.fetch dk ~url:"git://localhost/" ~branch:"origin" >>*= fun fetch_head ->
  DK.Tree.read_dir (DK.Commit.tree fetch_head) (p "") >>*= fun items ->
  Alcotest.(check (list string)) "Remotes entries" ["foo"; "x"] items;
  Lwt.return_unit

let test_remove dk =
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.make_dirs t (p "foo/bar/baz") >>*= fun () ->
      DK.Transaction.create_file t ~dir:(p "foo") "file1" (v "1") >>*= fun () ->
      DK.Transaction.create_file t ~dir:(p "foo/bar") "file2" (v "2")
      >>*= fun () ->

      (* XXX: hack to make the test pass until we fix
         https://github.com/docker/datakit/issues/147 *)
      DK.Transaction.read_dir t (p "foo") >>*= fun _ ->

      DK.Transaction.remove t (p "foo/bar") >>*= fun () ->
      DK.Transaction.read_dir t (p "foo") >>*= fun items ->
      Alcotest.(check (list string)) "Remaining entries" ["file1"] items;
      DK.Transaction.abort t >|= fun () -> Ok ()
    )
  >>*= fun () ->
  DK.Branch.head master >>*= fun head ->
  Alcotest.(check (option reject)) "Aborted" None head;
  Lwt.return ()

let test_large_write dk =
  let data = Cstruct.create (1024 * 1024) in
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.create_file t ~dir:(p "") "test" data >>*= fun () ->
      DK.Transaction.commit t ~message:"big-write"
    )
  >>*= fun () ->
  expect_head master >>*= fun head ->
  DK.Tree.read_file (DK.Commit.tree head) (p "test") >>*= fun actual ->
  Alcotest.(check int) "Wrote large file" (Cstruct.len data) (Cstruct.len actual);
  Lwt.return ()

let test_create_or_replace dk =
  DK.branch dk "master" >>*= fun master ->
  DK.Branch.with_transaction master (fun t ->
      DK.Transaction.exists t (p "README") >>*= fun exists ->
      Alcotest.(check bool) "Doesn't yet exist" false exists;
      DK.Transaction.create_or_replace_file t ~dir:(p "") "README" (v "Data")
      >>*= fun () ->
      DK.Transaction.exists t (p "README") >>*= fun exists ->
      Alcotest.(check bool) "Now exists" true exists;
      DK.Transaction.create_or_replace_file t ~dir:(p "") "README" (v "Data2")
      >>*= fun () ->
      DK.Transaction.commit t ~message:"create-or-replace"
    )
  >>*= fun () ->
  expect_head master >>*= fun head ->
  DK.Tree.read_file (DK.Commit.tree head) (p "README") >>*= fun actual ->
  Alcotest.(check string) "Updated" "Data2" (Cstruct.to_string actual);
  Lwt.return ()

let run f () =
  Test_utils.run (fun _repo conn ->
      f (DK.connect conn)
    )

let test_set = [
  "Transaction", `Quick, run test_transaction;
  "Watch"      , `Quick, run test_watch;
  "Make dirs"  , `Quick, run test_make_dirs;
  "Rename branch", `Quick, run test_rename_branch;
  "Rename dir" , `Quick, run test_rename_dir;
  "Rename file", `Quick, run test_rename_file;
  "Truncate"   , `Quick, run test_truncate;
  "Parents"    , `Quick, run test_parents;
  "Merge"      , `Quick, run test_merge;
  "Merge_metadata", `Quick, run test_merge_metadata;
  "Merge empty", `Quick, run test_merge_empty;
  "Conflicts"  , `Quick, run test_conflicts;
  "Large write", `Quick , run test_large_write;
  "Remove"     , `Quick , run test_remove;
  "Remotes"    , `Slow , run test_remotes;
  "Create_or_replace", `Slow , run test_create_or_replace;
]
