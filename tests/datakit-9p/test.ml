open Lwt.Infix
open Test_utils
open Astring
module Server = Fs9p.Make(Test_flow)

let src9p = Logs.Src.create "test9p" ~doc:"Datakit tests"
module Log9p = (val Logs.src_log src9p)

module Client = Protocol_9p.Client.Make(Log9p)(Test_flow)

module Store = Ivfs_tree.Make(Maker)
module Filesystem = Ivfs.Make(Store)

let p l = Ivfs_tree.Path.v l
let v b = Ivfs_blob.of_string b

let root_entries =
  ["branch"; "debug"; "snapshots"; "trees"; "commits"; "remotes"]

let run fn =
  Lwt_main.run begin
    quiet_9p src9p;
    Store.Repo.v config >>= fun repo ->
    Store.Repo.branches repo >>= fun branches ->
    Lwt_list.iter_s (fun branch ->
        Store.Branch.remove repo branch
      ) branches
    >>= fun () ->
    let for_client, for_server = Test_flow.create () in
    let info msg =
      let date = 0L in
      Irmin.Info.v ~date ~author:"irmin9p" msg
    in
    let root = Filesystem.create ~info repo in
    let server_thread =
      Server.accept ~root ~msg:"test" for_server >>*= Lwt.return
    in
    Lwt.finalize
      (fun () -> Client.connect for_client ~max_fids:Int32.max_int () >>*= fn repo)
      (fun () -> Lwt.cancel server_thread; Lwt.return ())
  end

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

let rwx = [`Read; `Write; `Execute]
let rw = [`Read; `Write]
let rx = [`Read; `Execute]
let r = [`Read]
let rwxr_xr_x = Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx ()
let rw_r__r__ = Protocol_9p.Types.FileMode.make ~owner:rw ~group:r ~other:r ()
let symlink = Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx
    ~is_symlink:true ()

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
  let ( >>== ) = Protocol_9p.Infix.( >>*= ) in
  with_file conn path (fun fid ->
      begin
        if not truncate then Lwt.return_unit
        else Client.LowLevel.update ~length:0L conn fid >>*= Lwt.return
      end >>= fun () ->
      Client.LowLevel.openfid conn fid Protocol_9p.Types.OpenMode.write_only
      >>== fun _open ->
      Client.LowLevel.write conn fid 0L (Cstruct.of_string contents)
      >>== fun _resp ->
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

(* Get the history starting from commit [hash] *)
let rec history conn id =
  read_file conn ["snapshots"; id; "parents"] >>= fun parents ->
  read_file conn ["snapshots"; id; "msg"] >>= fun msg ->
  let parents = Str.(split (regexp "\n")) parents in
  Lwt_list.map_s (history conn) parents >|= fun parents ->
  let parents = List.sort compare_history_node parents in
  { id; msg = String.trim msg; parents }

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
          let name = stat.Protocol_9p.Types.Stat.name in
          Client.remove conn (t @ ["rw"; name]) >>*= Lwt.return
        ) >>= fun () ->
      Client.readdir conn (t @ ["rw"]) >>*= fun existing ->
      existing
      |> List.map (fun e -> e.Protocol_9p.Types.Stat.name)
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
          let dir, name = split path in
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

  Store.master repo >>= fun master ->
  Store.Head.get master >>= fun commit ->
  let info = Store.Commit.info commit in
  let msg = Irmin.Info.message info in
  Alcotest.(check string) "Message" "My commit" msg;

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
           |> List.map (fun i -> i.Protocol_9p.Types.Stat.name)
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

let info = Irmin.Info.none

let test_watch repo conn =
  Store.master repo >>= fun master ->
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
  Store.set master ~info (p ["src"; "Makefile"]) (v "all: build") >>= fun () ->
  Store.get master (p ["src"; "Makefile"]) >>= fun makefile_init ->
  Alcotest.(check string)
    "Makefile contents" "all: build" (Ivfs_blob.to_string makefile_init);
  with_stream conn
    ["branch"; "master"; "watch"; "src.node"; "Makefile.node"; "tree.live"]
  @@ fun makefile ->
  read_line_exn makefile >>= fun makefile_init ->
  Alcotest.(check string) "Makefile hash"
    "F-d81e367f87ee314bcd3e449b1c6641efda5bc269" makefile_init;
  (* Modify file under doc *)
  let next_make = makefile () in
  Store.set ~info master (p ["doc"; "README"]) (v "Instructions") >>= fun () ->
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
  Store.of_branch repo "old" >>= fun old ->
  Store.set ~info old (p ["key"]) (v "value") >>= fun () ->
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
  Store.Branch.mem repo "old" >>= fun old_exists ->
  Store.Branch.mem repo "new" >>= fun new_exists ->
  Alcotest.(check bool) "Old gone" false old_exists;
  Alcotest.(check bool) "New appeared" true new_exists;
  Client.remove conn ["branch"; "new"] >>*= fun () ->
  Store.Branch.mem repo "new" >>= fun new_exists ->
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

let test_debug _repo conn =
  let src = Logs.Src.create "test.debug" ~doc:"Debug test log" in
  Logs.Src.set_level src None;
  let read path msg ~expect =
    Client.read conn path 0L 100l >>*= fun data ->
    let got = Cstruct.concat data |> Cstruct.to_string in
    Alcotest.(check string) msg expect got;
    Lwt.return ()
  in
  let write path value =
    Client.with_fid conn (fun fid ->
        Client.walk_from_root conn fid path >>*= fun _ ->
        Client.LowLevel.update conn ~length:0L fid
      )
    >>*= fun () ->
    Client.write conn path 0L (Cstruct.of_string value)
  in
  let test_level = ["debug"; "src"; "test.debug"; "level"] in
  read test_level "Initially quiet" ~expect:"quiet\n" >>= fun () ->

  write test_level "debug\n" >>*= fun () ->
  read test_level "Now debug" ~expect:"debug\n" >>= fun () ->

  write test_level "info" >>*= fun () ->
  read test_level "Now info" ~expect:"info\n" >>= fun () ->

  write test_level "quiet" >>*= fun () ->
  read test_level "None again" ~expect:"quiet\n"

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

let run f () = run f

let test_set = [
  "Transaction"   , `Quick, run test_transaction;
  "Qids"          , `Quick, run test_qids;
  "Watch"         , `Quick, run test_watch;
  "Range"         , `Quick, run test_bad_range;
  "Rename branch" , `Quick, run test_rename_branch;
  "Rename file"   , `Quick, run test_rename_file;
  "Truncate"      , `Quick, run test_truncate;
  "Parents"       , `Quick, run test_parents;
  "Merge"         , `Quick, run test_merge;
  "Merge_metadata", `Quick, run test_merge_metadata;
  "Merge empty"   , `Quick, run test_merge_empty;
  "Conflicts"     , `Quick, run test_conflicts;
  "Stable inodes" , `Quick, run test_stable_inodes;
  "Remotes"       , `Slow , run test_remotes;
  "Debug"         , `Quick, run test_debug;
]

module C = Test_client.Make(struct
    include Datakit_client_9p.Make(Client)
    let run f = run (fun _repo t -> f (connect t)) ()
  end)

let () =
  Alcotest.run "datakit-9p" [
    "server", test_set;
    "client", C.test_set;
  ]
