open Lwt.Infix

module API = struct

  open Vgithub

  type state = {
    user  : string;
    repo  : string;
    mutable status: (string * Status.t list) list;
    mutable prs   : PR.t list;
    mutable events: Event.t list;
    mutable count_events: int;
  }

  type token = state

  let user_exists t ~user = user = t.user
  let repo_exists t ~user ~repo = user = t.user && repo = t.repo
  let repos t ~user = if not (t.user = user) then [] else [t.repo]

  let status t ~user ~repo ~commit =
    if not (t.user = user && t.repo = repo) then []
    else
      try List.assoc commit t.status
      with Not_found -> []

  let set_status t ~user ~repo s =
    if not (t.user = user && t.repo = repo) then ()
    else
      let commit = s.Status.commit in
      let keep (c, _) = c <> commit in
      let status = List.filter keep t.status in
      let rest =
        try
          List.find (fun x -> not (keep x)) t.status
          |> snd
          |> List.filter (fun y -> y.Status.context <> s.Status.context)
        with Not_found ->
          []
      in
      let status = (commit, s :: rest) :: status in
      t.status <- status

  let prs t ~user ~repo =
    if not (t.user = user && t.repo = repo) then []
    else t.prs

  let events t ~user ~repo =
    t.count_events <- t.count_events + 1;
    if not (t.user = user && t.repo = repo) then Lwt.return_nil
    else Lwt.return t.events

end

open Test_utils
open Vgithub
open Datakit_path.Infix

module VG = Sync(API)(DK)

let s1 = {
  Status.context = Some "foo/bar/baz";
  url            = None;
  description    = Some "foo";
  state          = `Pending;
  commit         = "bar";
}

let s2 = {
  Status.context = Some "foo/bar/toto";
  url            = Some "toto";
  description    = None;
  state          = `Failure;
  commit         = "bar";
}

let s3 = {
  Status.context = Some "foo/bar/baz";
  url            = Some "titi";
  description    = Some "foo";
  state          = `Success;
  commit         = "foo";
}

let s4 = {
  Status.context = Some "foo";
  url            = None;
  description    = None;
  state          = `Pending;
  commit         = "bar";
}

let events0 = [
  Event.PR { PR.number = 1; state = `Open  ; head = "foo" };
  Event.PR { PR.number = 1; state = `Closed; head = "foo" };
  Event.PR { PR.number = 2; state = `Open  ; head = "bar" };
  Event.Status s1;
  Event.Status s2;
  Event.Status s3;
  Event.Status s4;
]

let events1 = [
  Event.PR { PR.number = 1; state = `Open  ; head = "foo" };
  Event.PR { PR.number = 2; state = `Open  ; head = "bar" };
  Event.Status s1;
  Event.Status s2;
  Event.Status s3;
  Event.Status s4;
]

let status_state: Status_state.t Alcotest.testable =
  (module struct include Status_state let equal = (=) end)

let user = "test"
let repo = "test"
let branch = "test"
let writes = "test-writes"

let init events =
  { API.user; repo; status = []; prs = []; events; count_events = 0 }

let run f () =
  Test_utils.run (fun _repo conn ->
      let dk = DK.connect conn in
      DK.branch dk branch >>*= fun branch ->
      DK.Branch.with_transaction branch (fun tr ->
          let dir = Datakit_path.(empty / user / repo) in
          DK.Transaction.make_dirs tr dir >>*= fun () ->
          DK.Transaction.create_or_replace_file tr ~dir
            "README" (Cstruct.of_string "")
          >>*= fun () ->
          DK.Transaction.commit tr ~message:"init"
        )
      >>*= fun () ->
      f dk
    )

let check_dirs = Alcotest.(check (slist string String.compare))
let check_data msg x y = Alcotest.(check string) msg x (Cstruct.to_string y)

let check tree =
  (* check test/test/commit *)
  let commit = Datakit_path.empty / user / repo / "commit" in
  DK.Tree.exists_dir tree commit >>*= fun exists ->
  Alcotest.(check bool) "commit dir exists" exists true;
  DK.Tree.read_dir tree commit >>*= fun dirs ->
  check_dirs "commits" ["bar"] dirs;
  DK.Tree.read_dir tree (commit / "bar"/ "status" ) >>*= fun dirs ->
  check_dirs "status 0" ["foo"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" ) >>*= fun dirs ->
  check_dirs "status 1" ["state";"bar"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar")
  >>*= fun dirs ->
  check_dirs "status 2" ["baz";"toto"] dirs;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar" / "baz")
  >>*= fun dirs ->
  check_dirs "status 3" ["description";"state"] dirs;
  DK.Tree.read_file tree
    (commit / "bar" / "status" / "foo" / "bar" / "baz" / "state")
  >>*= fun data ->
  check_data "status/state" "pending\n" data;
  DK.Tree.read_file tree
    (commit / "bar" / "status" / "foo" / "bar" / "baz" / "description")
  >>*= fun data ->
  check_data "status/description" "foo\n" data;
  DK.Tree.read_dir tree (commit / "bar" / "status" / "foo" / "bar" / "toto")
  >>*= fun dirs ->
  check_dirs "status 3" ["target_url";"state"] dirs;

  (* check test/test/pr *)
  let pr = Datakit_path.empty / user / repo / "pr" in
  DK.Tree.exists_dir tree pr >>*= fun exists ->
  Alcotest.(check bool) "pr dir exists" true exists;
  DK.Tree.read_dir tree pr >>*= fun dirs ->
  check_dirs "pr 1" ["2"] dirs ;
  DK.Tree.read_dir tree (pr / "2") >>*= fun dirs ->
  check_dirs "pr 2" dirs ["state"; "head"];
  DK.Tree.read_file tree (pr / "2" / "state") >>*= fun data ->
  check_data "state" "open\n" data;
  DK.Tree.read_file tree (pr / "2" / "head") >>*= fun data ->
  check_data "head" "bar\n" data ;

  Lwt.return_unit

let test_events dk =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  let t = init events0 in
  let s = VG.empty in
  DK.branch dk branch >>*= fun branch ->
  DK.branch dk writes >>*= fun writes ->
  Alcotest.(check int) "API.event: 0" 0 t.API.count_events;
  VG.sync ~policy:`Once s branch ~writes t >>= fun s ->
  Alcotest.(check int) "API.event: 1" 1 t.API.count_events;
  VG.sync ~policy:`Once s branch ~writes t >>= fun _s ->
  Alcotest.(check int) "API.event: 2" 1 t.API.count_events;
  expect_head branch >>*= fun head ->
  check (DK.Commit.tree head)

let test_updates dk =
  quiet_9p ();
  quiet_git ();
  quiet_irmin ();
  let t = init events1 in
  let s = VG.empty in
  DK.branch dk branch >>*= fun branch ->
  DK.branch dk writes >>*= fun writes ->
  Alcotest.(check int) "API.event: 0" t.API.count_events 0;
  VG.sync ~policy:`Once s branch ~writes t >>= fun s ->
  VG.sync ~policy:`Once s branch ~writes t >>= fun s ->
  Alcotest.(check int) "API.event: 1" t.API.count_events 1;
  expect_head branch >>*= fun head ->
  let dir = Datakit_path.empty / user / repo / "commit" / "foo" in
  DK.Tree.exists_dir (DK.Commit.tree head) dir >>*= fun exists ->
  Alcotest.(check bool) "exist commit/foo" true exists;
  DK.Branch.with_transaction writes (fun tr ->
      DK.Transaction.create_or_replace_file tr
        ~dir:(dir / "status" / "foo" / "bar" / "baz") "state"
        (Cstruct.of_string "pending\n")
      >>*= fun () ->
      DK.Transaction.commit tr ~message:"Test"
    ) >>*= fun () ->
  VG.sync ~policy:`Once s branch ~writes t >>= fun _s ->
  Alcotest.(check int) "API.event: 2" t.API.count_events 1;
  let s =
    try List.find (fun (c, _) -> c = "foo") t.API.status |> snd |> List.hd
    with Not_found -> Alcotest.fail "foo not found"
  in
  Alcotest.(check status_state) "update status" s.Status.state `Pending;
  Lwt.return_unit

let test_set = [
  "events" , `Quick, run test_events;
  "updates", `Quick, run test_updates;
]
