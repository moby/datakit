open Lwt.Infix
open Test_utils
open Result

let ( >>!= ) x f =
  match x with
  | Ok y -> f y
  | Error vfs_error ->
    Alcotest.fail ("Vfs.error: " ^ Fmt.to_to_string Vfs.Error.pp vfs_error)
let vfs_error = Alcotest.of_pp Vfs.Error.pp
let vfs_result ok = Alcotest.result ok vfs_error

module Store = Datakit.Make_git(Maker)
module RW = Datakit.Dir(Store)

let p l = Datakit.Path.v l

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

let test_rw () =
  let v x = Datakit.Blob.string x, `Normal in
  let err = (module RW_err : Alcotest.TESTABLE with type t = RW_err.t) in
  let err1 = (module RW_err1 : Alcotest.TESTABLE with type t = RW_err1.t) in
  Lwt_main.run begin
    Store.Repo.v config >>= fun repo ->
    let rw = RW.v repo Store.Tree.empty in

    RW.update rw (p []) "foo" (v "a")
    >|= Alcotest.(check (result unit err)) "Write /a" (Ok ()) >>= fun () ->

    RW.update rw (p ["sub"; "bar"]) "baz" (v "b")
    >|= Alcotest.(check (result unit err)) "Write /sub/bar/baz" (Ok ())
    >>= fun () ->

    (* /foo is a file *)
    RW.update rw (p ["foo"; "bar"]) "baz" (v "b")
    >|= Alcotest.(check (result unit err)) "Write /foo/bar/baz"
      (Error `Not_a_directory)
    >>= fun () ->

    RW.remove rw (p ["foo"]) "bar"
    >|= Alcotest.(check (result unit err1)) "rm /foo/bar"
      (Error `Not_a_directory)
    >>= fun () ->

    RW.update_force rw (p ["foo"; "bar"]) "baz" (v "b") >>= fun () ->

    RW.update rw (p ["foo"]) "bar" (v "b")
    >|= Alcotest.(check (result unit err)) "Write /foo/bar"
      (Error `Is_a_directory)
    >>= fun () ->

    RW.remove rw (p ["foo"; "bar"]) "baz"
    >|= Alcotest.(check (result unit err1)) "rm /foo/bar/baz" (Ok ())
    >>= fun () ->

    let root = RW.root rw in

    Store.Tree.list root Store.Key.empty
    >|= List.map fst
    >|= Alcotest.(check (slist string String.compare)) "ls /" ["foo"; "sub"]
    >>= fun () ->

    Lwt.return ()
  end

let test_blobs_fast_path () =
  let correct = ref (Cstruct.create 0) in
  let blob = ref Datakit.Blob.empty in
  for _ = 1 to 100 do
    let data = Cstruct.create (Random.int 10) in
    for j = 0 to Cstruct.len data - 1 do
      Cstruct.set_uint8 data j (Random.int 26 + 65)
    done;
    correct := Cstruct.append !correct data;
    Datakit.Blob.write !blob ~offset:(Datakit.Blob.len !blob) data >>!= fun b ->
    blob := b
  done;
  let correct = Cstruct.to_string !correct in
  let actual = Datakit.Blob.to_string !blob in
  Alcotest.(check string) "Fast-append worked" correct actual

let test_blobs_random () =
  let int64 = Alcotest.of_pp Fmt.int64 in
  let str b = Datakit.Blob.to_string b in
  let read_ok b ~offset ~count =
    Datakit.Blob.read b ~offset ~count >>!= Cstruct.to_string
  in
  let write_ok b ~offset data =
    Datakit.Blob.write b ~offset (Cstruct.of_string data) >>!= fun x -> x
  in
  let truncate_ok b len = Datakit.Blob.truncate b len >>!= fun x -> x in
  (* Empty *)
  let b = Datakit.Blob.empty in
  Alcotest.check int64 "Empty" 0L (Datakit.Blob.len b);
  (* Negative offset write *)
  let bad_write = Datakit.Blob.write b ~offset:(-2L) (Cstruct.of_string "bad") in
  Alcotest.check (vfs_result reject) "Negative offset"
    (Vfs.Error.negative_offset (-2L)) bad_write;
  (* Write *)
  let b = write_ok b ~offset:2L "1st" in
  Alcotest.check int64 "1st write" 5L (Datakit.Blob.len b);
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
    (Vfs.Error.negative_offset (-1L)) (Datakit.Blob.truncate b (-1L));
  (* Read *)
  Alcotest.(check string) "Read neg" "" (read_ok b ~offset:2L ~count:(-3));
  Alcotest.(check string) "Read zero" "" (read_ok b ~offset:2L ~count:0);
  Alcotest.(check string) "Read short" "1s" (read_ok b ~offset:2L ~count:2);
  Alcotest.(check string) "Read full" "1st" (read_ok b ~offset:2L ~count:3);
  Alcotest.(check string) "Read long" "1st" (read_ok b ~offset:2L ~count:4);
  Alcotest.(check string) "Read EOF" "" (read_ok b ~offset:5L ~count:4);
  Alcotest.check (vfs_result reject) "Read negative"
    (Vfs.Error.negative_offset (-1L)) (Datakit.Blob.read b ~offset:(-1L) ~count:1);
  Alcotest.check (vfs_result reject) "Read after EOF"
    (Vfs.Error.offset_too_large ~offset:6L 5L) (Datakit.Blob.read b ~offset:6L ~count:1);
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

let test_set = [
  "Writes"      , `Quick, test_writes;
  "RW"          , `Quick, test_rw;
  "Blobs fast"  , `Quick, test_blobs_fast_path;
  "Blobs random", `Quick, test_blobs_random;
  "Streams"     , `Quick, test_streams;
]

let endpoint = Alcotest.testable Datakit_conduit.pp (=)

type expt = Same | Expect of string

let test_conduit () =
  let check (te, got, expect) =
    let expect = match expect with Same -> got | Expect s -> s in
    match Datakit_conduit.parse ~default_tcp_port:123 got with
    | `Error e -> Alcotest.fail e
    | `Ok t    ->
      Alcotest.(check endpoint) "parse" te t;
      let got = Fmt.to_to_string Datakit_conduit.pp t in
      Alcotest.(check string) "pp" expect got
  in
  let uri s = Uri.of_string @@ Fmt.strf "hyperv-%s://test" s in
  List.iter check [
    `Tcp ("localhost", 1234)      , "tcp://localhost:1234"  ,Same;
    `Tcp ("localhost", 123)       , "tcp://localhost",
    Expect "tcp://localhost:123";
    `File ("foo/bar")             , "file://foo/bar"       , Same;
    `NamedPipe "\\\\file\\on\\win", "\\\\file\\on\\win"    , Same;
    `Fd 42                        , "fd://42"              , Same;
    `HyperV_connect(uri "connect"), "hyperv-connect://test", Same;
    `HyperV_accept (uri "accept") , "hyperv-accept://test" , Same;
  ]

let () =
  Alcotest.run "datakit" [
    "server" , test_set;
    "conduit", [ "basic", `Quick, test_conduit ];
  ]
