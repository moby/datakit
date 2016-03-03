(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* FIXME: add in alcotest *)
let bool_t: bool Alcotest.testable =
  (module struct type t = bool let equal = (=) let pp = Fmt.bool end)

let set (type a) (a: a Alcotest.testable) compare: a list Alcotest.testable =
  let module A = (val a) in
  let module L = (val Alcotest.list a) in
  (module struct
    type t = A.t list
    let equal x y = L.equal (List.sort compare x) (List.sort compare y)
    let pp = L.pp
  end)

let of_pp (type a) pp: a Alcotest.testable =
  (module struct type t = a let equal = (=) let pp = pp end)

let pair (type a) (type b)
    (a:a Alcotest.testable) (b:b Alcotest.testable): (a * b) Alcotest.testable =
  let module A = (val a) in
  let module B = (val b) in
  (module struct
    type t = a * b
    let equal (a1, b1) (a2, b2) = A.equal a1 a2 && B.equal b1 b2
    let pp = Fmt.pair A.pp B.pp
  end)

let source_t: Source.t Alcotest.testable = (module Source)
let task_t: Task.t Alcotest.testable = (module Task)
let worker_t: Worker.t Alcotest.testable = (module Worker)
let job_t: Job.t Alcotest.testable = (module Job)
let jobs_t = set job_t Job.compare
let tasks_t = set task_t Task.compare
let workers_t = set worker_t Worker.compare
let task_status_t = of_pp Task.pp_status
let job_status_t = of_pp Job.pp_status
let worker_status_t = of_pp Worker.pp_status

let random_cstruct n =
  let t  = Unix.gettimeofday () in
  let cs = Cstruct.create 8 in
  Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ;
  Nocrypto.Rng.reseed cs;
  Nocrypto.Rng.generate n

let random_ascii_string n =
  let s = Bytes.create n in
  for i = 0 to n-1 do
    Bytes.set s i (Char.chr (Random.int 128))
  done;
  s

let to_str codec v =
  let b = Buffer.create 64 in
  let e = Jsonm.encoder (`Buffer b) in
  let e = Jsont.encoder e codec v in
  match Jsont.encode e with
  | `Ok      -> Buffer.contents b
  | `Partial -> assert false

let of_str codec s =
  let e = Jsonm.decoder (`String s) in
  let e = Jsont.decoder e codec in
  match Jsont.decode e with
  | `Ok (_, v)    -> v
  | `Await        -> assert false
  | `Error (_, e) ->
    invalid_arg (Jsont.error_to_string e)

let json codec v =
  let s = to_str codec v in
  Fmt.(pf stdout) "%s\n" s;
  of_str codec s

let r1 = "example", Uri.of_string "http://example.com"
let r2 = "example2", Uri.of_string "http://example.com/2"

let url1 = Url.parse "http://github.com/samoht/foo.git"
let url2 = Url.parse "http://github.com/samoht/foo.git#master"

let s1 = Source.create "foo" ~url:url1
let s2 = Source.create "foo" ~url:url2

let c1 = Cmd.create "bar" ~exec:"echo bar"
let c2 = Cmd.create "toto" ~exec:"foo/bar hello!"

let f1 = Flow.create ~inputs:[(s1, c1)] ~deps:[(c1, c2)]
let f2 = Flow.create ~inputs:[(s2, c2)] ~deps:[(c1, c2)]

let t1 = Task.create f1
let t2 = Task.create f2

let job_workers  = [Worker.create `Job ; Worker.create `Job]
let task_workers = [Worker.create `Task; Worker.create `Task]
let workers = job_workers @ task_workers

let wj1 = List.hd job_workers
let wt1 = List.hd task_workers

let j1 = Job.create c1
let j2 = Job.create ~inputs:[Job.id j1] c2

let jobs = [j1; j2]
let job_roots = List.filter (fun j -> Job.inputs j = []) jobs

let job_root = j1

let jr1 = wj1
let jnr1 = wt1

let store () =
  let _ = Sys.command "rm -rf /tmp/datakit-tests" in
  Store.local ~root:"/tmp/datkit-tests" ()

let ts = 0.01

let retry f =
  let open Lwt.Infix in
  let c = ref ts in
  let rec aux n =
    if n <= 1 then f ()
    else
      Lwt.catch f
        (fun e ->
           Fmt.(pf stderr) "RETRY: got %s (%.2f)\n" (Printexc.to_string e) !c;
           c := 2. *. !c;
           Lwt_unix.sleep !c >>= fun () ->
           aux (n-1))
  in
  aux 10

let run f =
  let err e =
    Fmt.(pf stdout "%!");
    Fmt.(pf stderr "%!");
    flush stdout;
    flush stderr;
    raise e
  in
  Lwt.async_exception_hook := err;
  let protect f () = try f () with e -> Lwt.fail e in
  Lwt_main.run (Lwt.catch (protect f) err)

let () =
  Irmin_unix.install_dir_polling_listener ts;
  Fmt.(set_style_renderer stdout `Ansi_tty)
