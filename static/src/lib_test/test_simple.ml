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

include Test_common

let simple_package () =
  List.iter (fun p ->
      let name = Package.to_string p in
      Alcotest.(check package_t) name p1 (json Package.json p1)
    ) [p1; p2]

let simple_task () =
  List.iter (fun t ->
      let id = Id.to_string (Task.id t) in
      Alcotest.(check task_t) id t1 (json Task.json t1)
    ) [t1; t2]

let simple_host () =
  List.iter (fun h ->
      let name = Fmt.to_to_string Host.pp h in
      Alcotest.(check host_t) name h (json Host.json h)
    ) hosts

let simple_switch () =
  List.iter (fun c ->
      let name = Fmt.to_to_string Switch.pp c in
      Alcotest.(check switch_t) name c (json Switch.json c)
    ) Switch.defaults

let simple_worker () =
  List.iter (fun w ->
      let name = Id.to_string (Worker.id w) in
      Fmt.pf Fmt.stdout "%a\n%!" Worker.pp w;
      Alcotest.(check worker_t) name w (json Worker.json w)
    ) workers


let simple_job () =
  List.iter (fun j ->
      let name = Id.to_string (Job.id j) in
      Alcotest.(check job_t) name j (json Job.json j)
    ) jobs

let obj () =
  let file name contents = name, Digest.string contents in
  let files = [
    file "foo.ml"  "let x = 3";
    file "foo.cmo" (random_ascii_string 1024)
  ] in
  Object.(archive files (random_cstruct 2049))

let lines n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (random_ascii_string 80 :: acc) (n-1)
  in
  let str = String.concat "\n" (aux [] n) in
  Cstruct.of_string str

let objects = [
  obj ();
  obj ();
  obj ();
  Object.file "foo" (lines 10);
  Object.file "bar" (lines 100);
]

let simple_object () =
  List.iter (fun o ->
      let id = Id.to_string (Object.id o) in
      Alcotest.(check object_t) id o (json Object.json o)
    ) objects

let simple_status pp js l () =
  List.iter (fun x ->
      let str = Fmt.to_to_string pp x in
      let y = json js x in
      Alcotest.(check @@ of_pp pp) str x y
    ) l

let simple_task_status =
  simple_status Task.pp_status Task.json_status [
    `New;
    `Dispatched (Worker.id wt1, `Pending);
    `Dispatched (Worker.id wj1, `Started);
    `Pending;
    `Complete `Success;
    `Complete `Failure;
    `Cancelled
  ]

let simple_job_status =
  simple_status Job.pp_status Job.json_status [
    `Pending;
    `Runnable;
    `Dispatched (Worker.id wt1, `Pending);
    `Dispatched (Worker.id wj1, `Started);
    `Complete `Success;
    `Complete `Failure;
    `Cancelled;
  ]

let simple_work_status =
  simple_status Worker.pp_status Worker.json_status [
    `Idle;
    `Job (Job.id j1);
    `Job (Job.id j2);
    `Task (Task.id t1);
  ]

let suite = [
  "package"  , `Quick, simple_package;
  "task"     , `Quick, simple_task;
  "host"     , `Quick, simple_host;
  "switch"   , `Quick, simple_switch;
  "worker"   , `Quick, simple_worker;
  "job"      , `Quick, simple_job;
  "object"   , `Quick, simple_object;
  "task status", `Quick, simple_task_status;
  "job status" , `Quick, simple_job_status;
  "work status", `Quick, simple_work_status;
]
