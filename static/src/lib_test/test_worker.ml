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

open Lwt.Infix
include Test_common

let cache_root = "/tmp/datakit-tests"

let check_task s sched msg expected =
  let testable =
    let compare (x, _) (y, _) = Task.compare x y in
    let pp ppf (x, y) =
      Fmt.(pf ppf "%a:%a" Id.pp (Task.id x) (option Task.pp_status) y)
    in
    set (of_pp pp) compare
  in
  let tasks = Scheduler.Task.list (Scheduler.task sched) in
  Lwt_list.map_p (fun t ->
      Store.Task.status s (Task.id t) >|= fun status ->
      t, status
    ) tasks
  >|= fun tasks ->
  let expected = List.map (fun (t, s) -> t, Some s) expected in
  Alcotest.(check testable) msg expected tasks

let check_task s sched msg expected =
  retry (fun () -> check_task s sched msg expected)

let check_job s sched msg expected =
  let testable =
    let compare (x, _) (y, _) = Job.compare x y in
    let pp ppf (x, y) =
      Fmt.(pf ppf "%a:%a" Id.pp (Job.id x) (option Job.pp_status) y)
    in
    set (of_pp pp) compare
  in
  let jobs = Scheduler.Job.list (Scheduler.job sched) in
  Lwt_list.map_p (fun t ->
      Store.Job.status s (Job.id t) >|= fun status ->
      t, status
    ) jobs
  >|= fun jobs ->
  let expected = List.map (fun (t, s) -> t, Some s) expected in
  Alcotest.(check testable) msg expected jobs

let check_job s sched msg expected =
  retry (fun () -> check_job s sched msg expected)

let check_worker s sched msg expected =
  let testable =
    let compare (x, _) (y, _) = Worker.compare x y in
    let pp ppf (x, y) =
      Fmt.(pf ppf "%a:%a" Id.pp (Worker.id x) (option Worker.pp_status) y)
    in
    set (of_pp pp) compare
  in
  let workers = Scheduler.Worker.list (Scheduler.worker sched) in
  Lwt_list.map_p (fun t ->
      Store.Worker.status s (Worker.id t) >|= fun status ->
      t, status
    ) workers
  >|= fun workers ->
  let expected = List.map (fun (t, s) -> t, Some s) expected in
  Alcotest.(check testable) msg expected workers

let check_worker s sched msg expected =
  retry (fun () -> check_worker s sched msg expected)

let task_worker () =
  let t, w = Lwt.task () in
  let callback _t _task f = Lwt.join [t; f jr1; f jnr1] in
  callback, Lwt.wakeup w

let job_worker () =
  let t1, w1 = Lwt.task () in
  let t2, w2 = Lwt.task () in
  let callback _t job =
    if Job.equal job jr1 then t1
    else if Job.equal job jnr1 then t2
    else failwith "job_worker"
  in
  callback, Lwt.wakeup w1, Lwt.wakeup w2

(* - starts the scheduler
   - add a task
   - add a task worker
   - check that the task is dispatched
   - add job workers
   - check that the jobs are complete
   - check that the task is complete
 *)
let test_task_worker () =
  let test () =
    store () >>= fun s ->
    Scheduler.start s >>= fun scheduler ->
    let check_t = check_task s scheduler in
    let check_j = check_job s scheduler in
    let check_w = check_worker s scheduler in
    Store.Task.add s t1 >>= fun () ->
    check_t "init task" [t1, `New] >>= fun () ->
    check_j "init job" [] >>= fun () ->
    check_w "init worker" [] >>= fun () ->

    let callback, stop = task_worker () in
    Task_worker.start ~callback ~host ~opam_root s >>= fun tw1 ->
    let w1 = Task_worker.worker tw1 in
    let wid1 = Worker.id w1 in
    (* need to wait two ticks here, as this involved 2 transitions in
       the state machine. *)
    check_t "start task" [t1, `Dispatched (wid1, `Started)] >>= fun () ->
    check_j "start job" [] >>= fun () ->
    check_w "start worker" [w1, `Task (Task.id t1)] >>= fun () ->

    stop ();
    check_t "ready task" [t1, `Pending] >>= fun () ->
    check_j "ready job" [jr1, `Runnable; jnr1, `Pending] >>= fun () ->
    check_w "ready worker" [w1, `Idle] >>= fun () ->

    let callback, stop1, stop2 = job_worker () in
    Job_worker.start ~callback ~host ~opam_root s >>= fun tw2 ->
    let w2 = Job_worker.worker tw2 in
    let wid2 = Worker.id w2 in
    check_t "start2 task" [t1, `Pending] >>= fun () ->
    check_w "start2 worker" [w1, `Idle; w2, `Job (Job.id jr1)] >>= fun () ->
    check_j "start2 job" [
      jr1 , `Dispatched (wid2, `Started);
      jnr1, `Pending
    ] >>= fun () ->

    stop1 `Success;
    check_t "next task" [t1, `Pending] >>= fun () ->
    check_j "next job" [
      jr1 , `Complete `Success;
      jnr1, `Dispatched (wid2, `Started)
    ] >>= fun () ->
    check_w "next worker" [w1, `Idle; w2, `Job (Job.id jnr1)] >>= fun () ->

    stop2 `Success;
    check_j "end job" [
      jr1 , `Complete `Success;
      jnr1, `Complete `Success;
    ] >>= fun () ->
    check_t "end task" [] >>= fun () ->
    check_w "end worker" [w1, `Idle; w2, `Idle] >>= fun () ->

    Task_worker.stop tw1 >>= fun () ->
    Job_worker.stop tw2 >>= fun () ->
    check_w "grand final" [] >>= fun () ->
    Scheduler.stop scheduler
  in
  run test

(* - starts the scheduler
   - add a task
   - add a task worker
   - check that the task is dispatched  *)

(* - starts the scheduler
   - add a task
   - add a task worker
   - check that the task is dispatched  *)

let suite = [
  "task", `Quick, test_task_worker;
]
