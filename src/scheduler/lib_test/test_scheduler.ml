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

open Lwt.Infix

let rcheck f msg x y =
  retry (fun () -> y () >|= fun y -> Alcotest.check f msg x y)

let mk f x () = Lwt.return (f x)

let basic_tasks () =
  let test () =
    store () >>= fun s ->
    Scheduler.Task.start s >>= fun t ->
    rcheck tasks_t "0 tasks" [] (mk Scheduler.Task.list t)
    >>= fun () ->
    Store.Task.add s t1 >>= fun () ->
    rcheck tasks_t "1 task" [t1] (mk Scheduler.Task.list t)
    >>= fun () ->
    Scheduler.Task.stop t >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
    >>= fun () ->
    Scheduler.Task.start s >>= fun t ->
    rcheck tasks_t "2 task" [t1] (mk Scheduler.Task.list t)
    >>= fun () ->
    rcheck Alcotest.(option task_status_t) "status" (Some `New)
      (fun () -> Store.Task.status s (Task.id t1))
    >>= fun () ->
    Scheduler.Task.stop t >>= fun () ->
    rcheck Alcotest.int "no more watches" 0
      (fun () -> Lwt.return @@ Store.nb_watches s)
  in
  run test

let basic_jobs () =
  let test () =
    let check_roots t =
      List.iter (fun h ->
          let root = job_root h in
          Alcotest.(check @@ option job_t) "root"
            (Some root) (Scheduler.Job.peek t h)
        ) hosts
    in
    store () >>= fun s ->
    Scheduler.Job.start s >>= fun t ->
    rcheck Alcotest.(list job_t) "0 jobs" [] (mk Scheduler.Job.list t)
    >>= fun () ->
    Lwt_list.iter_p (Store.Job.add s) jobs >>= fun () ->
    rcheck jobs_t "jobs" jobs (mk Scheduler.Job.list t)
    >>= fun () ->
    check_roots t;
    Scheduler.Job.stop t >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
    >>= fun () ->
    Scheduler.Job.start s >>= fun t ->
    rcheck jobs_t "jobs" jobs (mk Scheduler.Job.list t)
    >>= fun () ->
    check_roots t;
    Scheduler.Job.stop t >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

let basic_workers () =
  let test () =
    store () >>= fun s ->
    Scheduler.Worker.start s >>= fun t ->
    Alcotest.(rcheck @@ list worker_t) "0 workers" [] (mk Scheduler.Worker.list t)
    >>= fun () ->
    Lwt_list.iter_p (Store.Worker.add s) workers >>= fun () ->
    rcheck workers_t "workers" workers (mk Scheduler.Worker.list t)
    >>= fun () ->
    rcheck bool_t "worker" true (fun () ->
        let w = match Scheduler.Worker.peek t `Job with
          | None   -> Alcotest.fail "worker peek"
          | Some w -> w
        in
        Lwt.return (List.mem w job_workers))
    >>= fun () ->
    Scheduler.Worker.stop t >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
    >>= fun () ->
    Scheduler.Worker.start s >>= fun t ->
    rcheck workers_t "workers" workers (mk Scheduler.Worker.list t)
    >>= fun () ->
    Lwt_list.iter_s (fun w -> Store.Worker.forget s (Worker.id w)) workers
    >>= fun () ->
    rcheck Alcotest.(list worker_t) "0 workers again"
      [] (mk Scheduler.Worker.list t)
    >>= fun () ->
    Scheduler.Worker.stop t >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

let task_check s ~section sched msg expected =
  let msg = Printf.sprintf "[%s] %s" section msg in
  let tasks = Scheduler.Task.list (Scheduler.task sched) in
  Alcotest.(check @@ tasks_t) "t1 is monitored" [t1] tasks;
  Store.Task.status s (Task.id t1) >>= fun status ->
  Alcotest.(check @@ option task_status_t) msg (Some expected) status;
  Lwt.return_unit

let task_check s ~section sched msg expected =
  retry (fun () -> task_check s ~section sched msg expected)

(* - add a task
   - start the scheduler
   - add a worker: the task is picked-up
   - kill the worker: the task is new again *)
let task_scheduler_1 () =
  let test () =
    store () >>= fun s ->
    Store.Task.add s t1  >>= fun () ->
    Scheduler.start s    >>= fun scheduler ->
    let check = task_check s ~section:"task -> scheduler -> worker" scheduler in
    check "init" `New >>= fun () ->
    Store.Worker.add s wj1 >>= fun () ->
    check "init" `New >>= fun () ->
    Store.Worker.add s wt1 >>= fun () ->
    check "start" (`Dispatched (Worker.id wt1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wt1) >>= fun () ->
    check "forget" `New >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

(* - add a worker
   - start the scheduler
   - add a task: the task is picked-up
   - kill the worker: the task is new again *)
let task_scheduler_2 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wt1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = task_check s ~section:"worker -> scheduler -> task" scheduler in
    rcheck tasks_t "t1 is not monitored" []
      (mk Scheduler.Task.list (Scheduler.task scheduler))
    >>= fun () ->
    Store.Task.add s t1 >>= fun () ->
    check "start" (`Dispatched (Worker.id wt1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wt1) >>= fun () ->
    check "forget" `New >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

(* - add a worker
   - add a task
   - start the scheduler
   - the task is picked-up
   - kill the worker: the task is new again *)
let task_scheduler_3 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wt1 >>= fun () ->
    Store.Task.add s t1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = task_check s ~section:"worker -> task -> scheduler" scheduler in
    check "start" (`Dispatched (Worker.id wt1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wt1) >>= fun () ->
    check "forget" `New >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

let job_check s ~section sched msg expected =
  let msg = Printf.sprintf "[%s] %s" section msg in
  let jobs = Scheduler.Job.list (Scheduler.job sched) in
  Alcotest.(check @@ jobs_t) "jr1 is monitored" [jr1] jobs;
  Store.Job.status s (Job.id jr1) >>= fun status ->
  Alcotest.(check @@ option job_status_t) msg (Some expected) status;
  Lwt.return_unit

let job_check s ~section sched msg expected =
  retry (fun () -> job_check s ~section sched msg expected)

(* - add a job
   - start the scheduler
   - add a worker: the job is picked-up
   - kill the worker: the job is pending again *)
let job_scheduler_1 () =
  let test () =
    store () >>= fun s ->
    Store.Job.add s jr1  >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = job_check s ~section:"job -> scheduler -> worker" scheduler in
    check "init" `Runnable >>= fun () ->
    Store.Worker.add s wt1 >>= fun () ->
    check "init" `Runnable >>= fun () ->
    Store.Worker.add s wj1 >>= fun () ->
    check "start" (`Dispatched (Worker.id wj1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wj1) >>= fun () ->
    check "forget" `Runnable >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

(* - add a worker
   - start the scheduler
   - add a job: the job is picked-up
   - kill the worker: the job is new again *)
let job_scheduler_2 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wj1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = job_check s ~section:"worker -> scheduler -> job" scheduler in
    rcheck jobs_t "jr1 is not monitored" []
      (mk Scheduler.Job.list (Scheduler.job scheduler)) >>= fun () ->
    Store.Job.add s jr1 >>= fun () ->
    check "start" (`Dispatched (Worker.id wj1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wj1) >>= fun () ->
    check "forget" `Runnable >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

(* - add a worker
   - add a job
   - start the scheduler
   - the job is picked-up
   - kill the worker: the job is new again *)
let job_scheduler_3 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wj1 >>= fun () ->
    Store.Job.add s jr1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = job_check s ~section:"worker -> job -> scheduler" scheduler in
    check "start" (`Dispatched (Worker.id wj1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wj1) >>= fun () ->
    check "forget" `Runnable >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

(* - add a task
   - assign it to a dead worker
   - start the scheduler
   - the task should be new *)
let task_and_dead_worker () =
  let test () =
    store () >>= fun s ->
    Store.Task.add s t1 >>= fun () ->
    Store.Task.ack s (Task.id t1) (Worker.id wt1) >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = task_check s ~section:"task to dead worker" scheduler in
    check "start" `New >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

(* - add a job
   - assign it to a dead worker
   - start the scheduler
   - the job should be runnable *)
let job_and_dead_worker () =
  let test () =
    store () >>= fun s ->
    Store.Job.add s jr1 >>= fun () ->
    Store.Job.ack s (Job.id jr1) (Worker.id wt1) >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = job_check s ~section:"task to dead worker" scheduler in
    check "start" `Runnable >>= fun () ->
    Scheduler.stop scheduler >>= fun () ->
    rcheck Alcotest.int "no more watches" 0 (mk Store.nb_watches s)
  in
  run test

let suite = [
  "basic tasks"    , `Quick, basic_tasks;
  "basic jobs"     , `Quick, basic_jobs;
  "basic workers"  , `Quick, basic_workers;
  "task schduler 1", `Quick, task_scheduler_1;
  "task schduler 2", `Quick, task_scheduler_2;
  "task schduler 3", `Quick, task_scheduler_3;
  "job schduler 1" , `Quick, job_scheduler_1;
  "job schduler 2" , `Quick, job_scheduler_2;
  "job schduler 3" , `Quick, job_scheduler_3;
  "task & dead worker", `Quick, task_and_dead_worker;
  "job & dead worker" , `Quick, job_and_dead_worker;
]
