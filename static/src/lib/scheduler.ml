(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let debug fmt = Gol.debug ~section:"scheduler" fmt
let err fmt = Printf.ksprintf failwith ("Scheduler: " ^^ fmt)
let todo f = err "TODO %s" f
let count name =
  let c = ref 0 in fun () -> incr c; name ^ "-" ^ string_of_int !c

let some x = Some x
let none () = None

let opt ppf =
  let none ppf () = Fmt.pf ppf "-" in
  Fmt.(option ~none) ppf

module XTask = struct

  module TMap = Map.Make(Task)

  type status = [ Task.status | `Await ]

  let pp_status ppf = function
    | `Await -> Fmt.string ppf "await"
    | #Task.status as x -> Task.pp_status ppf x

  type t = {
    cond: unit Lwt_condition.t;
    store: Store.t;                                            (* local store *)
    mutable tasks: status TMap.t;        (* new, dispatched and pending tasks *)
    mutable stop: unit -> unit Lwt.t;
    mutable cancels: (unit -> unit Lwt.t) TMap.t;
  }

  let list t = TMap.fold (fun t _ acc -> t :: acc) t.tasks []

  let is_runnable t task = try TMap.find task t.tasks = `New with Not_found -> false

  let status t task =
    TMap.fold (fun t s -> function
        | None -> if Task.equal t task then Some s else None
        | x    -> x
      ) t.tasks None

  let cancel t task =
    try
      TMap.find task t.cancels () >|= fun () ->
      t.cancels <- TMap.remove task t.cancels
    with Not_found ->
      Lwt.return_unit

  let remove_task t task =
    let id = Task.id task in
    debug "remove taks:%a" Id.pp id;
    cancel t task >|= fun () ->
    t.tasks <- TMap.remove task t.tasks

  let update_status t task s =
    let old_s = status t task in
    if old_s <> Some s && not (old_s = Some `Await && s = `New) then (
      debug "task:%a: %a => %a"
        Id.pp (Task.id task) (opt pp_status ) old_s pp_status s;
      t.tasks <- TMap.add task s t.tasks;
      if s = `New then Lwt_condition.broadcast t.cond ()
    )

  (* FIXME: need to watch the related jobs status updates to update
     the task status. *)
  let watch_task_status t task =
    if TMap.mem task t.cancels then Lwt.return_unit
    else (
      let id = Task.id task in
      Store.Task.watch_status t.store id (function
          | None ->
            remove_task t task  >>= fun () ->
            Store.Task.forget t.store id
          | Some (`Complete _) -> remove_task t task
          | Some (`New | `Pending | `Dispatched _ as s) ->
            update_status t task s;
            Lwt.return_unit
          | Some `Cancelled -> todo "Task.cancel"
        ) >|= fun c ->
      t.cancels <- TMap.add task c t.cancels
    )

  let add_task t task =
    if TMap.mem task t.tasks then Lwt.return_unit
    else (
      let id = Task.id task in
      Store.Task.status t.store id >>= function
      | None -> Store.Task.forget t.store id
      | Some `Cancelled -> todo "cancelled tasks"
      | Some (`Complete _) -> Lwt.return_unit
      | Some (`New | `Pending | `Dispatched _ as s) ->
        update_status t task s;
        watch_task_status t task
    )

  let empty store =
    let cond = Lwt_condition.create () in
    let stop () = Lwt.return_unit in
    let cancels = TMap.empty in
    let tasks = TMap.empty in
    { cond; store; tasks; stop; cancels; }

  let new_tasks t =
    TMap.fold (fun v status acc -> match status with
        | `New -> v :: acc
        | _    -> acc
      ) t.tasks []

  let peek t = match new_tasks t with
    | []   -> None
    | h::_ -> Some h

  let count_task = count "task"

  let peek_s t =
    let name = count_task () in
    let rec aux () = match peek t with
      | None ->
        debug "[%s] waiting for a new task ..." name;
        Lwt_condition.wait t.cond >>= fun () ->
        aux ()
      | Some ta ->
        let id = Task.id ta in
        debug "[%s] task:%a can be started" name Id.pp id;
        update_status t ta `Await;
        Lwt.return ta
    in
    aux ()

  let watch_task t = Store.Task.watch t.store (add_task t)

  let start store =
    debug "starting the task scheduler";
    let t = empty store in
    Store.Task.list store >>=
    Lwt_list.filter_map_p (fun id ->
        Store.Task.mem store id >>= function
        | true  -> Store.Task.get store id >|= some
        | false -> Store.Task.forget store id >|= none
      ) >>=
    Lwt_list.iter_p (add_task t) >>= fun () ->
    watch_task t >|= fun cancel ->
    t.stop <- cancel;
    t

  let stop t =
    debug "stopping the task scheduler";
    t.stop () >>= fun () ->
    let cancels = TMap.fold (fun _ v l -> v :: l) t.cancels [] in
    Lwt_list.iter_p (fun f -> f ()) cancels

end

module XJob = struct

  module JSet = Set.Make(Job)
  module JMap = Map.Make(Job)

  type status = [ Job.status | `Await ]

  let pp_status ppf = function
    | `Await -> Fmt.string ppf "await"
    | #Job.status as x -> Job.pp_status ppf x

  (* FIXME: add formard deps caching, a la TUP. *)
  type t = {
    cond  : unit Lwt_condition.t;
    store : Store.t;
    mutable jobs: status JMap.t;
    mutable stop: unit -> unit Lwt.t;
    mutable cancels: (unit -> unit Lwt.t) JMap.t;
    mutable on_complete: (unit -> unit Lwt.t) JMap.t;
  }

  let on_complete t j f = t.on_complete <- JMap.add j f t.on_complete

  let complete t j =
    try
      let f = JMap.find j t.on_complete in
      t.on_complete <- JMap.remove j t.on_complete;
      f ()
    with Not_found ->
      Lwt.return_unit

  let list t = JMap.fold (fun k _ acc -> k :: acc) t.jobs []

  let _dump ppf t =
    Fmt.pf ppf "[%d] " (Unix.getpid ());
    JMap.bindings t.jobs
    |> List.iter (fun (w, s) ->
        Fmt.pf ppf "%a:%s:%a "
          Id.pp (Job.id w) (Host.short @@ Job.host w) pp_status s
      )

  let runnable_jobs t h =
    JMap.fold (fun job status acc ->
        let h' = Job.host job in
        if not (Host.equal h' h) then acc
        else match status with
          | `Runnable -> job :: acc
          | _ -> acc
      ) t.jobs []

  let is_runnable t j = try JMap.find j t.jobs = `Runnable with Not_found -> false

  let status t job =
    JMap.fold (fun j s -> function
        | None -> if Job.equal j job then Some s else None
        | x    -> x
      ) t.jobs None

  let cancel t j =
    try
      JMap.find j t.cancels () >|= fun () ->
      t.cancels <- JMap.remove j t.cancels
    with Not_found ->
      Lwt.return_unit

  let remove_job t j reason =
    debug "remove job:%a (%s)" Id.pp (Job.id j) reason;
    cancel t j >|= fun () ->
    t.jobs <- JMap.remove j t.jobs

  let update_status t j s =
    let old_s = status t j in
    if old_s <> Some s && not (old_s = Some `Await && s = `Runnable) then (
      debug "job:%a: %a => %a"
        Id.pp (Job.id j) (opt pp_status) old_s pp_status s;
      t.jobs <- JMap.add j s t.jobs;
      if s = `Runnable then Lwt_condition.broadcast t.cond ()
    )

  let peek t host = match runnable_jobs t host with
    | []   -> None
    | h::_ -> Some h

  let count_job = count "job"

  let peek_s t host =
    let name = count_job () in
    let rec aux () = match peek t host with
      | Some j ->
        let id = Job.id j in
        debug "[%s] job:%a can run on %s" name Id.pp id (Host.short host);
        update_status t j `Await;
        Lwt.return j
      | None   ->
        debug "[%s] waiting for a job on %s ..." name (Host.short host);
        Lwt_condition.wait t.cond >>= fun () ->
        aux ()
    in
    aux ()

  let status_of_id t id =
    JMap.fold (fun j status -> function
        | None -> if Id.equal (Job.id j) id then Some status else None
        | s    -> s
      ) t.jobs None

  (* FIXME: use rev-deps to make it fast (a al TUP). *)
  let update_runnable t =
    let jobs = ref [] in
    JMap.iter (fun job -> function
        | `Pending ->
          let inputs = Job.inputs job in
          let is_success j =
            match status_of_id t j with
            | Some (`Complete `Success) -> true
            | _ -> false
          in
          if List.for_all is_success inputs then jobs := job :: !jobs
        | _ -> ()
      ) t.jobs;
    Lwt_list.iter_p (fun job ->
        Store.Job.runnable t.store (Job.id job) >|= fun () ->
        update_status t job `Runnable
      ) !jobs

  let watch_job_status t j =
    if JMap.mem j t.cancels then Lwt.return_unit
    else (
      let id = Job.id j in
      Store.Job.watch_status t.store id (function
          | None ->
            remove_job t j "removed">>= fun () ->
            Store.Job.forget t.store id
          | Some `Cancelled -> todo "job cancelled"
          | Some (`Pending | `Runnable | `Dispatched _  as s) ->
            update_status t j s;
            Lwt.return_unit
          (* FIXME: decide if we want to keep the jobs in memory... *)
          (* remove_job t j "complete" *)
          | Some (`Complete `Success as s) ->
            update_status t j s;
            complete t j >>= fun () ->
            update_runnable t
          | Some (`Complete `Failure as s) ->
            update_status t j s;
            complete t j >>= fun () ->
            Lwt.return_unit
        ) >|= fun c ->
      t.cancels <- JMap.add j c t.cancels;
    )

  let add_job t job =
    if JMap.mem job t.jobs then Lwt.return_unit
    else (
      let id = Job.id job in
      debug "add job:%a" Id.pp id;
      Store.Job.status t.store id >>= function
      | None -> Store.Job.forget t.store id
      | Some `Cancelled -> todo "cancelled job"
      | Some (`Complete _) -> Lwt.return_unit
      | Some (`Pending | `Dispatched _ | `Runnable as s) ->
        update_status t job s;
        update_runnable t >>= fun () ->
        watch_job_status t job
    )

  let watch_jobs t = Store.Job.watch t.store (add_job t)

  let empty store =
    let stop () = Lwt.return_unit in
    { store; stop;
      cond = Lwt_condition.create ();
      jobs = JMap.empty;
      cancels  = JMap.empty;
      on_complete = JMap.empty; }

  let start store =
    debug "starting the job scheduler.";
    let t = empty store in
    Store.Job.list store >>=
    Lwt_list.filter_map_p (fun id ->
        Store.Job.mem store id >>= function
        | true  -> Store.Job.get store id >|= some
        | false -> Store.Job.forget store id >|= none
      ) >>=
    Lwt_list.iter_p (add_job t) >>= fun () ->
    watch_jobs t >|= fun cancel ->
    t.stop <- cancel;
    t

  let stop t =
    debug "stoping the job scheduler";
    t.stop () >>= fun () ->
    let cancels = JMap.fold (fun _ c acc -> c :: acc) t.cancels [] in
    Lwt_list.iter_p (fun f -> f ()) cancels

end

module XWorker = struct

  module WSet = Set.Make(Worker)
  module WMap = Map.Make(Worker)

  type status = [ Worker.status | `Await ]

  let pp_status ppf = function
    | `Await -> Fmt.string ppf "await"
    | #Worker.status as x -> Worker.pp_status ppf x

  type t = {
    cond   : unit Lwt_condition.t;
    store  : Store.t;
    mutable cancels: (unit -> unit Lwt.t) WMap.t;
    mutable ticks  : (unit -> unit Lwt.t) WMap.t;
    mutable stop   : unit -> unit Lwt.t;    (* stop the main scheduler watch. *)
    mutable workers: status WMap.t;                       (* all the workers. *)
  }

  let list t = WMap.fold (fun e _ l -> e :: l)  t.workers []

  let cancel t w =
    let aux map =
    try
      WMap.find w map () >|= fun () ->
      WMap.remove w map
    with Not_found ->
      Lwt.return map
    in
    aux t.cancels >>= fun cancels ->
    aux t.ticks   >|= fun ticks ->
    t.cancels <- cancels;
    t.ticks   <- ticks

  (* reset the job a dead worker was working on. This will trigger the
     job scheduler to distribute it to someone else. *)
  let reset_job t = function
    | `Job id  -> Store.Job.runnable t.store id
    | `Task id -> Store.Task.reset t.store id
    | `Await
    | `Idle    -> Lwt.return_unit

  let status t w =
    WMap.fold (fun w' s -> function
        | None -> if Worker.equal w w' then Some s else None
        | x    -> x
      ) t.workers None

  let remove_worker t w =
    let id = Worker.id w in
    debug "remove worker:%a" Id.pp id;
    let reset () = match status t w with
      | None   -> Lwt.return_unit
      | Some s -> reset_job t s
    in
    reset () >>= fun () ->
    cancel t w >|= fun () ->
    t.workers <- WMap.remove w t.workers

  (* watch for worker ticks, clean-up the worker state if it's dead. *)
  let watch_woker_ticks t w =
    if WMap.mem w t.ticks then Lwt.return_unit
    else (
      let id = Worker.id w in
      let now () = Unix.time () in
      let last_tick = ref (now ()) in
      let is_dead = 10. in
      Store.Worker.watch_ticks t.store id (fun _tick ->
          last_tick := now ();
          Lwt.return_unit
        )
      >|= fun cancel ->
      t.ticks <- WMap.add w cancel t.ticks;
      let forget () =
        remove_worker t w >>= fun () ->
        Store.Worker.forget t.store id
      in
      let rec loop () =
        if now () -. !last_tick > is_dead then forget ()
        else
          Store.Worker.mem t.store id >>= function
          | false -> forget ()
          | true ->
            Lwt_unix.sleep is_dead >>= fun () ->
            loop ()
      in
      Lwt.async loop
    )

  let _dump ppf t =
    WMap.bindings t.workers
    |> List.iter (fun (w, s) ->
        Fmt.pf ppf "%a:%a " Id.pp (Worker.id w) pp_status s
      )

  let update_status t w s =
    let old_s = status t w in
    if Some s <> old_s && not (old_s = Some `Await && s = `Idle) then (
      debug "%a-worker:%a: %a => %a" Worker.pp_kind (Worker.kind w)
        Id.pp (Worker.id w) (opt pp_status) old_s pp_status s;
      t.workers <- WMap.add w s t.workers;
      if s = `Idle then Lwt_condition.broadcast t.cond ()
    )

  let watch_worker_status t w =
    if WMap.mem w t.cancels then Lwt.return_unit
    else (
      let id = Worker.id w in
      Store.Worker.watch_status t.store id (function
          | None   ->
            remove_worker t w >>= fun () ->
            Store.Worker.forget t.store id
          | Some s ->
            update_status t w (s :> status);
            Lwt.return_unit
        ) >|= fun cancel ->
      t.cancels <- WMap.add w cancel t.cancels
    )

  let add_worker t w =
    if WMap.mem w t.workers then Lwt.return_unit
    else (
      let id = Worker.id w in
      Store.Worker.status t.store id >>= function
      | None   -> Store.Worker.forget t.store id
      | Some s ->
        update_status t w (s :> status);
        watch_worker_status t w >>= fun () ->
        watch_woker_ticks t w
    )

  let remove_worker_id t id =
    let w =
      WMap.fold (fun w _ -> function
          | None -> if Id.equal (Worker.id w) id then Some w else None
          | acc  -> acc
        ) t.workers None
    in match w with
    | None   -> Store.Worker.forget t.store id
    | Some w -> remove_worker t w

  let watch_workers t = Store.Worker.watch t.store (function
      | `Added w    -> add_worker t w
      | `Removed id -> remove_worker_id t id
    )

  let idle_workers t k =
    WMap.fold (fun w status acc ->
        match status with
        | `Idle -> if Worker.kind w = k then w :: acc else acc
        | _     -> acc
      ) t.workers []

  let is_runnable t w =
    try WMap.find w t.workers = `Idle with Not_found -> false

  let peek t k = match idle_workers t k with
    | []   -> None
    | h::_ -> Some h

  let count_worker = count "worker"

  let peek_s t k =
    let name = count_worker () in
    let rec aux () = match peek t k with
      | Some w ->
        let id = Worker.id w in
        debug "[%s] %a-worker:%a is ready to start"
          name Worker.pp_kind k Id.pp id;
        update_status t w `Await;
        Lwt.return w
      | None   ->
      debug "[%s] waiting for a %a-worker ..." name Worker.pp_kind k;
      Lwt_condition.wait t.cond >>= fun () ->
      aux ()
    in
    aux ()

  let empty store =
    let cond = Lwt_condition.create () in
    let cancels = WMap.empty in
    let ticks   = WMap.empty in
    let workers = WMap.empty in
    let stop () = Lwt.return_unit in
    { cond; store; ticks; cancels; workers; stop }

  let start store =
    debug "starting the work scheduler";
    let t = empty store in
    watch_workers t >>= fun cancel ->
    t.stop <- cancel;
    Store.Worker.list store >>=
    Lwt_list.filter_map_p (fun id ->
        Store.Worker.mem store id >>= function
        | true  -> Store.Worker.get store id >|= some
        | false -> remove_worker_id t id >|= none
      ) >>= fun workers ->
    Lwt_list.iter_p (add_worker t) workers >|= fun () ->
    t

  let stop t =
    debug "stoping the work scheduler";
    t.stop () >>= fun () ->
    let cancels = WMap.fold (fun _ l acc -> l :: acc) t.cancels [] in
    let cancels = WMap.fold (fun _ l acc -> l :: acc) t.ticks cancels in
    Lwt_list.iter_p (fun f -> f ()) cancels

end

let fmt s id = s ^ " " ^ Id.to_string id

type t = {
  j: XJob.t; t: XTask.t; w: XWorker.t;
  mutable cancels: (unit -> unit);  }

let job t = t.j
let task t = t.t
let worker t = t.w

let cleanup_dead_workers t =
  let workers = XWorker.list t.w in
  let is_dead id =
    let has_same_id w = Id.equal id (Worker.id w) in
    not (List.exists has_same_id workers)
  in
  let tasks = XTask.list t.t in
  let jobs = XJob.list t.j in
  Lwt_list.iter_p (fun task ->
      match XTask.status t.t task with
      | Some (`Dispatched (w,_)) when is_dead w ->
        Store.Task.reset t.t.XTask.store (Task.id task)
      | _ -> Lwt.return_unit
    ) tasks
  >>= fun () ->
  Lwt_list.iter_p (fun job ->
      match XJob.status t.j job with
      | Some (`Dispatched (w,_)) when is_dead w ->
        Store.Job.runnable t.j.XJob.store (Job.id job)
      | _ -> Lwt.return_unit
    ) jobs

let refresh_task t =
  debug "refresh task";
  let store = t.t.XTask.store in
  let tasks = XTask.list t.t in
  Lwt_list.iter_p (fun task ->
      match XTask.status t.t task with
      | Some `Pending ->
        Store.Task.jobs store (Task.id task) >>= fun jobs ->
        let status =
          List.map (XJob.status_of_id t.j) jobs
          |> List.map (function
              | None        -> `Pending
              | Some `Await -> `Runnable
              | Some (#Job.status as s) -> s )
          |> Job.task_status
        in
        if status <> `Pending then
          Store.Task.update_status store (Task.id task) status
        else
          Lwt.return_unit
      | _ -> Lwt.return_unit
    ) tasks

let start store =
  XJob.start store    >>= fun j ->
  XTask.start store   >>= fun t ->
  XWorker.start store >>= fun w ->
  let scheduler = { j; t; w; cancels = (fun () -> ()); } in
  cleanup_dead_workers scheduler >|= fun () ->
  let peek_job host = XJob.peek_s j host in
  let peek_task () = XTask.peek_s t in
  let peek_worker kind = XWorker.peek_s w kind in
  let dbg worker =
    debug "DISPATCH: %a-worker:%a (%s) is available"
      Worker.pp_kind (Worker.kind worker)
      Id.pp (Worker.id worker)
      (Host.short @@ Worker.host worker);
  in
  let rec dispatch_job () =
    peek_worker `Job >>= fun worker ->
    dbg worker;
    let wid = Worker.id worker in
    let host = Worker.host worker in
    peek_job host >>= fun j ->
    XJob.on_complete scheduler.j j (fun () -> refresh_task scheduler);
    let id = Job.id j in
    Store.with_transaction store (fmt "starting job" id) (fun t ->
        debug "DISPATCH: job:%a to worker:%a" Id.pp id Id.pp wid;
        Store.Job.dispatch_to t id wid >>= fun () ->
        Store.Worker.start_job t wid id
      ) >>= fun () ->
    dispatch_job ()
  in
  let rec dispatch_task () =
    peek_worker `Task >>= fun worker ->
    dbg worker;
    let wid = Worker.id worker in
    peek_task () >>= fun t ->
    let id = Task.id t in
    Store.with_transaction store (fmt "starting task" id) (fun t ->
        debug "DISPATCH: task:%a to worker:%a" Id.pp id Id.pp wid;
        Store.Task.dispatch_to t id wid >>= fun () ->
        Store.Worker.start_task t wid id
      ) >>= fun () ->
    dispatch_task ()
  in
  let loop =
    Lwt.catch
      (fun () -> Lwt.join [dispatch_job (); dispatch_task ()])
      (function Lwt.Canceled -> Lwt.return_unit | e -> Lwt.fail e)
  in
  scheduler.cancels <- (fun () -> Lwt.cancel loop);
  Lwt.async (fun () -> loop);
  scheduler

let stop t =
  t.cancels ();
  Lwt.join [
    XJob.stop t.j;
    XWorker.stop t.w;
    XTask.stop t.t;
  ]

module type S = sig
  type t
  type value
  val start: Store.t -> t Lwt.t
  val stop: t -> unit Lwt.t
  val list: t -> value list
  val is_runnable: t -> value -> bool
end

module Job = XJob
module Worker = XWorker
module Task = XTask
