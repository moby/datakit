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
open Irmin_unix

let debug fmt = Gol.debug ~section:"store" fmt
let err fmt = Printf.ksprintf failwith ("Store: " ^^ fmt)
let (/) dir file = List.append dir [file]

module StringSet = struct
  include Set.Make(String)
  let of_list = List.fold_left (fun s e -> add e s) empty
end

module Basic (S: Irmin.S_MAKER) =
  S (Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

module R = Basic(Irmin_http.Make)
module L = Basic(Irmin_git.FS)

module RV = Irmin.View(R)
module LV = Irmin.View(L)

type store =
  | R  of R.t
  | L  of L.t
  | RV of RV.t
  | LV of LV.t

type 'a callback = 'a -> unit Lwt.t

type cancel = unit callback

type t = { t: string -> store; mutable cancels: (int * cancel) list }

let create t = { t; cancels = [] }

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = "ciso" in
  Irmin.Task.create ~date ~owner msg

let remote ?(uri = Uri.of_string "http://127.0.0.1:8888") () =
  let config = Irmin_http.config uri in
  R.Repo.create config >>= R.master task >|= fun t ->
  create (fun x -> R (t x))

let err_invalid_version v =
  err "invalid /version: got %s, expecting %s." v Version.current

let check_version t =
  L.read (t "read version") ["version"] >>= function
  | None   -> L.update (t "init") ["version"] Version.current
  | Some v ->
    if v <> Version.current then err_invalid_version v else Lwt.return_unit

let local ?root () =
  let config = Irmin_git.config ?root ~bare:true () in
  L.Repo.create config >>= L.master task >>= fun t ->
  check_version t >|= fun () ->
  create (fun x -> L (t x))

let rv t = create (fun _ -> (RV t))
let lv t = create (fun _ -> (LV t))

(* FIXME: should be in Irmin *)
let retry ?(n=5) f =
  let rec aux i =
    if i >= n then
      f () >|= function
      | `Ok ()      -> true
      | `Conflict _ -> false
    else
      f () >>= function
      | `Ok ()      -> Lwt.return_true
      | `Conflict _ ->
        Lwt_unix.sleep (float i /. 10.) >>= fun () ->
        aux (i+1)
  in
  aux 1

let err_cannot_commit_transaction () = err "Cannot commit the transaction"

(* FIXME: this doesn't really work as expected, see
   https://github.com/mirage/irmin/issues/272 *)
let with_transaction ?retry:n t msg f =
  let aux () = match t.t msg with
    | R t ->
      retry ?n (fun () ->
          RV.of_path t [] >>= fun v ->
          f (rv v) >>= fun () ->
          RV.merge_path t [] v
        )
    | L t ->
      retry ?n (fun () ->
          LV.of_path t [] >>= fun v ->
          f (lv v) >>= fun () ->
          LV.merge_path t [] v
        )
    | RV _ | LV _ ->
      (* no nested transactions *)
      Lwt.return_false
  in
  aux () >>= function
  | true  -> Lwt.return_unit
  | false -> err_cannot_commit_transaction ()

let to_str codec v =
  let b = Buffer.create 64 in
  let e = Jsonm.encoder (`Buffer b) in
  let e = Jsont.encoder e codec v in
  match Jsont.encode e with
  | `Ok      ->
    Buffer.add_char b '\n';
    Buffer.contents b
  | `Partial -> assert false

let of_str codec s =
  let e = Jsonm.decoder (`String s) in
  let e = Jsont.decoder e codec in
  match Jsont.decode e with
  | `Ok (_, v)    -> v
  | `Await        -> assert false
  | `Error (_, e) ->
    invalid_arg (Jsont.error_to_string e)

module Store = struct

  let mem = function
    | R t  -> R.mem t
    | L t  -> L.mem t
    | RV t -> RV.mem t
    | LV t -> LV.mem t

  let update = function
    | R t  -> R.update t
    | L t  -> L.update t
    | RV t -> RV.update t
    | LV t -> LV.update t

  let read = function
    | R t  -> R.read t
    | L t  -> L.read t
    | RV t -> RV.read t
    | LV t -> LV.read t

  let read_exn = function
    | R t  -> R.read_exn t
    | L t  -> L.read_exn t
    | RV t -> RV.read_exn t
    | LV t -> LV.read_exn t

  let last l = List.hd (List.rev l)

  let list t k = match t with
    | R t  -> R.list t k  >|= List.map last
    | L t  -> L.list t k  >|= List.map last
    | RV t -> RV.list t k >|= List.map last
    | LV t -> LV.list t k >|= List.map last

  let rmdir = function
    | R t  -> R.remove_rec t
    | L t  -> L.remove_rec t
    | RV t -> RV.remove_rec t
    | LV t -> LV.remove_rec t

  let head = function
    | R t  -> R.head t
    | L t  -> L.head t
    | RV _ | LV _ -> err "watch not supported on views"

  let don't_watch _k ?init:_ _f = Lwt.return (fun () -> Lwt.return_unit)

  let remember t id cancel =
    let cancel () =
      let cancels = List.filter (fun (i,_) -> i<>id) t.cancels in
      t.cancels <- cancels;
      cancel ()
    in
    t.cancels <- (id, cancel) :: t.cancels;
    cancel

  let check t id f x =
    if List.mem_assoc id t.cancels then f x else Lwt.return_unit

  let watch_key t msg k f =
    let id = Random.int 1024_000 in
    begin match t.t msg with
      | R x  -> R.watch_key x k (check t id f)
      | L x  -> L.watch_key x k (check t id f)
      | RV _ | LV _ ->
        (* cannot watch transactions *)
        (* FIXME: fix this in Irmin *)
        don't_watch k f
    end >|= fun cancel ->
    remember t id cancel

  (* FIXME: move that into Irmin *)

  let rv t = RV.list t [] >|= List.map last
  let lv t = LV.list t [] >|= List.map last
  let list_of_view f (_, v) = f v

  let list_diff f = function
    | `Updated (x, y) ->
      list_of_view f x >>= fun x ->
      list_of_view f y >|= fun y ->
      `Updated (x, y)
    | `Added x   -> list_of_view f x >|= fun x -> `Added x
    | `Removed x -> list_of_view f x >|= fun x -> `Removed x

  let init_of_l t key = function
    | None   -> Lwt.return_none
    | Some h ->
      R.of_commit_id (fun () -> R.task t) h (R.repo t) >>= fun t ->
      RV.of_path (t ()) key >|= fun v ->
      Some (h, v)

  let init_of_r t key = function
    | None   -> Lwt.return_none
    | Some h ->
      L.of_commit_id (fun () -> L.task t) h (L.repo t) >>= fun t ->
      LV.of_path (t ()) key >|= fun v ->
      Some (h, v)

  let watch_key_rec t msg key f =
    let id = Random.int 1024_000 in
    head (t.t msg)  >>= fun h ->
    match t.t msg  with
    | R x ->
      init_of_l x key h >>= fun init ->
      RV.watch_path x key ?init (fun d -> list_diff rv d >>= check t id f)
      >|= fun cancel ->
      remember t id cancel
    | L x ->
      init_of_r x key h >>= fun init ->
      LV.watch_path x key ?init (fun d -> list_diff lv d >>= check t id f)
      >|= fun cancel ->
      remember t id cancel
    | RV _ | LV _ ->
      (* cannot watch transactions *)
      don't_watch t key

  let watch t msg root f =
    let process a children =
      Lwt_list.fold_left_s (fun acc id -> match a with
          | `Removed -> Lwt.return (`Removed id :: acc)
          | `Added   ->
            read (t.t msg) (root / id / "value") >|= function
            | Some v -> `Added v :: acc
            | None   -> acc
        ) [] children >>=
      Lwt_list.iter_p f
    in
    watch_key_rec t msg root (function
        | `Added l   -> process `Added l
        | `Removed l -> process `Removed l
        | `Updated (o, n) ->
          let old_ids = StringSet.of_list o in
          let new_ids = StringSet.of_list n in
          let added = StringSet.diff new_ids old_ids in
          let removed = StringSet.diff old_ids new_ids in
          Lwt.join [
            process `Added (StringSet.elements added);
            process `Removed (StringSet.elements removed);
          ]
      )

end

module type S = sig
  type id
  type value
  val add: t -> value -> unit Lwt.t
  val mem: t -> id -> bool Lwt.t
  val get: t -> id -> value Lwt.t
  val list: t -> id list Lwt.t
  val forget: t -> id -> unit Lwt.t
end

let pretty id = Id.to_string id
let fmt msg id = msg ^ " " ^ pretty id
let mk t msg id = t.t (fmt msg id)

module XSource = struct

  let root = ["sources"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"

  let list t =
    Store.list (t.t "list sources") root >|= List.map (Id.of_string `Source)

  let forget t id =
    debug "forget source:%a" Id.pp id;
    Store.rmdir (mk t "forget source" id) (path id)

  let mem t id = Store.mem (mk t "mem source" id) (value_p id)

  let add t obj =
    let id = Source.id obj in
    debug "add: source %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      let obj = to_str Source.json obj in
      Store.update (mk t "publish source" id) (value_p id) obj >|= fun () ->
      debug "add: source %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "get source" id) (value_p id) >|=
    of_str Source.json

end

module XJob = struct

  let root = ["jobs"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"
  let status_p id = path id / "status"
  let outputs_p id = path id / "outputs"
  let output_p id obj = outputs_p id / "outputs" / Id.to_string obj

  let mem t id = Store.mem (mk t "mem job" id) (value_p id)

  let add t job =
    let id = Job.id job in
    debug "add: job %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      with_transaction t (fmt "add job" id) (fun t ->
          Store.update (t.t "") (value_p id) (to_str Job.json job) >>= fun () ->
          Store.update (t.t "") (status_p id) (to_str Job.json_status `Pending)
        ) >|= fun () ->
      debug "add: job %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "find job" id) (value_p id) >|= of_str Job.json

  let update_status status t id =
    let status = to_str Job.json_status status in
    Store.update (mk t ("job " ^ status) id) (status_p id) status

  let success = update_status (`Complete `Success)
  let failure = update_status (`Complete `Failure)
  let pending = update_status `Pending
  let runnable = update_status `Runnable

  let dispatch_to t id w = update_status (`Dispatched (w, `Pending)) t id
  let ack t id w = update_status (`Dispatched (w, `Started)) t id

  let status t id =
    Store.read (mk t "job status" id) (status_p id) >|= function
    | None   -> None
    | Some s -> Some (of_str Job.json_status s)

  let add_output t id src =
    XSource.add t src >>= fun () ->
    let oid = Source.id src in
    Store.update (mk t "add job output" id) (output_p id oid) ""

  let outputs t id =
    Store.list (mk t "list job outputs" id) (outputs_p id) >|=
    List.map (Id.of_string `Source)

  let list t =
    Store.list (t.t "list jobs") root >|= List.map (Id.of_string `Job)

  let forget t id =
    debug "forget job:%a" Id.pp id;
    Store.rmdir (mk t "forget job" id) (path id)

  let watch_status t id f =
    Store.watch_key t (fmt "watch job status" id) (status_p id) (function
        | `Updated (_, (_, s))
        | `Added (_, s) -> f (Some (of_str Job.json_status s))
        | `Removed _    -> f None
      )

  let watch t f =
    let mk v = of_str Job.json v in
    Store.watch t "watch jobs" root (function
        | `Added v   -> f (mk v)
        | `Removed _ -> Lwt.return_unit
      )

end

module XTask = struct

  let root = ["tasks"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"
  let status_p id = path id / "status"
  let jobs_p id = path id / "jobs"
  let job_p id j = jobs_p id / Id.to_string j

  let list t =
    Store.list (t.t "list tasks") root >|= List.map (Id.of_string `Task)

  let forget t id =
    debug "forget task:%a" Id.pp id;
    Store.rmdir (mk t "forget task" id) (path id)

  let mem t id = Store.mem (mk t "mem task" id) (value_p id)

  let add t task =
    let id = Task.id task in
    debug "add: task %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      with_transaction t (fmt "add task" id) (fun t ->
          Store.update (t.t "") (value_p id) (to_str Task.json task) >>= fun () ->
          Store.update (t.t "") (status_p id) (to_str Task.json_status `New)
        )
      >|= fun () ->
      debug "add: task %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "find task" id) (value_p id) >|=
    of_str Task.json

  let add_job t id j = Store.update (mk t "add job" id) (job_p id j) ""

  let jobs t id =
    Store.list (mk t "list jobs of task" id) (jobs_p id) >|=
    List.map (Id.of_string `Job)

  let update_status t id status =
    let status = to_str Task.json_status status in
    Store.update (mk t ("task " ^ status) id) (status_p id) status

  let reset t id = update_status t id `New
  let dispatch_to t id w = update_status t id (`Dispatched (w, `Pending))
  let ack t id w = update_status t id (`Dispatched (w, `Started))

  let refresh_status t id =
    jobs t id >>= fun jobs ->
    Lwt_list.map_p (XJob.status t) jobs >>= fun status ->
    let status =
      List.fold_left (fun acc -> function
          | None   -> acc
          | Some x -> x :: acc
        ) [] status
    in
    let status = Task.status status |> to_str Task.json_status in
    (* FIXME: because of https://github.com/mirage/irmin/issues/272  *)
    begin Store.read (t.t "") (status_p id) >|= function
      | None    -> true
      | Some s -> s <> status
    end >>= function
    | true  -> Store.update (mk t "update task status" id) (status_p id) status
    | false -> Lwt.return_unit

  let status t id =
    Store.read (mk t "task status" id) (status_p id) >|= function
    | None   -> None
    | Some s -> Some (of_str Task.json_status s)

  let watch_status t id f =
    Store.watch_key t (fmt "watch task status" id) (status_p id) (function
        | `Updated (_, (_, s))
        | `Added (_, s) -> f (Some (of_str Task.json_status s))
        | `Removed _    -> f None
      )

  let watch t f =
    let mk v = of_str Task.json v in
    Store.watch t "watch taks" root (function
        | `Added v   -> f (mk v)
        | `Removed _ -> Lwt.return_unit
      )

end

module XWorker = struct

  let root = ["workers"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"
  let tick_p id = path id / "tick"
  let status_p id = path id / "status"

  let list t =
    Store.list (t.t "list workers") root >|= List.map (Id.of_string `Worker)

  let mem t id = Store.mem (mk t "mem worker" id) (value_p id)

  let add t w =
    let id = Worker.id w in
    debug "add: worker %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      let w = to_str Worker.json w in
      with_transaction t (fmt "publish worker" id) (fun t ->
          Store.update (t.t "") (value_p id) w >>= fun () ->
          Store.update (t.t "") (status_p id) (to_str Worker.json_status `Idle)
        ) >|= fun () ->
      debug "add: worker %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "get worker" id) (value_p id) >|=
    of_str Worker.json

  let forget t id =
    debug "forget worker:%a" Id.pp id;
    Store.rmdir (mk t "forget worker" id) (path id)

  let tick t id f =
    Store.update (mk t "tick worker" id) (tick_p id) (string_of_float f)

  let status t id =
    Store.read (mk t "worker status" id) (status_p id) >|= function
    | None   -> None
    | Some s -> Some (of_str Worker.json_status s)

  let start t id status =
    let status_s = to_str Worker.json_status status in
    let msg = match status with
      | `Idle -> "worker is idle"
      | _     -> "worker starts " ^ status_s
    in
    Store.update (mk t msg id) (status_p id) status_s

  let start_job t id jid = start t id (`Job jid)
  let start_task t id tid = start t id (`Task tid)
  let idle t id = start t id `Idle

  let watch_ticks t id f =
    Store.watch_key t (fmt "watch_ticks" id) (tick_p id) (function
        | `Updated (_, (_, tick))
        | `Added   (_, tick) -> f (float_of_string tick)
        | `Removed _         -> f 0. (* FIXME: ? *)
      )

  let watch_status t id f =
    Store.watch_key t (fmt "watch status" id) (status_p id) (function
        | `Updated (_, (_, s))
        | `Added (_, s) -> f (Some (of_str Worker.json_status s))
        | `Removed _    -> f None
      )

  type diff = [`Added of Worker.t | `Removed of Worker.id]

  let watch t f =
    Store.watch t "watch workers" root (function
        | `Added v    -> f (`Added (of_str Worker.json v))
        | `Removed id -> f (`Removed (Id.of_string `Worker id))
      )

end

module Task = XTask
module Job = XJob
module Source = XSource
module Worker = XWorker

let cancel_all_watches t = Lwt_list.iter_p (fun (_, t) -> t ()) t.cancels
let nb_watches t = List.length t.cancels
