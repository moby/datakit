open Lwt.Infix
open Astring

module Log = CI_utils.Log

type t = {
  title : string;
  buffer : Buffer.t;
  cond : unit Lwt_condition.t;          (* Fires when [buffer] changes *)
  mutable finished : bool;

  mutable pending : string list;        (* Stack: first item is current reason. *)
  mutable pending_updated : unit Lwt.t; (* Resolves when [pending] changes. *)
  mutable pending_waker : unit Lwt.u;

  branch : string;
  manager : manager;

  switch : Lwt_switch.t option;
}
and manager = t String.Map.t ref        (* Branch -> current live log *)

type stream = {
  data : string;
  next : stream option Lwt.t Lazy.t;
}

let create_manager () =
  ref String.Map.empty

let create ?switch ~pending ~branch ~title manager =
  let pending_updated, pending_waker = Lwt.wait () in
  let t = {
    title;
    buffer = Buffer.create 10000;
    cond = Lwt_condition.create ();
    pending = [pending];
    pending_updated;
    pending_waker;
    finished = false;
    branch;
    manager;
    switch;
  } in
  match String.Map.find branch !(t.manager) with
  | Some existing ->
    CI_utils.failf "Can't create new live log on branch %S; one is already active!\n%s" branch (Buffer.contents existing.buffer)
  | None ->
    manager := String.Map.add branch t !manager;
    t

let lookup ~branch manager = String.Map.find branch !manager

let branch t = t.branch
let title t = t.title

let stream t =
  let rec loop offset =
    let length = Buffer.length t.buffer in
    let send () =
      let data = Bytes.create (length - offset) in
      Buffer.blit t.buffer offset data 0 (length - offset);
      Lwt.return @@ Some { data = Bytes.unsafe_to_string data; next = lazy (loop length) }
    in
    if length = offset then (
      if t.finished then Lwt.return None
      else Lwt_condition.wait t.cond >>= send
    ) else send ()
  in
  loop 0

let write t str =
  assert (not t.finished);
  Buffer.add_string t.buffer str;
  Lwt_condition.broadcast t.cond ()

let printf t fmt =
  Fmt.kstrf (write t) fmt

let log t fmt =
  fmt |> Fmt.kstrf @@ fun msg ->
  let tm = Unix.(localtime (time ())) in
  write t Unix.(Fmt.strf "[%04d-%02d-%02d %02d:%02d.%02d] %s\n"
                  (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec msg
               )

let heading t fmt =
  "=== " ^^ fmt ^^ "\n" |> Fmt.kstrf @@ fun msg ->
  if Buffer.length t.buffer <> 0 then Buffer.add_char t.buffer '\n';
  write t msg

let contents t = Buffer.contents t.buffer

let pending t =
  let next =
    if t.finished then `Stop
    else `Continue t.pending_updated
  in
  List.hd t.pending, next

let notify_pending t =
  Log.debug (fun f -> f "notify_pending: %s" (List.hd t.pending));
  let old_waker = t.pending_waker in
  let pending_updated, pending_waker = Lwt.wait () in
  t.pending_updated <- pending_updated;
  t.pending_waker <- pending_waker;
  Lwt.wakeup old_waker ()

let rec remove_first msg = function
  | [] -> assert false
  | x::xs when x = msg -> xs
  | x::xs -> x :: remove_first msg xs

let with_pending_reason t reason fn =
  assert (not t.finished);
  let result = fn () in
  if Lwt.state result = Lwt.Sleep then (
    t.pending <- reason :: t.pending;
    notify_pending t;
    Lwt.finalize
      (fun () -> result)
      (fun () ->
         t.pending <- remove_first reason t.pending;
         notify_pending t;
         Lwt.return ()
      )
  ) else (
    result
  )

let enter_with_pending_reason t reason use fn =
  let got, set_got = Lwt.wait () in
  let done_waiting = lazy (Lwt.wakeup set_got ()) in
  Lwt.async (fun () -> with_pending_reason t reason (fun () -> got));
  Lwt.finalize
    (fun () -> use (fun x -> Lazy.force done_waiting; fn x))
    (fun () -> Lazy.force done_waiting; Lwt.return ())

let finish t =
  if not t.finished then (
    t.finished <- true;
    t.manager := String.Map.remove t.branch !(t.manager);
    Lwt_condition.broadcast t.cond ();
    notify_pending t
  )

let can_cancel t =
  match t.switch with
  | Some switch when Lwt_switch.is_on switch -> true
  | Some _ | None -> false

let cancel = function
  | {switch = None; branch; _} -> Lwt.return (Error (Fmt.strf "Branch %S cannot be cancelled" branch))
  | {switch = Some switch; _} as t ->
    log t "Cancelled at user's request";
    Lwt_switch.turn_off switch >|= fun () ->
    Ok ()
