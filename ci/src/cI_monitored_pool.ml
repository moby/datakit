open Astring

type t = {
  label: string;
  capacity: int;
  mutable active: int;
  pool: unit Lwt_pool.t;
  mutable users : (string * CI_live_log.t option) list;
}

let registered_pools = ref String.Map.empty

let create label capacity =
  let pool = Lwt_pool.create capacity Lwt.return in
  let t = { label; capacity; active = 0; pool; users = [] } in
  assert (not (String.Map.mem label !registered_pools));
  registered_pools := String.Map.add label t !registered_pools;
  t

let rec remove_first msg = function
  | [] -> assert false
  | (m,_)::xs when m = msg -> xs
  | x::xs -> x :: remove_first msg xs

let use ?log t ~reason fn =
  Lwt_pool.use t.pool
    (fun v ->
       t.active <- t.active + 1;
       t.users <- (reason, log) :: t.users;
       Lwt.finalize
         (fun () -> fn v)
         (fun () ->
            t.active <- t.active - 1;
            t.users <- remove_first reason t.users;
            Lwt.return_unit)
    )

let use t ~reason ?log fn =
  match log with
  | None -> use ?log t ~reason fn
  | Some log ->
    let msg = Fmt.strf "Waiting for resource in %S" t.label in
    CI_live_log.enter_with_pending_reason log msg (use ~log t ~reason) fn

let active t = t.active
let capacity t = t.capacity
let pools () = !registered_pools
let users t = t.users
