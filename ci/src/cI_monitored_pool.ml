open Astring

module Metrics = struct
  open CI_prometheus

  let namespace = "DataKitCI"
  let subsystem = "pool"

  let qlen =
    let help = "Number of users waiting for a resource" in
    let family = Gauge.v_labels ~help ~label_names:[|"name"|] ~namespace ~subsystem "qlen" in
    fun name -> Gauge.labels family [|name|]

  let wait_time =
    let help = "Time spent waiting for a resource" in
    let family = Summary.v_labels ~help ~label_names:[|"name"|] ~namespace ~subsystem "wait_time_seconds" in
    fun name -> Summary.labels family [|name|]

  let use_time =
    let help = "Time spent using a resource" in
    let family = Summary.v_labels ~help ~label_names:[|"name"|] ~namespace ~subsystem "use_time_seconds" in
    fun name -> Summary.labels family [|name|]
end

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
  let qlen = Metrics.qlen t.label in
  CI_prometheus.Gauge.inc_one qlen;
  let start_wait = Unix.gettimeofday () in
  Lwt_pool.use t.pool
    (fun v ->
       CI_prometheus.Gauge.dec_one qlen;
       let stop_wait = Unix.gettimeofday () in
       CI_prometheus.Summary.observe (Metrics.wait_time t.label) (stop_wait -. start_wait);
       t.active <- t.active + 1;
       t.users <- (reason, log) :: t.users;
       Lwt.finalize
         (fun () -> fn v)
         (fun () ->
            let stop_use = Unix.gettimeofday () in
            CI_prometheus.Summary.observe (Metrics.use_time t.label) (stop_use -. stop_wait);
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
