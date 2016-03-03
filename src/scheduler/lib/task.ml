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

type id = [`Task] Id.t

type t = {
  id  : id;
  date: float;
  flow: Flow.t;
}

let equal x y = Id.equal x.id y.id
let compare x y = Id.compare x.id y.id

let id t = t.id
let date t = t.date
let flow t = t.flow

let json =
  let o = Jsont.objc ~kind:"task" () in
  let id = Jsont.mem o "id" Id.json in
  let flow = Jsont.mem o "flow" Flow.json in
  let date = Jsont.(mem o "date" float) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok { id = get id; date = get date; flow = get flow; }
  in
  let enc t =
    Jsont.(new_obj c [
        memv id t.id;
        memv date t.date;
        memv flow t.flow;
      ]) in
  Jsont.view (dec, enc) c

let string_of_date f =
  let tm = Unix.localtime f in
  let open Unix in
  Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec

let pp ppf t =
  let block = [
    "id  ", [Id.to_string t.id];
    "date", [string_of_date t.date];
    "flow", [Fmt.to_to_string Flow.pp t.flow]; (* FIXME: probably wrong fmt *)
  ] in
  Gol.show_block ppf block

let hash ~date ~flow =
  let concat = String.concat "-" in
  let flow = Fmt.to_to_string Flow.pp flow in
  let date = string_of_date date in
  let str = concat [ flow; date; ] in
  Id.digest `Task str

let create flow =
  let date = Unix.gettimeofday () in
  let id = hash ~date ~flow in
  { id; date; flow }

type core = [ `New | `Pending | `Cancelled ]
type dispatch =  [`Pending | `Started]
type complete = [`Success | `Failure]

type status = [
  | core
  | `Dispatched of  [`Worker] Id.t * dispatch
  | `Complete of complete
]

let to_string = function
  | `New       -> "new"
  | `Dispatched-> "dispatched"
  | `Pending   -> "pending"
  | `Started   -> "started"
  | `Resolving -> "resolving"
  | `Complete  -> "complete"
  | `Success   -> "success"
  | `Failure   -> "failure"
  | `Cancelled -> "canceled"

let core = [`New; `Dispatched; `Pending; `Complete; `Cancelled ]
let dispatch = [ `Pending; `Started]
let complete = [`Success; `Failure]

let mk_enum status =
  let default = List.hd status in
  Jsont.enum ~default @@ List.map (fun s -> to_string s, s) status

let json_params =
  let o = Jsont.objc ~kind:"task-status-params" () in
  let worker = Jsont.(mem_opt o "worker" Id.json) in
  let status = Jsont.(mem o "status" @@ mk_enum (dispatch @ complete)) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get worker o, Jsont.get status o) in
  let enc (w, s) = Jsont.(new_obj c [memv worker w; memv status s]) in
  Jsont.view (dec, enc) c

let json_status =
  let o = Jsont.objc ~kind:"worker-status" () in
  let status = Jsont.(mem o "status" @@ mk_enum core) in
  let params = Jsont.(mem_opt o "params" json_params) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let params = match Jsont.get params o with
      | None -> `N
      | Some (Some id, (#dispatch as p)) -> `D (id, p)
      | Some (None   , (#complete as p)) -> `C p
      | _ -> `Error "task_params"
    in
    match Jsont.get status o, params with
    | `Dispatched, `D p -> `Ok (`Dispatched p)
    | `Complete  , `C p -> `Ok (`Complete p)
    | #core as x , `N   -> `Ok x
    | _ -> `Error "task_status"
  in
  let enc t =
    let cast t = (t :> [dispatch | complete]) in
    let s, i = match t with
      | `Dispatched (w, p) -> `Dispatched, Some (Some w, cast p)
      | `Complete p        -> `Complete  , Some (None  , cast p)
      | #core as x         -> x          , None
    in
    Jsont.(new_obj c [memv status s; memv params i])
  in
  Jsont.view (dec, enc) c

let pp_s ppf = Fmt.of_to_string to_string ppf

let pp_status ppf = function
  | `Dispatched (w, s) -> Fmt.pf ppf "dispatched to %a (%a)" Id.pp w pp_s s
  | `Complete s -> Fmt.pf ppf "complete: %a" pp_s s
  | #core as  x -> Fmt.of_to_string to_string  ppf x

let is_success = function `Complete `Success -> true | _ -> false
let is_failure = function `Complete `Failure -> true | _ -> false
let is_cancelled = function `Cancelled -> true | _ -> false

let status = function
  | [] -> `New
  | l  ->
    if List.for_all is_success l then `Complete `Success
    else if List.exists is_failure l then `Complete `Failure
    else if List.exists is_cancelled l then `Cancelled
    else `Pending
