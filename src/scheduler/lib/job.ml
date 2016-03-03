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

type id = [`Job] Id.t

type t = {
  id      : id;                                                 (* the job id *)
  inputs  : id list;                 (* the transitive reduction of need jobs *)
  cmd     : Cmd.t;
}

let equal x y = Id.equal x.id y.id
let compare x y = Id.compare x.id y.id

let json =
  let o = Jsont.objc ~kind:"job" () in
  let id = Jsont.mem o "id" Id.json in
  let inputs = Jsont.(mem ~opt:`Yes_rem o "inputs" @@ array Id.json) in
  let cmd = Jsont.mem o "cmd" Cmd.json in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok { id = get id; inputs = get inputs; cmd = get cmd }
  in
  let enc t =
    Jsont.(new_obj c [memv id t.id; memv inputs t.inputs; memv cmd t.cmd])
  in
  Jsont.view (dec, enc) c

let pp ppf t =
  let mk = Fmt.to_to_string in
  let mks pp = List.map (mk pp) in
  let block = [
    "id    ", [Id.to_string t.id];
    "inputs", mks Id.pp t.inputs;
    "cmd   ", [Cmd.to_string t.cmd];
  ] in
  Gol.show_block ppf block

let id t = t.id
let cmd t = t.cmd
let inputs t = t.inputs

let hash ~cmd ~inputs =
  let x l = String.concat "+" (List.sort String.compare l) in
  let y   = String.concat "-" in
  let cmd = Cmd.to_string cmd in
  let inputs = List.map Id.to_string inputs in
  let str = y [cmd; x inputs] in
  Id.digest `Job str

let create ?(inputs=[]) cmd =
  let id = hash ~cmd ~inputs in
  { id; inputs; cmd }

type core = [ `Pending | `Runnable | `Cancelled ]
type dispatch =  [`Pending | `Started]
type complete = [`Success | `Failure]

type status = [
  | core
  | `Complete of complete
  | `Dispatched of [`Worker] Id.t * dispatch
]

let to_string = function
  | `Pending    -> "pending"
  | `Runnable   -> "runnnable"
  | `Dispatched -> "dispatched"
  | `Started    -> "started"
  | `Complete   -> "complete"
  | `Success    -> "success"
  | `Failure    -> "failure"
  | `Cancelled  -> "cancelled"

let core = [ `Pending; `Runnable; `Dispatched; `Complete; `Cancelled ]
let dispatch = [ `Pending; `Started]
let complete = [`Success; `Failure]

let mk_enum status =
  let default = List.hd status in
  Jsont.enum ~default @@ List.map (fun s -> to_string s, s) status

let json_params =
  let o = Jsont.objc ~kind:"job-status-params" () in
  let worker = Jsont.(mem_opt o "worker" Id.json) in
  let status = Jsont.(mem o "status" @@ mk_enum (dispatch @ complete)) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get worker o, Jsont.get status o) in
  let enc (w, s) = Jsont.(new_obj c [memv worker w; memv status s]) in
  Jsont.view (dec, enc) c

let json_status =
  let o = Jsont.objc ~kind:"job-status" () in
  let status = Jsont.(mem o "status" @@ mk_enum core) in
  let params = Jsont.(mem_opt o "params" json_params) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let params = match Jsont.get params o with
      | None -> `N
      | Some (Some w, (#dispatch as p)) -> `D (w, p)
      | Some (None  , (#complete as p)) -> `C p
      | _ -> `Error "task_params"
    in
    match Jsont.get status o, params with
    | `Dispatched, `D p -> `Ok (`Dispatched p)
    | `Complete  , `C p -> `Ok (`Complete p)
    | #core as x , `N   -> `Ok x
    | _ -> `Error "task_status"
  in
  let enc (t:status) =
    let cast t = (t :> [dispatch | complete]) in
    let s, i = match t with
      | `Dispatched (w, p) -> `Dispatched, Some (Some w, cast p)
      | `Complete p        -> `Complete  , Some (None  , cast p)
      | #core as x         -> x          , None
    in
    Jsont.(new_obj c [memv status s; memv params i])
  in
  Jsont.view (dec, enc) c

(* FIXME: code duplication with task.pp_status *)

let pp_s ppf = Fmt.of_to_string to_string ppf

let pp_status ppf = function
  | `Dispatched (w, s) -> Fmt.pf ppf "dispatched to %a (%a)" Id.pp w pp_s s
  | `Complete s -> Fmt.pf ppf "complete: %a" pp_s s
  | #core as  x -> Fmt.of_to_string to_string  ppf x

let hash t = Id.hash t.id
