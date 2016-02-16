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

type id = [`Worker] Id.t

type kind = [`Job|`Task]

type t = { id: id; kind: [`Job|`Task]; host: Host.t; }

let equal x y = Id.equal x.id y.id
let compare x y = Id.compare x.id y.id

let pp_kind =
  let str = function `Job -> "job" | `Task -> "task" in
  Fmt.of_to_string str

let pp ppf t =
  let mk pp x = [Fmt.to_to_string pp x] in
  let block = [
    "id    ", mk Id.pp t.id;
    "kind  ", mk pp_kind t.kind;
    "host  ", [Host.short t.host];
  ] in
  Gol.show_block ppf block

let json =
  let o = Jsont.objc ~kind:"worker" () in
  let id = Jsont.(mem o "id" Id.json) in
  let host = Jsont.(mem o "host" Host.json) in
  let kind = Jsont.(mem o "kind" @@ enum ["job",`Job; "task",`Task]) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get x = Jsont.get x o in
    `Ok { kind = get kind; id = get id; host = get host } in
  let enc t =
    Jsont.(new_obj c [memv id t.id; memv kind t.kind; memv host t.host])
  in
  Jsont.view (dec, enc) c

let create kind host =
  let id = Id.uuid `Worker in
  { id; kind; host }

let id t = t.id
let host t = t.host
let kind t = t.kind

type status = [
  | `Idle
  | `Job of Job.id
  | `Task of Task.id
]

let pp_status ppf = function
  | `Idle   -> Fmt.string ppf "idle"
  | `Job j  -> Fmt.pf ppf "job %a" Id.pp j
  | `Task t -> Fmt.pf ppf "task %a" Id.pp t

let status = [`Idle; `Job; `Task]

let to_string = function
  | `Idle -> "idle"
  | `Job  -> "job"
  | `Task -> "task"

let status_enum =
  Jsont.enum ~default:`Idle @@ List.map (fun s -> to_string s, s) status

let json_status =
  let o = Jsont.objc ~kind:"worker-status" () in
  let status = Jsont.(mem o "status" @@ status_enum) in
  let id = Jsont.(mem_opt o "id" string) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    match Jsont.get status o, Jsont.get id o with
    | `Idle, None   -> `Ok `Idle
    | `Job , Some x -> `Ok (`Job (Id.of_string `Job x))
    | `Task, Some x -> `Ok (`Task (Id.of_string `Task x))
    | _ -> `Error "worker_status"
  in
  let enc t =
    let s, i = match t with
      | `Idle   -> `Idle, None
      | `Job x  -> `Job , Some (Id.to_string x)
      | `Task x -> `Task, Some (Id.to_string x)
    in
    Jsont.(new_obj c [memv status s; memv id i])
  in
  Jsont.view (dec, enc) c
