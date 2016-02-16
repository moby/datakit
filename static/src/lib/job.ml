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
  switch  : Switch.t;                       (* switch on which to run the job *)
  host    : Host.t;                           (* host on which to run the job *)
  packages: Package.meta list;(* the list of metadata anout packages to build *)
}

let equal x y = Id.equal x.id y.id
let compare x y = Id.compare x.id y.id

let json =
  let o = Jsont.objc ~kind:"job" () in
  let id = Jsont.mem o "id" Id.json in
  let inputs = Jsont.(mem ~opt:`Yes_rem o "inputs" @@ array Id.json) in
  let switch = Jsont.(mem o "switch" Switch.json) in
  let host = Jsont.(mem o "host" Host.json) in
  let packages = Jsont.(mem o "packages" @@ array Package.json_meta) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok {
      id = get id; inputs = get inputs; switch = get switch;
      host = get host; packages = get packages
    } in
  let enc t =
    Jsont.(new_obj c [
        memv id t.id; memv inputs t.inputs;
        memv switch t.switch; memv host t.host;
        memv packages t.packages])
  in
  Jsont.view (dec, enc) c

let pp ppf t =
  let mk = Fmt.to_to_string in
  let mks pp = List.map (mk pp) in
  let short id = String.sub id 0 8 in
  let shorts ids = List.map short ids in
  let block = [
    "id      ", [Id.to_string t.id];
    "inputs  ", shorts @@ mks Id.pp t.inputs;
    "switch  ", [mk Switch.pp t.switch];
    "host    ", [Host.short t.host];
    "packages", mks Package.pp @@ List.map Package.pkg t.packages;
  ] in
  Gol.show_block ppf block

let id t = t.id
let inputs t = t.inputs
let switch t = t.switch
let host t = t.host
let packages t = t.packages

let digest buf = Cstruct.to_string (Nocrypto.Hash.SHA1.digest buf)

let hash ~host ~inputs ~switch ~packages =
  let x l = String.concat "+" (List.sort String.compare l) in
  let y   = String.concat "-" in
  let switches = [Fmt.to_to_string Switch.pp switch] in
  let hosts = [Fmt.to_to_string Host.pp host] in
  let inputs = List.map Id.to_string inputs in
  let packages =
    List.map (fun m ->
        let p = Package.to_string (Package.pkg m) in
        let mk f = digest (f m) in
        let mko f = match f m with
          | None   -> []
          | Some k -> [digest k]
        in
        let mkf (f, c) = f ^ digest c in
        let files = List.map mkf (Package.files m) in
        y (p :: mk Package.opam :: mko Package.url @ files)
      ) packages
  in
  let str = y [x switches; x hosts; x packages; x inputs] in
  Id.digest `Job str

let create ?(inputs=[]) host switch packages =
  let id = hash ~host ~inputs ~switch ~packages in
  { id; inputs; switch; host; packages; }

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

(* FIXME: code duplication with Task.json_{params,status} *)
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

let is_success = function `Complete `Success -> true | _ -> false
let is_failure = function `Complete `Failure -> true | _ -> false
let is_cancelled = function `Cancelled -> true | _ -> false

let task_status = function
  | [] -> `New
  | l  ->
    if List.for_all is_success l then `Complete `Success
    else if List.exists is_failure l then `Complete `Failure
    else if List.exists is_cancelled l then `Cancelled
    else `Pending

(* FIXME: code duplication with task.pp_status *)

let pp_s ppf = Fmt.of_to_string to_string ppf

let pp_status ppf = function
  | `Dispatched (w, s) -> Fmt.pf ppf "dispatched to %a (%a)" Id.pp w pp_s s
  | `Complete s -> Fmt.pf ppf "complete: %a" pp_s s
  | #core as  x -> Fmt.of_to_string to_string  ppf x
