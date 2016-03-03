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

type t = {
  name: string;
  exec: string option;
}

let nope = { name = "nope"; exec = None }
let is_nope t = nope == t

let equal x y =
  String.compare x.name y.name = 0
  && String.compare x.name y.name = 0

let compare x y =
  match String.compare x.name y.name with
  | 0 -> compare x.exec y.exec
  | i -> i

let pp ppf t =
  if is_nope t then Fmt.string ppf "nope"
  else match t.exec with
    | None   -> Fmt.string ppf t.name
    | Some e -> Fmt.pf ppf "%s: '%s'" t.name e

let to_string = Fmt.to_to_string pp

let json =
  let o = Jsont.objc ~kind:"node" () in
  let name = Jsont.(mem o "name" string) in
  let exec = Jsont.(mem_opt o "cmd" string) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok { name = Jsont.get name o; exec = Jsont.get exec o } in
  let enc t = Jsont.(new_obj c [memv name t.name; memv exec t.exec]) in
  Jsont.view (dec, enc) c

let name t = t.name

let debug fmt = Gol.debug ~section:"cmd" fmt

let exec t = match t.exec with
  | None   -> debug "+ nope"; Lwt.return `Ok
  | Some x ->
    debug "+ %s" x;
    Lwt_unix.system x >|= function
    | Lwt_unix.WEXITED 0 -> `Ok
    | s -> `Error s

let create ~exec name = { name; exec = Some exec }
let hash = Hashtbl.hash
