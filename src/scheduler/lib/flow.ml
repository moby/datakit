(*
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

type t = {
  inputs: (Source.t * Cmd.t) list;
  deps  : (Cmd.t * Cmd.t) list;
}

let inputs t = t.inputs
let deps t = t.deps
let create ~inputs ~deps = { inputs; deps }

let json_pair kind json_input json_output =
  let o = Jsont.objc ~kind () in
  let input = Jsont.mem o "input"  json_input in
  let output = Jsont.mem o "output" json_output in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get input o, Jsont.get output o) in
  let enc (n, u) = Jsont.(new_obj c [memv input n; memv output u]) in
  Jsont.view (dec, enc) c

let json_inputs = Jsont.array (json_pair "input" Source.json Cmd.json)
let json_deps   = Jsont.array (json_pair "edge" Cmd.json Cmd.json)

let json =
  let o = Jsont.objc ~kind:"flow" () in
  let inputs = Jsont.mem o "inputs" json_inputs in
  let deps = Jsont.mem o "deps" json_deps in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok { inputs = get inputs; deps = get deps }
  in
    let enc t =
    Jsont.(new_obj c [memv inputs t.inputs; memv deps t.deps])
  in
  Jsont.view (dec, enc) c

let pp_input ppf (src, dst) = Fmt.pf ppf "%a => %a" Source.pp src Cmd.pp dst
let pp_dep ppf (src, dst) = Fmt.pf ppf "%a => %a" Cmd.pp src Cmd.pp dst

let pp ppf t =
  let mk = Fmt.to_to_string in
  let mks pp = List.map (mk pp) in
  let block = [
    "inputs", mks pp_input t.inputs;
    "dpes  ", mks pp_dep t.deps;
  ] in
  Gol.show_block ppf block

let fetch_input ~dst (i, n) =
  let i = Source.fetch ~dst i in
  (i, n)

let fetch_inputs ~dst = List.map (fetch_input ~dst)
let fetch ~dst t = { t with inputs = fetch_inputs ~dst t.inputs }
