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

type t = {
  name: string;
  version: string option;
}

let equal x y =
  String.compare x.name y.name = 0
  && match x.version, y.version with
  | None  , None   -> true
  | Some x, Some y -> String.compare x y = 0
  | _ -> false

let compare x y =
  match String.compare x.name y.name with
  | 0 -> begin
      match x.version, y.version with
      | None  , None   -> 0
      | Some _, None   -> 1
      | None  , Some _ -> -1
      | Some x, Some y -> String.compare x y
    end
  | i -> i

let pp ppf t = match t.version with
  | None   -> Fmt.string ppf t.name
  | Some v -> Fmt.pf ppf "%s.%s" t.name v

let json =
  let o = Jsont.objc ~kind:"package" () in
  let name = Jsont.(mem o "name" string) in
  let version = Jsont.(mem_opt o "version" string) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok { name = Jsont.get name o; version = Jsont.get version o } in
  let enc t = Jsont.(new_obj c [memv name t.name; memv version t.version]) in
  Jsont.view (dec, enc) c

let name t = t.name
let version t = t.version
let create ?version name = { name; version }

let of_string s = match Stringext.cut s ~on:"." with
  | None        -> create s
  | Some (n, v) -> create ~version:v n

let to_string t = match t.version with
  | None   -> t.name
  | Some v -> t.name ^ "." ^ v

type meta = {
  pkg  : t;
  opam : Cstruct.t;
  descr: Cstruct.t option;
  url  : Cstruct.t option;
  files: (string * Cstruct.t) list;
}

let pp_meta ppf t =
  let mk x = Id.digest_cstruct `Foo x |> Id.to_string in
  let mko = function None -> [] | Some x -> [mk x] in
  let mkf (f, c) = f ^ ":" ^ mk c in
  let block = [
    "package", [to_string t.pkg];
    "opam   ", [mk t.opam];
    "descr  ", mko t.descr;
    "url    ", mko t.url;
    "files  ", List.map mkf t.files
  ] in
  Gol.show_block ppf block

let json_cstruct =
  let dec o = `Ok (Cstruct.of_string o) in
  let enc c = Cstruct.to_string c in
  Jsont.view (dec, enc) Jsont.nat_string

let json_file =
  let o = Jsont.objc ~kind:"file" () in
  let name = Jsont.(mem o "name" string) in
  let contents = Jsont.(mem o "contents" json_cstruct) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get name o, Jsont.get contents o) in
  let enc (n, x) = Jsont.(new_obj c [memv name n; memv contents x]) in
  Jsont.view (dec, enc) c

let json_meta =
  let o = Jsont.objc ~kind:"meta" () in
  let pkg = Jsont.(mem o) "package" json in
  let opam = Jsont.(mem o "opam" json_cstruct) in
  let descr = Jsont.(mem_opt o "descr" json_cstruct) in
  let url = Jsont.(mem_opt o "url" json_cstruct) in
  let files = Jsont.(mem ~opt:`Yes_rem o "files" @@ array json_file) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get x = Jsont.get x o in
    let t =
      { pkg = get pkg; opam = get opam; url = get url;
        descr = get descr; files = get files; }
    in
    `Ok t
  in
  let enc t = Jsont.(new_obj c [
      memv pkg t.pkg;
      memv opam t.opam;
      memv descr t.descr;
      memv url t.url;
      memv files t.files;
    ]) in
  Jsont.view (dec, enc) c

let meta ~opam ?descr ?url ?(files=[]) pkg = { pkg; opam; descr; url; files }

let pkg m = m.pkg
let opam m = m.opam
let url m = m.url
let descr m = m.descr
let files m = m.files
