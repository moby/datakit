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

type id = [`Source] Id.t

type digest = [ `SHA1 of string ]

type t = {
  id    : id;
  name  : string;
  url   : Url.t;
}

let id t = t.id
let name t = t.name
let url t = t.url

let json_uri =
  let dec s = `Ok (Url.of_string s) in
  let enc u = Url.to_string u in
  Jsont.(view (dec, enc) string)

let json =
  let o = Jsont.objc ~kind:"source" () in
  let id = Jsont.mem o "id" Id.json in
  let name = Jsont.mem o "name" Jsont.string in
  let uri = Jsont.mem o "uri" json_uri in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get x = Jsont.get x o in
    `Ok { id = get id; name = get name; url = get uri } in
  let enc t = Jsont.(new_obj c [
      memv id t.id;
      memv name t.name;
      memv uri t.url;
    ]) in
  Jsont.view (dec, enc) c

let pp_uri ppf x = Fmt.string ppf (Url.to_string x)

let pp_digest ppf = function
  | None           -> ()
  | Some (`SHA1 d) -> Fmt.pf ppf "#%s" d

let pp ppf t = Fmt.(pf ppf "%s:%a" t.name pp_uri t.url)
let to_string = Fmt.to_to_string pp

let hash ~name ~url =
  let y   = String.concat "-" in
  let url = Url.to_string url in
  let str = y [name; url] in
  Id.digest `Source str

let create ?digest ~url name =
  let id = hash ~name ~url in
  { id; name; url; }

let hash t = Id.hash t.id
let equal x y = Id.equal x.id y.id
let compare x y = Id.compare x.id y.id

let (/) = Filename.concat

let fetch ~dst source =
  match source.url.Url.backend with
  | `Git ->
    let repo = {
      GitSource.name = source.name;
      url = source.url;
      root = dst / source.name
    } in
    if GitSource.exists repo
    then GitSource.fetch repo
    else GitSource.init repo;
    `SHA1 (GitSource.revision repo)

  | _ -> failwith "TODO"
