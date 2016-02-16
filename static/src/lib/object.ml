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

type id = [`Object] Id.t

type kind = [ `Archive | `File ]

type archive = {
  files: (string * Digest.t) list;
  raw  : Cstruct.t;
}

let pp_file ppf (f, d) = Fmt.pf ppf "%s %s" f (Digest.to_hex d)
let pp_archive ppf t = Fmt.(pf ppf "[@[<v>files: %a@]]" (list pp_file) t.files)

let json_digest =
  let dec o = `Ok (Digest.from_hex o) in
  let enc s = Digest.to_hex s in
  Jsont.view ~default:(Digest.string "") (dec, enc) Jsont.string

let json_file_d =
  let o = Jsont.objc ~kind:"file" () in
  let name = Jsont.mem o "name" Jsont.string in
  let digest  = Jsont.mem o "digest" json_digest in
  let c = Jsont.obj ~seal:true o in
  let dec o =`Ok  (Jsont.get name o, Jsont.get digest o) in
  let enc (n, d) = Jsont.(new_obj c [memv name n; memv digest d]) in
  Jsont.view (dec, enc) c

(* FIXME: it's probably not a good idea to do that. *)
let json_cstruct =
  let dec o = `Ok (Cstruct.of_string (Hex.to_string (`Hex o))) in
  let enc c = let `Hex h = Hex.of_cstruct c in h in
  Jsont.view (dec, enc) Jsont.nat_string

let json_archive =
  let o = Jsont.objc ~kind:"archive" () in
  let files = Jsont.(mem o "files" @@ array json_file_d) in
  let raw  = Jsont.mem o "raw" json_cstruct in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok { files = Jsont.get files o; raw = Jsont.get raw o } in
  let enc a = Jsont.(new_obj c [memv files a.files; memv raw a.raw]) in
  Jsont.view (dec, enc) c

type file = string * Cstruct.t

let pp_file ppf (p, _) = Fmt.string ppf p

let json_file =
  let o = Jsont.objc ~kind:"archive" () in
  let name = Jsont.(mem o "name" string) in
  let raw  = Jsont.(mem o "contents" string) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get name o, Cstruct.of_string (Jsont.get raw o)) in
  let enc (n, r) =
    Jsont.(new_obj c [memv name n; memv raw (Cstruct.to_string r)])
  in
  Jsont.view (dec, enc) c

type contents = Archive of archive | File of file

let pp_contents ppf = function
  | Archive a -> pp_archive ppf a
  | File f    -> pp_file ppf f

let json_contents =
  let o = Jsont.objc ~kind:"contents" () in
  let archive = Jsont.(mem_opt o "archive" json_archive) in
  let file = Jsont.(mem_opt o "file" json_file) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get f = Jsont.get f o in
    match get archive, get file with
    | Some a, None -> `Ok (Archive a)
    | None, Some f -> `Ok (File f)
    | _ -> `Error "json_contents"
  in
  let enc t =
    Jsont.(new_obj c [match t with
        | Archive a -> memv archive (Some a)
        | File f    -> memv file (Some f)
      ]) in
  let default = File ("", Cstruct.of_string "") in
  Jsont.view ~default (dec, enc) c

type t = { id : id; contents: contents; }

let equal x y = Id.equal x.id y.id

let pp ppf t = Fmt.pf ppf
    "@[<v>\
     id:       %a@;\
     contents: %a@@]"
    Id.pp t.id
    pp_contents t.contents

let json =
  let o = Jsont.objc ~kind:"object" () in
  let id = Jsont.(mem o "id" Id.json) in
  let contents = Jsont.(mem o "contents" json_contents) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok { id = Jsont.get id o; contents = Jsont.get contents o } in
  let enc t = Jsont.(new_obj c [memv id t.id; memv contents t.contents]) in
  Jsont.view (dec, enc) c

let id t = t.id
let contents t = t.contents

let kind t = match t.contents with
  | Archive _ -> `Archive
  | File _    -> `File

let hash k =
  let l = match k with
    | `File (n, r) -> [n; Cstruct.to_string r]
    | `Files files ->
      List.map (fun (f, d) -> f ^ ":" ^ Digest.to_hex d) files
      |> List.sort String.compare
  in
  Id.digest `Object (String.concat "+" l)

let archive files raw =
  let id = hash (`Files files) in
  { id; contents = Archive { files; raw } }

let file name raw =
  let id = hash (`File (name, raw)) in
  { id; contents = File (name, raw) }
