(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let verbose = ref false

let timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  let open Unix in
  Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec

let debug ~section fmt =
  let header ppf () = Fmt.(pf ppf "[%s %s]" (timestamp ()) section) in
  Fmt.kstrf (fun str ->
      if !verbose then
        Fmt.(pf stdout "%a %s\n%!" (styled `Magenta header) () str)
    ) fmt

let show_block =
  let pp_field ppf (i, k, v) =
    let color = if i = 0 then Fmt.(styled `Cyan) else (fun x -> x) in
    Fmt.pf ppf "@[%a: @[<4>%a@]@]@."
      Fmt.(styled `Bold string) k
      Fmt.(color (list ~sep:sp string)) v
  in
  let pp_fields ppf block =
    let block = List.filter (fun (_, l) -> l <> []) block in
    let block = List.mapi (fun i (k, v) -> i, k, v) block in
    List.iter (pp_field ppf) block
  in
  fun ppf block -> Fmt.pf ppf "@[<v>%a@]" pp_fields block
