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

open Cmdliner
open Lwt.Infix
open Irmin_unix
open Ciso_common

module S =
  Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module Server = Irmin_http_server.Make(S)

let server local uri =
  Lwt_main.run begin
    let root = match local with
      | Some r -> Lwt.return r
      | None   ->
        config_file () >|= fun config ->
        match config "local" with
        | Some r -> r
        | None   -> err "no store specified!"; exit 1
    in
    root >>= fun root ->
    let config = Irmin_git.config ~root ~bare:true () in
    S.Repo.create config >>= S.master Irmin_unix.task >>= fun t ->
    let callback = Server.http_spec (t "start server") in
    let port = match Uri.port (Uri.of_string uri) with
      | None   -> 80
      | Some p -> p
    in
    Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port)) callback
  end

let uri =
  let doc = "Local URI where the database will be published." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URI")

let main () =
  Term.(pure server $ local $ uri),
  term_info ~doc:"Publish the database over HTTP" "ciso-publish"

let () = match Term.eval (main ()) with
  | `Error _ -> exit 1
  | `Help | `Ok _ | `Version -> ()
