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
include Ciso_common

let package_c: Package.t Arg.converter =
  let parse str = `Ok (Package.of_string str) in
  let print ppf t = Package.pp ppf t in
  parse, print

let repo_c: Task.repo Arg.converter =
  let parse str =
    let name = String.sub (Id.digest `Repo str |> Id.to_string) 0 8 in
    `Ok (name, Uri.of_string str)
  in
  parse, Task.pp_repo

let pin_c: Task.pin Arg.converter =
  let parse str =
    let r = match Stringext.cut str ~on:":" with
      | Some (n, r) -> n, Some (Uri.of_string r)
      | None        -> str, None
    in `Ok r
  in
  parse, Task.pp_pin

let rev_deps_c: Task.rev_deps Arg.converter =
  let parse str =
    let r = match Stringext.split str ~on:',' with
      | []    -> `None
      | ["*"] -> `All
      | l     -> `Packages (List.map Package.of_string l)
    in `Ok r
  in
  parse, Task.pp_rev_deps

let packages =
  let doc = "The package to install" in
  Arg.(value & pos_all package_c [] & info [] ~docv:"PKGS" ~doc)

let rev_deps =
  let doc =
    "Disabled by default, use '*' to test every dependent packages allowed by \
     the constraints. If you want to test only specific dependent packages, \
     they may be provided in a comma-separated list."
  in
  Arg.(value & opt rev_deps_c `None & info ["rev-deps"] ~doc)

let base_repo =
  let doc = "Specify the repository to Initialize OPAM with." in
  Arg.(value & opt repo_c Task.default_repo & info ["base-repo"] ~doc)

let extra_repos =
  let doc =
    "In addition to changing the initial base repositories (see \
     $(i, --base-repo)) additional OPAM repositories can be layered on top of \
     the base repository and be used to build the OPAM universe that will be \
     loaded by the solver."
  in
  Arg.(value & opt (list repo_c) [] & info ["repos"] ~docv:"REPOS" ~doc)

let pins =
  let doc = "Specify pinned packages." in
  Arg.(value & opt (list pin_c) [] & info ["pins"] ~docv:"PKGS" ~doc)

let to_option = function
  | [] -> None
  | l  -> Some l

let main =
  let master store packages base_repo extra_repos pins rev_deps =
    if rev_deps <> `None then
      info "rev-deps" Fmt.(to_to_string Task.pp_rev_deps rev_deps);
    if packages = [] then ()
    else
      let repos = base_repo :: extra_repos in
      let pins = to_option pins in
      let task = Task.create ~rev_deps ?pins ~repos packages in
      Lwt_main.run begin
        store >>= fun store ->
        Store.Task.add store task >|= fun () ->
        Fmt.(pf stdout) "Task %a added!\n"
          Fmt.(styled `Cyan Id.pp) (Task.id task)
      end
  in
  Term.(global master $ store $ packages $ base_repo $ extra_repos
        $ pins $ rev_deps),
  term_info ~doc:"Add new tasks to CISO" "ciso-add" ~man:[`P "TODO"]

let () =
  match Term.eval main with
  | `Error _ -> exit 1
  | `Ok () | `Version | `Help -> ()
