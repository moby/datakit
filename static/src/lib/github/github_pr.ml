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

type pr = {
    pull_num : int;
    repo_url : string;
    base_sha : string;
    head_sha : string;
}

type t =
  | Github of string * string option * pr
  (* package name * version * pull *)

let make_pull num url base head = {
  pull_num = num;
  repo_url = url;
  base_sha = base;
  head_sha = head;
}

let make_gh_task ~name ?version pr = Github (name, version, pr)

(*

let user = "ocaml"
let repo = "opam-repository"
let token = ref None

let init_gh_token name =
  Github_cookie_jar.init ()
  >>= fun jar -> Github_cookie_jar.get jar ~name
  >>= function
  | Some auth -> Lwt.return (Github.Token.of_auth auth)
  | None -> err "None auth"

(* /packages/<pkg>/<pkg.version>/{opam, url, descr, files/.., etc} *)
let packages_of_pull token num =
  let open Github.Monad in
  Github.Pull.files ~token ~user ~repo ~num () |> Github.Stream.to_list
  >|= fun files ->
  List.fold_left (fun acc file ->
      let parts = Array.of_list
          (Str.split (Str.regexp "/") file.Github_t.file_filename) in
      let pkg = try
          if parts.(0) = "packages" && parts.(3) <> "descr"
          then parts.(2) else ""
        with _ -> "" in
      if pkg <> "" && not (List.mem pkg acc) then pkg :: acc else acc)
    [] files

let pull_info token num =
  let open Github_t in
  let open Github.Monad in
  Github.Pull.get ~token ~user ~repo ~num ()
  >|= fun pull_resp ->
  let pull = Github.Response.value pull_resp in
  let base = pull.pull_base and head = pull.pull_head in
  let base_repo =
    match base.branch_repo with Some repo -> repo | None -> failwith "pr_info"
  in
  Task.make_pull
    num base_repo.repository_clone_url base.branch_sha head.branch_sha

let resolve_and_add s ?pull pkg =
  let action_graph = Ci_opam.resolve [pkg] in
  let jobs = Ci_opam.jobs_of_graph ?pull action_graph in
  update_tables s jobs >|= fun () ->
  let r, sum = count_runnables () in
  debug "resolve %d/%d jobs" r sum

let github_hook s num =
  (match !token with
   | Some t -> Lwt.return t
   | None -> begin
       init_gh_token "scry"
       >>= fun t ->
       token := Some t;
       Lwt.return t
     end)
  >>= fun token -> Github.Monad.run (pull_info token num)
  >>= fun pull -> Github.Monad.run (packages_of_pull token num)
  >>= fun pkgs -> Lwt_list.iter_s (resolve_and_add s ~pull) pkgs

*)
