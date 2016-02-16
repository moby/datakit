(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open Exec.Op

let debug fmt = Gol.debug ~section:"git" fmt
let log ppf = Fmt.(pf stdout ppf)

type t = {
  name: string;
  root: string;
  url : Url.t;
}

let exists repo = Exec.exists_dir (repo.root / ".git")

let git ?env repo fmt =
  Printf.ksprintf (fun cmd ->
      Exec.(in_dir repo.root @@ fun () -> exec ?env "git %s" cmd)
    ) fmt

let gitx ?env repo fmt =
  Printf.ksprintf (fun cmd ->
      Exec.(in_dir repo.root @@ fun () -> read_stdout ?env "git %s" cmd)
    ) fmt

let init repo =
  let env =
    Array.append (Unix.environment ()) [|
      "GIT_AUTHOR_NAME=Datakit";
      "GIT_AUTHOR_EMAIL=datakit@docker.io";
      "GIT_COMMITTER_NAME=Datakit";
      "GIT_COMMITTER_EMAIL=datakit@docker.io"
    |] in
  git ~env repo "init";
  git ~env repo "remote add origin %s" (Url.base_url repo.url);
  git ~env repo "commit --allow-empty -m datakit-init"

let remote_ref = "refs/remotes/datakit-ref"

let fetch repo =
  let check_and_fix_remote =
    let r = gitx repo "config --get remote.origin.url" in
    let current_remote = match r with
      | [url] -> Some url
      | _ -> None
    in
    if current_remote <> Some (Url.base_url repo.url) then (
      log "Git remote for %s needs updating (was: %s)" repo.name
        (match current_remote with None -> "<none>" | Some v -> v);
      git repo "remote rm origin";
      git repo "remote add origin %s" (Url.base_url repo.url);
    )
  in
  check_and_fix_remote;
  let branch = match repo.url.Url.hash with None -> "HEAD" | Some h -> h in
  let refspec = Printf.sprintf "+%s:%s" branch remote_ref in
  try git repo "fetch -q origin %s" refspec
  with Failure _ ->
    git repo "fetch -q origin";
    git repo "fetch -q origin %s" refspec

let revision repo =
  let r = gitx repo "rev-parse HEAD" in
  match r with
  | []      -> "<none>"
  | full::_ -> if String.length full > 8 then (String.sub full 0 8) else full

let reset repo = git repo "reset --hard %s --" remote_ref
