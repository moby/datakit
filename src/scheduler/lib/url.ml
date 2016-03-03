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

let (/) = Filename.concat

type version_control = [ `Git | `Darcs | `Hg ]

type backend = [ `Http | `Rsync | version_control ]

type t = {
  transport: string;
  path: string;
  hash: string option;
  backend: backend;
}

let split_url =
  let re =
    let (@@) f x = f x in
    Re.(compile @@ seq [
        bos;
        opt @@ seq [
          opt @@ seq [ group @@ rep @@ diff any (set "+:");
                       alt [ char '+'; str "://"] ];
          group @@ rep @@ diff any (char ':');
          str "://"
        ];
        group @@ seq [
          non_greedy @@ rep @@ diff any (char '#');
          opt @@ seq [ char '.'; group @@ rep1 @@ diff any (set ".#")]
        ];
        opt @@ seq [ char '#'; group @@ rep any ];
        eos;
      ])
  in
  fun u ->
    match Re.get_all (Re.exec re u) with
    | [| _; vc; transport; path; suffix; hash |] ->
      let opt = function "" -> None | s -> Some s in
      opt vc, opt transport, path, opt suffix, opt hash
    | _ -> assert false

let vc_of_string = function
  | "git" -> `Git
  | "hg" -> `Hg
  | "darcs" -> `Darcs
  | x -> failwith (Printf.sprintf "Unsupported version control system %S" x)

let string_of_vc = function
  | `Git   -> "git"
  | `Darcs -> "darcs"
  | `Hg    -> "hg"

let string_of_backend = function
  | `Http  -> "http"
  | `Rsync -> "rsync"
  | #version_control as vc -> string_of_vc vc

let backend_of_string = function
  | "http" | "https" | "ftp" | "wget" | "curl" -> `Http
  | "file" -> `Rsync
  | "git" -> `Git
  | "darcs" -> `Darcs
  | "hg" -> `Hg
  | "path" | "local" | "rsync" | "ssh" | "scp" | "sftp" -> `Rsync
  | p -> failwith (Printf.sprintf "Unsupported protocol %S" p)


let looks_like_ssh_path =
  (* ':' before any '/' : assume ssh, like git does. Exception for 'x:' with
     single char, because Windows *)
  let re =
    Re.(compile @@ seq [
        group @@ repn (diff any (set "/:")) 2 None;
        char ':';
        opt @@ group @@ seq [
          alt [
            diff any digit;
            seq [rep digit; compl [digit; char '/']]
          ];
          rep any;
        ];
        eos;
      ])
  in
  fun path ->
    try
      let sub = Re.exec re path in
      Some (Re.get sub 1 ^ try "/" ^ Re.get sub 2 with Not_found -> "")
    with Not_found -> None

let parse ?backend ?(handle_suffix=true) s =
  let vc, transport, path, suffix, hash = split_url s in
  let backend =
    match backend with
    | Some b -> b
    | None ->
      match vc with
      | Some vc -> vc_of_string vc
      | None ->
        let of_suffix ~default =
          if not handle_suffix then default else
          match suffix with
          | None -> default
          | Some sf -> try vc_of_string sf with Failure _ -> default
        in
        match transport with
        | None -> of_suffix ~default:`Rsync
        | Some tr ->
          try vc_of_string tr with Failure _ ->
            of_suffix ~default:(backend_of_string tr)
  in
  let transport, path =
    match backend, transport, looks_like_ssh_path path with
    | `Http, None, _ ->
      "http", path
    | _, (None | Some ("git"|"hg"|"darcs")), Some path ->
      "ssh", path
    | _, (None | Some ("hg"|"darcs")), None ->
      "file", Exec.real_path path
    | _, Some tr, _ ->
      tr, path
  in
  {
    transport;
    path;
    hash;
    backend;
  }

let of_string url = parse ~handle_suffix:false url

let to_string url =
  let hash = match url.hash with Some h -> "#" ^ h | None -> "" in
  match url.backend with
  | #version_control as vc ->
    let vc = string_of_backend vc in
    if url.transport = vc then (* Don't be redundant on e.g git:// protocols *)
      Printf.sprintf "%s://%s%s" vc url.path hash
    else
      Printf.sprintf "%s+%s://%s%s" vc url.transport url.path hash
  | `Rsync | `Http ->
    Printf.sprintf "%s://%s%s" url.transport url.path hash

let base_url url =
  match url.transport with
  | "" -> url.path
  | tr -> Printf.sprintf "%s://%s" tr url.path

let guess_version_control s =
  let vc,transport,path,_,_ = split_url s in
  if vc = None && transport = None && looks_like_ssh_path path = None then
    if Exec.exists_dir (path / ".git") then Some`Git else
    if Exec.exists_dir (path / ".hg") then Some `Hg else
    if Exec.exists_dir (path / "_darcs") then Some `Darcs else
      None
  else
    None

let basename =
  let re =
    Re.(compile @@ seq [
        opt @@ seq [rep any; char '/'];
        group @@ rep1 (diff any (char '/'));
        rep @@ char '/';
      ])
  in
  fun t ->
    try
      Re.get (Re.exec re t.path) 1
    with Not_found -> ""

let has_trailing_slash { path; _ } =
  String.length path > 0 && path.[String.length path - 1] = '/'
