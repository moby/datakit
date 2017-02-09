#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let includes = function
  | "datakit-ci" -> ["ci"]
  | "datakit" -> ["src"; "src/datakit"]
  | "datakit-client" -> ["src"; "src/datakit-client"]
  | "datakit-server" -> ["src"; "src/datakit-server"]
  | "datakit-github" -> ["src/datakit"]
  | "datakit-bridge-local-git" -> ["bridge/local"]
  | "datakit-bridge-github"    -> ["src"; "bridge/github"]
  | x -> failwith ("Unknown includes for package: " ^ x)


let extra_deps c =
  let tests = Conf.build_tests c in
  match Conf.pkg_name c with
  | "datakit-ci" -> []
  | "datakit" ->
    ["datakit-server.vfs"; "datakit-server.fs9p"]
    @ if tests then ["datakit-client"] else []
  | "datakit-client" -> []
  | "datakit-server" -> []
  | "datakit-github" -> ["datakit-client"]
  | "datakit-bridge-local-git" -> ["datakit-github"; "datakit-client"]
  | "datakit-bridge-github"    ->
    ["datakit-client"; "datakit-server.vfs"; "datakit-server.fs9p";
     "datakit-github"] @ if tests then ["datakit.ivfs"] else []
  | x -> failwith ("Unknown includes for package: " ^ x)

let build =
  let cmd c os =
    let includes = match includes (Conf.pkg_name c) with
      | [] -> Cmd.empty
      | is -> Cmd.(v "-Is" % String.concat "," is)
    in
    let extra_deps = match extra_deps c with
      | [] -> Cmd.empty
      | ed -> Cmd.(v "-package" % String.concat "," ed)
    in
    Cmd.(Pkg.build_cmd c os %% includes %% extra_deps)
  in
  let cmd c os files = OS.Cmd.run @@ Cmd.(cmd c os %% of_list files) in
  Pkg.build ~cmd ()

let metas = List.map (Pkg.meta_file ~install:false) [
    "pkg/META";
    "pkg/META.client";
    "pkg/META.server";
    "pkg/META.github";
    "pkg/META.ci";
  ]

let opams = List.map (Pkg.opam_file ~lint_deps_excluding:None ~install:false) [
    "opam";
    "datakit-client.opam";
    "datakit-server.opam";
    "datakit-github.opam";
    "datakit-ci.opam";
    "datakit-bridge-github.opam";
    "datakit-bridge-local-git.opam"
  ]

let () =
  Pkg.describe ~opams ~metas ~build "datakit" @@ fun c ->
  match Conf.pkg_name c with
  | "datakit" -> Ok [
      Pkg.lib   "pkg/META";
      Pkg.lib   "opam";
      Pkg.mllib "src/datakit/ivfs.mllib";
      Pkg.bin   "src/datakit/main" ~dst:"datakit";
      Pkg.bin   "src/datakit-client/mount" ~dst:"datakit-mount" ;
      Pkg.test  "tests/test" ~args:(Cmd.v "-q");
      Pkg.test  "examples/ocaml-client/example" ~run:false;
    ]
  | "datakit-client" -> Ok [
      Pkg.lib   "pkg/META.client"     ~dst:"META";
      Pkg.lib   "datakit-client.opam" ~dst:"opam";
      Pkg.mllib "src/datakit-client/datakit-client.mllib";
      Pkg.lib   "src/datakit-client/datakit_S.mli";
      Pkg.lib   "src/datakit-client/datakit_S.cmi";
      Pkg.bin   "src/datakit-client/mount" ~dst:"datakit-mount" ;
    ]
  | "datakit-server" -> Ok [
      Pkg.lib   "pkg/META.server"     ~dst:"META";
      Pkg.lib   "datakit-server.opam" ~dst:"opam";
      Pkg.mllib "src/datakit-server/vfs.mllib";
      Pkg.mllib "src/datakit-server/fs9p.mllib";
    ]
  | "datakit-github" -> Ok [
      Pkg.lib   "pkg/META.github"     ~dst:"META";
      Pkg.lib   "datakit-github.opam" ~dst:"opam";
      Pkg.mllib "src/datakit-github/datakit-github.mllib";
    ]
  | "datakit-bridge-local-git" -> Ok [
      Pkg.bin   "bridge/local/main" ~dst:"datakit-bridge-local-git" ;
    ]
  | "datakit-bridge-github" -> Ok [
      Pkg.bin   "bridge/github/main" ~dst:"datakit-bridge-github";
      Pkg.test  "tests/test_github" ~args:(Cmd.v "-q");
    ]
  | "datakit-ci" -> Ok [
      Pkg.lib   "pkg/META.ci"     ~dst:"META";
      Pkg.lib   "datakit-ci.opam" ~dst:"opam";
      Pkg.mllib ~api:["Datakit_ci"] "ci/src/datakit-ci.mllib";
      Pkg.test  "ci/tests/test_ci" ~args:(Cmd.v "-q");
      Pkg.test  "ci/tests/exampleCI" ~run:false;
    ]
  | other -> R.error_msgf "unknown package name: %s" other
