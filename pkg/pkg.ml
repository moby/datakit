#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let build =
  let build_with_visible_warnings c os =
    let ocamlbuild = Conf.tool "ocamlbuild" os in
    let build_dir = Conf.build_dir c in
    let debug = Cmd.(on (Conf.debug c) (v "-tag" % "debug")) in
    let profile = Cmd.(on (Conf.profile c) (v "-tag" % "profile")) in
    Cmd.(ocamlbuild % "-use-ocamlfind" %% debug %% profile % "-build-dir" % build_dir)
  in
  let cmd c os files =
    OS.Cmd.run @@ Cmd.(build_with_visible_warnings c os %% of_list files)
  in
  Pkg.build ~cmd ()

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
  Pkg.meta_file ~install:false "pkg/META.client";
  Pkg.meta_file ~install:false "pkg/META.server";
  Pkg.meta_file ~install:false "pkg/META.github";
  Pkg.meta_file ~install:false "pkg/META.ci";
]

let opams =
  let lint_deps_excluding = None in
  let install = false in
  [
    Pkg.opam_file "opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "datakit-client.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "datakit-server.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "datakit-github.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "datakit-ci.opam" ~lint_deps_excluding ~install;
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
      Pkg.mllib "bridge/github/datakit-github.mllib";
      Pkg.bin   "bridge/github/main" ~dst:"datakit-github" ;
    ]
  | "datakit-ci" -> Ok [
      Pkg.lib   "pkg/META.ci"     ~dst:"META";
      Pkg.lib   "datakit-ci.opam" ~dst:"opam";
      Pkg.mllib "ci/src/datakit-ci.mllib";
      Pkg.test  "ci/tests/test_ci" ~args:(Cmd.v "-q");
      Pkg.test  "ci/tests/exampleCI" ~run:false;
    ]
  | other -> R.error_msgf "unknown package name: %s" other
