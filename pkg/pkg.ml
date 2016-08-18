#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
  Pkg.meta_file ~install:false "pkg/META.client";
  Pkg.meta_file ~install:false "pkg/META.github";
]

let opams =
  let lint_deps_excluding = None in
  let install = false in
  [
    Pkg.opam_file "opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "datakit-client.opam" ~lint_deps_excluding ~install;
    Pkg.opam_file "datakit-github.opam" ~lint_deps_excluding ~install;
  ]

let () =
  Pkg.describe ~opams ~metas "datakit" @@ fun c ->
  match Conf.pkg_name c with
  | "datakit" -> Ok [
      Pkg.lib   "pkg/META";
      Pkg.lib   "opam";
      Pkg.mllib "src/fs9p/fs9p.mllib"         ~dst_dir:"fs9p";
      Pkg.mllib "src/ivfs/ivfs.mllib"         ~dst_dir:"ivfs";
      Pkg.mllib "src/vfs/vfs.mllib"           ~dst_dir:"vfs";
      Pkg.bin   "src/bin/main"  ~dst:"datakit";
      Pkg.bin   "src/bin/mount" ~dst:"datakit-mount" ;
      Pkg.test "tests/test" ~args:(Cmd.v "-q");
    ]
  | "datakit-client" -> Ok [
      Pkg.lib   "pkg/META.client"     ~dst:"META";
      Pkg.lib   "datakit-client.opam" ~dst:"opam";
      Pkg.mllib "src/client/datakit-client.mllib";
      Pkg.lib   "src/client/datakit_S.mli";
      Pkg.lib   "src/client/datakit_S.cmi";
      Pkg.mllib "src/vfs/vfs.mllib" ~dst_dir:"vfs";
      Pkg.bin   "src/bin/mount" ~dst:"datakit-mount" ;
      Pkg.test  "examples/ocaml-client/example" ~run:false ;
    ]
  | "datakit-github" -> Ok [
      Pkg.lib   "pkg/META.github"     ~dst:"META";
      Pkg.lib   "datakit-github.opam" ~dst:"opam";
      Pkg.mllib "bridge/github/src/datakit-github.mllib";
      Pkg.bin   "bridge/github/main" ~dst:"datakit-github" ;
    ]
  | other -> R.error_msgf "unknown package name: %s" other
