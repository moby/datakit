#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let github = Conf.with_pkg "github"

let opam =
  let lint_deps_excluding = Some ["mirage-tc"; "tc"] in
  Pkg.opam_file ~lint_deps_excluding "opam"

  let () =
  Pkg.describe ~opams:[opam] "datakit" @@ fun c ->
  let github = Conf.value c github in
  Ok [
    Pkg.mllib "src/client/datakit-client.mllib";
    Pkg.lib "src/client/datakit_S.mli";
    Pkg.lib "src/client/datakit_S.cmi";
    Pkg.mllib "src/fs9p/fs9p.mllib";
    Pkg.mllib "src/irmin-io/irmin-io.mllib";
    Pkg.mllib "src/ivfs/ivfs.mllib";
    Pkg.mllib "src/vfs/vfs.mllib";
    Pkg.mllib ~cond:github "src/vgithub/vgithub.mllib";
    Pkg.bin ~dst:"datakit" "src/bin/main";
    Pkg.bin ~dst:"datakit-mount" "src/bin/mount";
    Pkg.bin ~cond:github ~dst:"datakit-gh-bridge" "src/bin/github_bridge";
    Pkg.test "tests/test" ~args:(Cmd.v "-q");
    Pkg.test ~run:false "examples/ocaml-client/example";
  ]
