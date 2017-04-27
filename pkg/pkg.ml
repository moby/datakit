#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let opams = List.map (Pkg.opam_file ~lint_deps_excluding:None ~install:false) [
    "datakit.opam";
    "datakit-client.opam";
    "datakit-server.opam";
    "datakit-github.opam";
    "datakit-ci.opam";
    "datakit-bridge-github.opam";
    "datakit-bridge-local-git.opam"
  ]

let () =
  Pkg.describe ~opams ~metas:[] "datakit" @@ fun c -> Ok []
