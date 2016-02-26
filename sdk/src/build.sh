#!/bin/sh
set -ex

eval `opam config env`

opam pin add -k git i9p.dev . -n
opam install i9p
