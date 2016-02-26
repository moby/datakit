#!/bin/sh
set -ex

eval `opam config env`

./configure
make

# bundler dylib dependencies
mkdir -p _build/root/Contents/MacOS
cp _build/db.native _build/root/Contents/MacOS/com.docker.db
dylibbundler -od -b \
  -x _build/root/Contents/MacOS/com.docker.db \
  -d _build/root/Contents/Resources/lib \
  -p @executable_path/../Resources/lib
