#!/bin/sh

set -eu

REPO_ROOT=$(git rev-parse --show-toplevel)

watermark() {
    file=$1
    path="${REPO_ROOT}/src/bin/${file}"
    tmp="${REPO_ROOT}/src/bin/${file}.tmp"
    cp "$path" "$tmp"
    sed -e "s/%%VERSION%%/$(git describe --always --dirty)/g" "$tmp" > "$path"
}

watermark main.ml
watermark github_bridge.ml
watermark mount.ml
