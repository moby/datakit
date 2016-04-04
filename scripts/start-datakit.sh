#!/usr/bin/env sh

set -exu

REPO_ROOT=$(git rev-parse --show-toplevel)

docker build -t datakit ${REPO_ROOT}
docker rm -f db || echo skip
docker run -p 5650:5640 --name=db --rm \
  -v ${HOME}/.github:/home/opam/.github \
  datakit
