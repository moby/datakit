#!/usr/bin/env sh

set -exu

REPO_ROOT=$(git rev-parse --show-toplevel)
DOCKERFILE=Dockerfile.github
NAME=datakit-gh-bridge

docker build -t ${NAME} -f ${DOCKERFILE} ${REPO_ROOT}

docker rm -f ${NAME} || echo skip
docker run --name=${NAME} --rm \
       -p 8080:80 \
       -v ${HOME}/.github:/root/.github \
       ${NAME}
