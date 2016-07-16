#!/usr/bin/env sh

set -exu

REPO_ROOT=$(git rev-parse --show-toplevel)
DOCKERFILE=Dockerfile.github
NAME=datakit-gh-bridge
ARGS="--listen=tcp://0.0.0.0:5641 -vv --datakit=tcp:192.168.65.1:5640"

docker build -t ${NAME} -f ${DOCKERFILE} ${REPO_ROOT}

docker rm -f ${NAME} || echo skip
docker run --name=${NAME} --rm \
       -p 8080:80 -p 5641:5641 \
       -v ${HOME}/.github:/root/.github \
       ${NAME} ${ARGS}
