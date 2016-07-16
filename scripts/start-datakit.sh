#!/usr/bin/env sh

set -exu

REPO_ROOT=$(git rev-parse --show-toplevel)
DOCKERFILE=Dockerfile
NAME=datakit
DATA=/tmp/datakit
ARGS="--url=tcp://0.0.0.0:5640 --git=/data -vv"

docker build -t ${NAME} -f ${DOCKERFILE} ${REPO_ROOT}

docker rm -f ${NAME} || echo skip
docker run --name=${NAME} -v ${DATA}:/data -p 5640:5640 --rm  ${NAME} ${ARGS}
