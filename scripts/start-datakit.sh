#!/usr/bin/env sh

set -exu

DATAKIT_GITHUB=${DATAKIT_GITHUB=0}

if [ "$DATAKIT_GITHUB" != 0 ]; then
    DOCKERFILE=Dockerfile.github
else
    DOCKERFILE=Dockerfile
fi

REPO_ROOT=$(git rev-parse --show-toplevel)

docker build -t datakit -f ${DOCKERFILE} ${REPO_ROOT}

docker rm -f datakit || echo skip
docker run -p 5650:5640 --name=datakit --rm \
  -v ${HOME}/.github:/home/opam/.github \
  datakit
