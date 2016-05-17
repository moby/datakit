#!/bin/bash -eux

DB=/srv/datakit

source map.sh

function build {
  echo 'BUILD FAILED' >msg
  ((docker build rw && \
    docker build -f rw/Dockerfile.github rw && \
    echo PASSED > "$TRANS/msg") || echo '*** BUILD FAILED ***') \
  2>&1 | tee "rw/log"
}

map build master
