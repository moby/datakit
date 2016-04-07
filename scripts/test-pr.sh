#!/usr/bin/env bash

set -eux

REPO=$1
SCRIPT=$2

while read N; do
    echo New PR: ${N}...
    PR=/db/github.com/${REPO}/pr/${N}
    mkdir -p ${PR}/status/test
    echo "My little test"    > ${PR}/status/test/descr
    echo "http://docker.com" > ${PR}/status/test/url
    echo Doing some work...
    ${SCRIPT}
    echo success > ${PR}/status/test/state
done < /db/github.com/${REPO}/pr/updates
