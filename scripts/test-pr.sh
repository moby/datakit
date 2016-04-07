#!/usr/bin/env bash

set -eux

REPO=$1
SCRIPT=$2

while read PR; do
    echo New PR: ${PR}...
    echo pending > /db/github.com/${REPO}/pr/${PR}/status/test
    echo Doing some work...
    ${SCRIPT}
    echo success > /db//github.com/${REPO}/pr/${PR}/status/test
done < /db/github.com/${REPO}/pr/updates
