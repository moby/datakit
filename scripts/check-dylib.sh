#!/usr/bin/env sh

set -ue

REPO_ROOT=$(git rev-parse --show-toplevel)
OUTPUT=${REPO_ROOT}/com.docker.db

# The output should have 2 lines, e.g.:
#
# Datakit.app/Contents/MacOS/com.docker.db:
#  /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1213.0.0)

if [ $(otool -L ${OUTPUT} | wc -l | xargs) != "2" ]; then
    otool -L ${OUTPUT}
    exit 1
fi
