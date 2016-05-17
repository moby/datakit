#!/usr/bin/env sh

docker run -it --privileged --rm --link db -v `pwd`:/src datakit sh
