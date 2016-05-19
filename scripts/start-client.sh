#!/usr/bin/env sh

docker run -it --privileged --rm --link datakit -v `pwd`:/src datakit sh
