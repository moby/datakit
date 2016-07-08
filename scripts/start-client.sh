#!/usr/bin/env sh

CMD='datakit-mount -h 172.17.0.2 -p 5640 /db && \
     datakit-mount -h 172.17.0.3 -p 5641 /gh && \
     /bin/sh'

docker run -it --privileged --rm \
       --link datakit --link datakit-gh-bridge \
       --entrypoint /bin/bash datakit -c "${CMD}"
