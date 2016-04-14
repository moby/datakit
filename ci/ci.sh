#!/bin/bash -eux
while read HASH; do
  echo Testing $HASH...
  docker pull ocaml/opam:alpine
  # Create branch for the result
  RESULT_BRANCH="/db/branch/ci-of-$HASH"
  mkdir "$RESULT_BRANCH" || echo Branch already exists
  if [ -f "$RESULT_BRANCH/ro/log" ]; then
    echo Already built this commit... skipping
  else
    # Base the result branch on the source commit
    echo $HASH > "$RESULT_BRANCH/fast-forward"
    TRANS="$RESULT_BRANCH/transactions/log"
    # Do the build, writing the log to a transaction
    mkdir "$TRANS"
    echo 'BUILD FAILED' > "$TRANS/msg"
    ((./build.sh $HASH && echo PASSED > "$TRANS/msg") || echo '*** BUILD FAILED ***') 2>&1 | tee "$TRANS/rw/log"
    echo commit > "$TRANS/ctl"
  fi
  echo Waiting for next update...
done < /db/branch/master/head.live
