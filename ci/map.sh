#!/bin/bash -eux

# `map fn branch` calls `fn` on the current head of `branch`, and then again
# each time it changes. `fn` runs in a transaction directory on a new branch.
# The transaction will be committed when `fn` returns.
function map {
  F=$1
  BRANCH=$2
  while read HASH; do
    RESULT_BRANCH="$DB/branch/$F-of-$HASH"
    if [ ! -d $RESULT_BRANCH -o -z "$(cat $RESULT_BRANCH/head)" ]; then
      # Base the result branch on the source commit
      TRANS="$RESULT_BRANCH/transactions/$F"
      # Do the build, writing the log to a transaction
      mkdir -p "$TRANS"
      echo $HASH > "$TRANS/merge"
      (cd $TRANS && $F)
      echo commit > $TRANS/ctl
    else
      echo Already processed commit $HASH... skipping
    fi
    echo Waiting for next update to $BRANCH...
  done < $DB/branch/$BRANCH/head.live
}
