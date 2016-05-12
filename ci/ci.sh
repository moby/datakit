#!/bin/bash -eux

DB=/srv/datakit
ORIGIN=$DB/remotes/origin

if [ ! -d $ORIGIN ]; then
  mkdir $ORIGIN
  echo -n git@github.com:docker/datakit.git > $ORIGIN/url
fi

# `map_branch fn br` calls `fn` on the current head of `br`, and then again
# each time it changes. `fn` runs in a transaction directory on a new branch.
# The transaction will be committed when `fn` returns.
function map_branch {
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

# `map_prs fn` calls `fn` on each new PR.
function map_prs {
  F=$1
  while read PR; do
    PR_DIR=$DB/github.com/docker/datakit/pr/$PR
    HASH=$(cat $PR_DIR/head)
    echo $HASH > $ORIGIN/fetch
    RESULT_BRANCH="$DB/branch/$F-of-$HASH"
    if [ ! -d $RESULT_BRANCH -o -z "$(cat $RESULT_BRANCH/head)" ]; then
      mkdir $PR_DIR/status/datakit
      echo Building > $PR_DIR/status/datakit/state
      # Base the result branch on the source commit
      TRANS="$RESULT_BRANCH/transactions/$F"
      # Do the build, writing the log to a transaction
      mkdir -p "$TRANS"
      echo $HASH > "$TRANS/merge"
      STATE=Failed
      (cd $TRANS && $F)
      echo commit > $TRANS/ctl
      echo $STATE > $PR_DIR/state
    else
      echo Already processed commit $HASH... skipping
    fi
    echo Waiting for next update to $BRANCH...
  done < $DB/github.com/docker/datakit/pr/updates
}

function build {
  echo 'BUILD FAILED' >msg
  ((docker build rw && docker build -f rw/Dockerfile.github rw && echo PASSED > "$TRANS/msg" && STATE=Passed) || echo '*** BUILD FAILED ***') 2>&1 | tee "rw/log"
}

#map_branch build master
map_prs build
