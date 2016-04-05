## Datakit: a Git-like database with a 9p interface

[![docs](https://img.shields.io/badge/doc-online-blue.svg)](http://nuc1.local:1234)

### Quick Start

At the root of the repository:

```
./scripts/start-datakit.sh # Start datakit without the Github API bindings

# In an other terminal
./scripts/start-client.sh
$ datakit-mount
$ cd /db
$ ls
branch     remotes    snapshots  trees
```

Now you can explore, edit and script `/db`.

To start Datakit with the (experimental) Github bindings:

```
DATAKIT_GITHUB=1 ./scripts/start-datakit.sh # Start datakit without the Github API bindings

# In an other terminal
./scripts/start-client.sh
$ datakit-mount
$ cd /db
$ ls
branch      github.com  remotes     snapshots   trees
```

### Build and Install

If you want to compile and install from sources, at the root of this
repository, type `make PREFIX=<prefix>` and `make install`. This will
install `datakit` and `datakit-mount` in `PREFIX/bin`.

### Filesystem API

The `/branch` directory contains one subdirectory for each branch. Use
`mkdir` to create a new branch and `rm` to delete one.

Each branch directory contains:

- `fast-forward` will do a fast-forward merge to any commit ID written
  to this file.

- `head` gives the commit ID of the head of the branch when
  read (or the empty string if the branch is empty).

- `head.live` is a stream which produces a list of commit IDs, one per
  line, starting with the current one and returning new ones as the
  branch is updated. When the branch does not have a commit, this is
  represented by a blank line.

- `reflog` is a stream which outputs a new line each time the current
  HEAD is updated. The line gives the commit hash (or is blank if the branch
  has been deleted). Unlike `head.live`, `reflog` does not start by outputting
  the current commit and it does not skip commits.

- `ro` a live read-only view onto the current contents of the head of
  the branch.

- `transactions`, which is used to update the branch.

- `watch`, which can be used to watch specific files or directories for changes.

Note that reading from `head.live` will skip directly to the latest
commit: even if you read continuously from it, you will not
necessarily see all intermediate commits.

The root also contains `/snapshots`, which can be used to explore any
commit in the repository, if you know its ID.  The directory will
always appear empty, but attempting to access a subdirectory named by
a commit ID will work.

The `/trees` directory works in a similar way to `/snapshots`, but is
indexed by directory tree or file hashes (as read from `tree.live`)
rather than by commit hashes.

#### Transactions

Read/write transactions can be created my making a new directory for
the transaction in `transactions`.  The newly created directory will
contain:

- `rw` a directory with the current contents of the
  transaction. Initially, this is a copy of the branch's `ro`
  directory. Modify this as desired.

- `msg` is the commit message to use.

- `parents` is the list of commit hashes of the parents, one per line.
  Initially, this is the single head commit at the time the transaction
  was created, but it can be modified to produce other effects.
  Simply appending another branch's 'head' here is equivalent to doing a Git
  merge with strategy 'ours' (which is *not* the same as "recursive/ours").

- `ctl` can be used to commit the transaction (by writing `commit` to
  it) or to cancel it (by writing `close`).

- `merge` can be used to start a merge (see below).

For example, to create a file `somefile`:

    ~/mnt $ mkdir branch/master/transactions/foo
    ~/mnt $ echo somedata > branch/master/transactions/foo/rw/somefile
    ~/mnt $ echo commit > branch/master/transactions/foo/ctl

If the branch has been updated since the transaction was created then,
when you try to commit, Irmin will try to merge the changes.

If there is a conflict (two edits to the same file) then the commit
will fail.  Merge errors are reported as 9p error strings.  When a
commit succeeds the transaction directory is automatically removed.

Each 9p connection has its own set of transactions, and the changes in
a transaction cannot be seen by other clients until it is committed.

#### Merging

Within a transaction, write a commit ID to the `merge` file to begin a merge.
The transaction directory will change slighty:

- `ours` is a read-only directory, containing whatever was previously in `rw`
- `theirs` is the commit being merged
- `base` is a common ancestor (or empty, if the commits share no history)
- `rw` contains irmin9p's initial attempt at a merge
- `conflicts` is a list of files in `rw` that need to be resolved manually
- `parents` has the new commit appended to it

Note that, unlike Git, irmin9p does not attempt to merge within files.
It simply replaces files with conflicting changes with a message noting
the conflict.

For each file in conflicts you should resolve the problem by either deleting
the file or doing your own three-way merge using `ours`, `theirs` and `base`.
When a file has been edited, it is removed from `conflicts`. You cannot commit
the transaction while `conflicts` is non-empty.

You may merge several commits in a single transaction, if desired.
However, doing multiple non-trivial merges at once will make viewing
the resulting merge commit difficult with most tools.

#### Snapshots

A snapshots for a given commit can be opened by accessing the
directory `/snapshots/COMMIT_ID`, which is created on demand.

    ~/mnt $ cd snapshots/4b6557542ec9cc578d5fe09b664110ba3b68e2c2
    ~/m/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $ ls
    hash  ro/
    ~/m/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $ cat hash
    4b6557542ec9cc578d5fe09b664110ba3b68e2c2
    ~/m/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $ ls ro
    somefile
    ~/m/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $

The contents of a snapshot directory are:

- `ro` is the read-only snapshot, which will never change.

- `hash` contains the commit hash.

- `parents` contains the hashes of the parent commits, one per line.


#### Watches

To watch for changes affecting a specific file or subdirectory in a
branch, use the branch's `watch` directory.

Each directory under `watch` contains a `tree.live` file that outputs
the current hash of the object that directory watches.  The top
`watch/tree.live` file tracks changes to all files and directories.
To watch for changes under `src/ui`, read the file
`watch/src.node/ui.node/tree.live`.  That is, add `.node` to each path
component to get a directory for that node.

Reading from a `tree.live` file outputs first one line for the current
state of the path.  This can be:

- A blank line, if the path does not currently exist.
- `D-HASH` if the path is a directory (the hash is the tree hash).
- `F-HASH` if the path is a file (the hash is the hash of the blob).

When the branch head changes so that the path has a different output,
a new line will be produced, in the same format. As with `head.live`,
watching for changes is triggered by reading on the open file, so if
several changes occur between reads then you will only see the latest
one.

**Note:** Listing a watch directory shows `.node` subdirectories for paths
that currently exist. However, these are just suggestions; you can
watch any path, whether it currently exists or not.

### Fetch

To fetch from a remote repository, use the `/remotes` root directory.
This directory is *not persisted* so will disapear accross reboots.

Each directory under `/remotes/<name>` corresponds to the configuration
of a remote server called `<name>`. Create a new directory (with `mkdir`)
to add a new configuration. Every configuration folder contains:

- A writable file: `url`, which contains the remote url.
- A control file: `fetch`, which is used to fetch branches from the
  remote server.
- A read-only stream file: `head` which contains the last knowns
  commit ID of the remote. On every fetch, a new line is added
  with the commit ID of the remote branch.

To  fetch `https://github.com/docker/datakit`'s master branch:

    ~/mnt $ cd remotes
    ~/mnt/remotes $ mkdir origin
    ~/mnt/remotes $ echo https://github.com/docker/datakit > origin/url
    ~/mnt/remotes $ echo master > origin/fetch
    ~/mnt/remotes $ cat origin/head
    4b6557542ec9cc578d5fe09b664110ba3b68e2c2

### Github PRs

There is a rudimentary support for interacting with Github PRs.

    ~/mnt $ ls github.com/docker/datakit
    41  42
    ~/mnt $ cat github.com/docker/datakit/pr/41/status/default
    pending
    ~/mnt $ echo success > github.com/docker/datakit/pr/41/status/default

This will toggle the status of the pull request on the Github interface.

### How do I...

#### Create a new branch

    mkdir branch/foo

#### Fork an existing branch

    cd branch
    mkdir new-branch
    cp old-branch/head new-branch/fast-forward

#### Rename a branch

    mv branch/old-name branch/new-name

#### Delete a branch

    rm branch/foo

#### Merge a branch

    cd branch/master/transactions
    mkdir my-merge
    cd my-merge
    cat ../../../feature/head > merge
    cat conflicts
    meld --auto-merge ours base theirs --output rw
    echo commit > ctl

## TODO

- change user name and email in commit message
- add support for Github hooks
- add support for commit hooks
