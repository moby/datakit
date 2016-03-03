## Datakit: a Git-like database with a 9p interface

### Build

At the root of this repository, type `make`. This will produces
`main.native` and `mount.native` in the current directory.

### Setup

How to run the experiments:

```
./main.native --git=/tmp/db.git  # starts the DB using /tmp/db.git for storage
./mount.native /tmp/mnt          # mount the DB into /tmp/mnt
```

Now you can explore, edit and script `/tmp/mnt` and see the result in
`/tmp/db.git`. The full filesystem interface is described below.

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
