## [DataKit](http://github.com/docker/datakit)

[![Build Status (OSX, Linux)](https://travis-ci.org/docker/datakit.svg)](https://travis-ci.org/docker/datakit)
[![Build status (Windows)](https://ci.appveyor.com/api/projects/status/6qrdgiqbhi4sehmy/branch/master?svg=true)](https://ci.appveyor.com/project/docker/datakit/branch/master)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://docker.github.io/datakit/)

*DataKit* is a tool to orchestrate applications using a 9P dataflow. It
revisits the UNIX pipeline concept, with a modern twist: streams of
tree-structured data instead of raw text. DataKit allows you to define
complex build pipelines over version-controlled data, using shell
scripts interacting with the filesystem. For instance to
[trigger](https://github.com/docker/datakit/blob/master/ci/ci.sh)
a build on every change in the `master` branch:

```bash
# The Git repository to test
DB=/data

# Load the `map` function.
# `map fn branch` calls `fn` on the current head of `branch`, and then again
# each time it changes. `fn` runs in a transaction directory on a new branch.
# The transaction will be committed when `fn` returns.
source ci/map.sh

function build {
  echo 'BUILD FAILED' > msg
  ((docker build rw && echo PASSED > "$TRANS/msg")  || echo '*** BUILD FAILED ***') 2>&1 | tee "rw/log"
}

map build master
```

DataKit is currently used as the coordination
layer for [HyperKit](http://github.com/docker/hyperkit), the
hypervisor component of
[Docker for Mac and Windows](https://blog.docker.com/2016/03/docker-for-mac-windows-beta/).


### Quick Start

The easiest way to use DataKit is to start both the server and the client in a
container.

To expose a Git repository as a 9p endpoint on port 5640 on a private network,
just run:

```shell
$ docker network create datakit-net # create a private network
$ docker run -it --net datakit-net --name datakit -v <path/to/git/repo>:/data docker/datakit
```

*Note*: The `--name datakit` option is mandatory.  It will allow the client
to connect to a known name on the private network.

You can then start a DataKit client, which will mount the 9p endpoint and
expose the database as a filesystem API:

```shell
# In an other terminal
$ docker run -it --privileged --net datakit-net docker/datakit:client
$ ls /db
branch     remotes    snapshots  trees
```

*Note*: the `--privileged` option is needed because the container will have
to mount the 9p endpoint into its local filesystem.

Now you can explore, edit and script `/db`. See the
[Filesystem API](https://github.com/docker/datakit#filesystem-api)
for more details.

#### Experimental GitHub API bindings

To start DataKit with the experimental GitHub bindings:

```shell
$ docker run -it --net datakit-net --name datakit -v <path/to/git/repo>:/data docker/datakit:github
$ docker run -it --privileged --net datakit-net docker/datakit:client
$ ls /db
branch      github.com  remotes     snapshots   trees
```

### Building

The easiest way to build the DataKit project is to use [docker](https://docker.com),
(which is what the
[start-datakit.sh](https://github.com/docker/datakit/blob/master/scripts/start-datakit.sh) script
does under the hood):

```shell
$ docker build -t datakit .
$ docker run datakit
```
These commands will expose the database's 9p endpoint on port 5640.

If you really want to build the project from source, you will need to install
[ocaml](http://ocaml.org/) and [opam](http://opam.ocaml.org/). Then write:

```shell
$ opam pin add datakit . -n -y
$ opam depext datakit -y
$ opam install alcotest datakit --deps-only -y
$ make && make test
```

### Usage

```shell
$ datakit -h
```

## Filesystem API

The `/branch` directory contains one subdirectory for each branch. Use
`mkdir` to create a new branch and `rm` to delete one.

Each branch directory contains:

- `fast-forward` will do a fast-forward merge to any commit ID written
  to this file.

- `head` gives the commit ID of the head of the branch when
  read (or the empty string if the branch is empty).

- `head.live` is a stream which produces a list of commit IDs, one per
  line, starting with the current commit and returning new commits as the
  branch is updated. A branch with no commits is
  represented by a blank line.

- `reflog` is a stream which outputs a new line each time the current
  `HEAD` is updated. The line gives the commit hash (or is blank if the branch
  has been deleted). Unlike `head.live`, `reflog` does not start by outputting
  the current commit and it does not skip commits.

- `ro` is a live read-only view of the current contents of the head of
  the branch.

- `transactions` is used to update the branch.

- `watch` can be used to watch specific files or directories for changes.

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

Read/write transactions can be created by making a new directory for
the transaction in `transactions`.  The newly created directory will
contain:

- `rw`, a directory with the current contents of the
  transaction. Initially, this is a copy of the branch's `ro`
  directory. Modify this as desired.

- `msg`, the commit message to use.

- `parents`, the list of commit hashes of the parents, one per line.
  Initially, this is the single head commit at the time the transaction
  was created, but it can be modified to produce other effects.
  Simply appending another branch's 'head' here is equivalent to doing a Git
  merge with strategy 'ours' (which is *not* the same as "recursive/ours").

- `ctl`, which can be used to commit the transaction (by writing `commit` to
  it) or to cancel it (by writing `close`).

- `merge`, which can be used to start a merge (see below).

For example, to create a file `somefile`:

    ~/db $ mkdir branch/master/transactions/foo
    ~/db $ echo somedata > branch/master/transactions/foo/rw/somefile
    ~/db $ echo commit > branch/master/transactions/foo/ctl

If the branch has been updated since the transaction was created then,
when you try to commit, Irmin will try to merge the changes.

If there is a conflict (two edits to the same file) then the commit
will fail.  Merge errors are reported as 9p error strings.  When a
commit succeeds the transaction directory is automatically removed.

Each 9p connection has its own set of transactions, and the changes in
a transaction cannot be seen by other clients until the transaction is committed.

#### Merging

Within a transaction, write a commit ID to the `merge` file to begin a merge.
The transaction directory will change slightly:

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

A snapshot for a given commit can be opened by accessing the
directory `/snapshots/COMMIT_ID`, which is created on demand.

    ~/db $ cd snapshots/4b6557542ec9cc578d5fe09b664110ba3b68e2c2
    ~/d/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $ ls
    hash  ro/
    ~/d/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $ cat hash
    4b6557542ec9cc578d5fe09b664110ba3b68e2c2
    ~/d/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $ ls ro
    somefile
    ~/d/s/4b6557542ec9cc578d5fe09b664110ba3b68e2c2 $

The contents of a snapshot directory are:

- `ro` is the read-only snapshot, which will never change.

- `hash` contains the commit hash.

- `msg` contains the commit message.

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
- `X-HASH` if the path is an executable file (the hash is the hash of the blob).
- `L-HASH` if the path is a symlink (the hash is the hash of the blob containing the target string).

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
This directory is *not persisted* so will disappear across reboots.

Each directory under `/remotes/<name>` corresponds to the configuration
of a remote server called `<name>`. Create a new directory (with `mkdir`)
to add a new configuration. Every configuration folder contains:

- A writable file: `url`, which contains the remote url.
- A control file: `fetch`, which is used to fetch branches from the
  remote server.
- A read-only stream file: `head` which contains the last known
  commit ID of the remote. On every fetch, a new line is added
  with the commit ID of the remote branch.

To fetch `https://github.com/docker/datakit`'s master branch using the
git protocol:

    ~/db $ cd remotes
    ~/db/remotes $ mkdir origin
    ~/db/remotes $ echo git://github.com/docker/datakit > origin/url
    ~/db/remotes $ echo master > origin/fetch
    ~/db/remotes $ cat origin/head
    4b6557542ec9cc578d5fe09b664110ba3b68e2c2

### GitHub PRs

There is basic support for interacting with GitHub PRs.

    ~/db $ ls github.com/docker/datakit
    41  42
    ~/db $ cat github.com/docker/datakit/pr/41/status/default/state
    pending
    ~/db $ echo success > github.com/docker/datakit/pr/41/status/default/state


This first queries the status of the pull request on the GitHub interface,
then updates the `default` status to `success`.

To create a new status and set its description, url and status:

    ~/db $ PR=github.com/docker/datakit/pr/41
    ~/db $ mkdir $PR/status/test
    ~/db $ echo "My status" > $PR/status/test/descr
    ~/db $ echo "http://example.com" > $PR/status/test/url
    ~/db $ echo success > $PR/status/test/state

To read the last GitHub events related to a repository:

    ~/db $ cat github.com/docker/datakit/events

This is a non-blocking read, and will produce a file where every line is a new
event.

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

    rmdir branch/foo

#### Merge a branch

    cd branch/master/transactions
    mkdir my-merge
    cd my-merge
    cat ../../../feature/head > merge
    cat conflicts
    meld --auto-merge ours base theirs --output rw
    echo commit > ctl


## Language bindings

* **Go** bindings are in the `api/go` directory.
* **OCaml** bindings are in the `api/ocaml` directory. See `examples/ocaml-client` for an example.

## Licensing

DataKit is licensed under the Apache License, Version 2.0. See
[LICENSE](https://github.com/docker/datakit/blob/master/LICENSE) for the full
license text.
