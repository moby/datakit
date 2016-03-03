## Datakit

A (distributed) dataflow engine.

[![Build Status](https://travis-ci.org/docker/datakit.svg)](https://travis-ci.org/docker/datakit)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://docker.github.io/datakit/)

### Usage

See `dk --help` for more information.

#### Quick configuration

The `dk` will look for a `.datakit` file located in the current directory. The
options can be overriden using command-line parameters.

A typical `.datakit` file contains:

```
local: /tmp/datakit/db
cache: /tmp/datakit/cache
```

- `local` is the local Irmin store used by all the CISO tools It is a normal Git
  repositories that can be inspected and modified if needed.

- `opam-root` is the local OPAM root, used by the CISO workers. It is better to
  have a different OPAM root for every worker (because of OPAM lock files)

- Use `global: http://example.com` instead of `local` to use a remote Irmin
  store.

#### Adding a new task

```
$ ciso add ctypes --rev-deps=*
Task d73d0a4c65bea9c3cd6dc4fd4409381ab01d2f66 added!
```

See `ciso add --help` for more information.

#### Showing tasks, jobs and workers

```
$ ciso show --id=d73d0a4c65bea9c3cd6dc4fd4409381ab01d2f66
id      : d73d0a4c65bea9c3cd6dc4fd4409381ab01d2f66
date    : 15:24:47
repo    : default:https://github.com/ocaml/opam-repository.git
switches: 3.12.1 4.00.1 4.01.0 4.02.3
hosts   : x86_64:linux:ubuntu x86_64:osx:homebrew
packages: ctypes
rev-deps: *
status  : new
```

See `ciso show --help` for more information.

#### Adding a new worker

```
$ ciso work --task # Add a task resolver
$ ciso work        # Add a build worker
```

#### Scheduling the work

Until now, no work has been done. A scheduler needs to connect to the Irmin
database to start scheduling the jobs to the workers.

```
$ ciso schedule
```

### License

ISC. See the [LICENSE](./blob/master/LICENSE) file.