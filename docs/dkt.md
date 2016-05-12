# dtk -- Datakit

Datakit is a version-controlled data-flow manager. It is used to
describe complex build/CI pipelines, spread accross various
heterogeneous components. The running example that this document will
follow is a replacement for the current ad-hoc built pipeline for
Docker for Mac and Windows Editions.

First, some definitions: for datakit, a data-flow network or graph is
a directed, acyclic bi-graph. These graphs have two kinds of nodes:

- **compute nodes** which model computation; and
- **data sources** which model source of external data.

An invariant of such graphs is that vertices always link compute nodes
to data sources and data sources to compute nodes. Some sort of links
can be manually added between data sources (for instance to account
for out-of-band relations) but compute nodes should not be linked
together.

Data sources with no inputs are **global sources**. Data sources with
no outputs are **global sinks**. Typically, global sources are the Git
repositories of source code. Global sinks are hosting build artifacts
that users can download, such as HockeyApp or S3. Datakit orchestrates
the flow of data accross that graph, and record provenance for every
new data generated so it is easy to audit the system. Also it is
flexible enough to hook into the pipeline and use custom data sources,
while keeping full provenance and reproducibility.


### Starting a Project

To create a new datakit project, run:

```
$ dkt init [--github-token=TOKEN] [--hockeyapp-token=TOKEN]
Initialized empty Datakit repository in /tmp/foo/.dkt/
```

This creates a `.dkt` directory containing (at least) a `.git` folder
where the project metadata are stored as a Git repositories and a
other folders containing secrets (such as Github or Docker Hub
credentials/tokens) and local configuration.

Existing project can be cloned using the usual Git workflow:

```
$ dkt clone REPO [--github-token=TOKEN] [--docker-token=TOKEN]
```

### Managing Data Sources

### Adding a New Source

To add a new source:

```
$ dkt source add NAME [LOCATION[:TAG]] [-k KIND] [-p PATH]
    [--read-only] [--write-only] [--read-write]
```

Usually the kind can be infered from the location, and the location
supports shortcuts.

KIND      | Description | Location shortcut
----------|-------------|------------------
docker    | Docker images | user/repo
git       | Git repositories | Git url
github    | Shortcut for Git repositories hosted on Github | user/repo
S3        | S3 buckets | bucket name
path      | local paths | absolute,relative and temporary paths
hockeyapp | HockeyApp application | App ID

- `NAME`, `LOCATION` and `TAG` can be any valid UTF8 string.

- `LOCATION` can also be an other data source. In that case, the new
data source will inherit the `KIND`, `LOCATION`, `TAG` and `PATH` from
its parent unless specified on the command-line. In that case the new
value will overwrite the parent's value, unless `PATH` is specified:
in that case, the new path is understood to be relative to the
previous path.

- For data sources supporting tags or specific IDs (such as sources of
kind `git`, `github` or `docker`), `TAG` can help desambiguate the
location. When appropriate, `TAG` can also represent a pattern, for
instance `*` to describe all Git references, or `v[0-1]+` to describe
tagged versions.

- `PATH` should be a valid sub-path in the data-source. Not all the data
sources will support that argument efficiently.

- Data sources have read-write properties, which can be set using
`--read-only`, `--write-only` and `--read-write`. Properties' default
vary depending on the source kind. For instance, for Git it is
`--read` and `--write`

As an example, let start by describing some of the data sources needed
to build and test [pinata](https://github.com/docker/pinata) today:

```
$ dkt source add pinata docker/pinata --king github
$ dkt source add pitfall docker/pinata:master --path v1/cmd/pitfall --read-only
$ dkt source add cases docker/pinata --path v1/tests
$ dkt source add dmg e63b4e58fd9d40e48e74f8f2e71c49a5 --kind hockeyapp
$ dkt source add tests
$ dkt source list
NAME   KIND      LOCATION                                      TAG    PATH
pinata github    https://github.com/docker/pinata              -      -
pifall github    https://github.com/docker/pinata              master /v1/pitfall
cases  github    https://github.com/docker/pinata              master /tests
dmg    hockeyapp https://rink.hockeyapp.net/manage/apps/259320 -      -
tests  path      /tmp/test-1234/                               -      -
```

> NOTE(samoht): would be nice if we can attach filters to data-sources

#### Source Events

Once added, reading or writing to a data source generate source
events. Events carry different attributes, depending on the kind of
the source they come from. For instance events coming from Github
sources will have a `COMMIT` attributes and might have `PR` related
attributes as well (such as `PR/number`, `PR/state`, etc.) Note that,
depending on the attribute, a different hash (sha1 for Git, sha256 for
Docker) or digest (md5 for paths and hockeyapp) can be used.
Attributes might be writable (such as `PR/state`).

```
$ dkt source log
NAME     HEAD
pinata   1bf6cb20
pitfall  b348e952
cases    e5e433a1
dmg      0E500A4E
tests    dedba9be
```

Depending on their kind, data sources can exist in multiple
versions. A Git repository on Github will expose all its commits and
the optional PR it is the head of:

```
$ dkt source log pinata
ID       PR COMMIT
1bf6cb20 -  sha1:1bf6cb2021fd9f38605be50d4ebc9e18b0bf6e90
3dd2cc1d 42 sha1:3dd2cc1db51828a643237c39d5ed76bb066bcad8
[..]
```

Hockeyapp sources contains the list of `dmg` files that the user can
download with their checksum and build numbers:

```
$ dkt source log dmg
ID       BUILD CHECKSUM
0E500A4E 2246  md5:0E500A4EB2A162F8098575DACED142E1
82A53D4E 2245  md5:82A53D4EA834FCAC928D2D589E3CF859
[..]
```

Data-source can be forced to a specific version (note the `*` sign
close to the name of the pinned package):

```
$ dkt source pin pinata 3dd2cc1d
$ dkt source list
NAME     HEAD
pinata*  3dd2cc1d
pitfall  b348e952
cases    e5e433a1
dmg      0E500A4E
tests    dedba9be
```

To change the location of a source (to point to a local mirror, for
instance):

```
$ dkt source pin pinata /local/git/pinata -k git
```

> NOTE(samoht): all source by default will subscribe to upstream
changes to get notified when there are changes. If a data-source is
locally updated, the change will be automatically pushed to the remote
source if the source has write access (which is set-up on init, using
the `--read-write` or the `--write` flag). It is expected that the
changes are propagated asynchronously so might results in conflicts:
each data-sources will have a built-in default conflict resolution
mechanism but the user might be able to overwrite them and manually
resolve confict. TBD

Out-of-band relations can also be accounted in this system. For
instance, to relate a particular `dmg` stored in hockeyapp and the
Git commit it was generated from, use:

```
dkt source link pinata:3dd2cc1d dmg:0E500A4E
```

Although ideally, this relation should be managed directly by datakit.

### Compute nodes

Compute nodes are temporary process executing arbitrary
commands. Their input data sources are put in scope before the command
are run. The compute nodes are expected to put their results in their
output data sources: that's the only way they have to persist
state. Compute node can have OS/architecture constraints, and can be
successful or fail.

To use the `pitfall` tool, we create two phases: one for build and one
for run, and we create a temporary local path which will contain the
build artefacts generated during the build phase:

```
$ dkt source add build
$ cat buid.yml
inputs:
  - pitfall
outputs:
  - build
run:
  - go build ${pitfall:ROOT}/v1/cmd/pitfall
  - cp ${pitfall:ROOT}/v1/cmd/pitfall/pitfall ${build:ROOT}
on-error:
  - mailto:build@docker.com

$ dkt node run build.yml
de728e45
```

Note that inputs and outputs are explicit, and that input events'
attributes are available as environment variable in the scope of the
run script. Note: look at TravisCI event hooks for
`on-error`/`on-success` hooks.

The we specify the context in which that node will run:

```
$ cat run.yml
labels:
  - test
  - osx
inputs:
  - dmg
  - cases
  - build
output:
  - tests
run:
  - mkdir ${tests:ROOT}/${dmg:BUILD}
  - ${build:ROOT}/pitfall -x "10.11" -b ${dmg:BUILD} ${cases:ROOT} >
      ${tests:ROOT}/${dmg:BUILD}
  -  echo "Exit $?" >> ${tests:ROOT}/${dmg:BUILD}

$ dkt node run run.yml
aa4132a1
```

The `run` command which is executed has some environment variables
set, parameterized by its inputs and outputs, of the form
`${<SOURCE-NAME>:<SOURCE-FIELD}`. Every data-source define its own set
of fields. For instance, HockeyApp data sources expose a a `BUILD`
field which will be different for every build number. Local paths have
a `ROOT` field. It also exposes private fields defined in the local
configuration file (e.g. `.dkt/config`). In the case of Pinata, this
is for instance useful to store ESX credentials: these could be
accessed using `${ESX_USER}` and `${ESX_PASS}`.

Every command is executed when at least one of its inputs has a new
version. See next section for more information about the scheduler.

The node ID (e.g. `de728e45`) is deterministic, it is the hash of all
its parameters and of the command to run, so that it's easy to merge
compute nodes when created concurrently.

```
$ dkt node list
ID        FILE  LABEL(S)  INPUT(S)        OUTPUT(S)
de728e45  build -         pitfall         build
aa4132a1  run   test,osx  build,cases,dmg tests
```

```
$ dkt node log aa4132a1
ID       STATUS  INPUT(S)                                    OUTPUT(S)
ad324g54 pending dmg:0E500A4E,cases:b348e952,buid:23e4eabc   -
ef3212a1 success dmg:82A53D4E,cases:b348e952,buid:23e4eabc  tests:d41d8cd9
```

### Scheduler

So far we've described a very static model. When the datakit
daemon/scheduler runs, it monitors for changes in data-sources and run
new computations when needed. TBD

By default, all the data sources will pull for upstream changes --
once a change is detected, the appropriate sub-graph will be
recomputed automatically. It should be possible to

- force the recomputation of a sub-graph
- manually update a data source
- stop automatic update of data sources

> TODO: add words about workers -- workers pick jobs to do using tags and
        complete them.

### Inspect/Audit

Powerful tooling to inspect provenance of the generated artifacts:


```
$ dkt inspect tests:d41d8cd9
# manifest for tests:d41d8cd9
[
  { "ID":     "1bf6cb20",
    "source": "pinata",
    "kind":   "git",
    "url":    "https://github.com/docker/pinata.git",
    "commit": "sha1:1bf6cb2021fd9f38605be50d4ebc9e18b0bf6e90" },
  { "ID":     "0E500A4E"
    "inputs": [ "1bf6cb20" ],
    "source": "dmg",
    "kind":   "hockeyapp",
    "url":    "https://rink.hockeyapp.net/manage/apps/259320",
    "digest": "md5:0E500A4EB2A162F8098575DACED142E1"
    },
  { "ID":     "d41d8cd9",
    "source": "path",
  [..] },
  { "ID":     "de728e45",
    "inputs": ["1bf6cb20", "b348e952"]
    "node":  : "run",
    "volumes": [ "dmg:/in", "tests:/out" ],
    "image": "pitfall",
      }
```

Use `dkt inspect tests:d41d8cd9 --dot` to get a Dot representation of
the graph.
