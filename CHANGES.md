### 0.9.0 (2017-02-03)

**prometheus** (new)

- prometheus: split prometheus into its own opam package
  (#438, @talex5 and @avsm)

**datakit-local-git** (new)

- local-git: add local git bridge (#458, @talex5)

Normally, we use datakit-github to monitor the state of a remote
repository on GitHub and use that as the input to the CI. When getting
started with DataKitCI it is more convenient to be able to monitor a
local Git repository.

**datakit-ci**

- ci: add Redis-backed web sessions (#393, @talex5)
- ci: don't copy command output to stdout (#394, @talex5)
- ci: fetch each GitHub user's security information at login  (#398, @talex5)
- ci: refactor the API to use `datakit-gitub` to manipulate GitHub metadaa.
  This is a major breaking API change (#384, @samoht)
- ci: report more metrics: number of tags, branches and open PRs (#408, @talex5)
- ci: fix race if two people request a rebuild at once (#416, @talex5)
- ci: fix fetching of tag objects (#417, @talex5)
- ci: change the url scheme for PRs and references (#411, @samoht)
- ci: improve form validation (#420, @talex5)
- ci: migrate self-ci from docker-compose to docker-cloud (#421, @talex5)
- ci: add support for getting X.509 certificates via certbot (#422, @talex5)
- ci: add web UI for configuring GitHub authentication (#424, @talex5)
- ci: allow non-TLS deployments (#425, @talex5)
- ci: bound the size of Term.wait_for_all status messages (#428, @avsm)
- ci: clone missing Git repositories automatically (#429, @talex5)
- ci: expose more functions on `Target.v` (#430, @avsm)
- ci: don't require a compare function for builders (#435, @talex5)
- ci: separate rebuilding from cache lookups (#436, @talex5)
- ci: cope with targets being deleted while we're updating their status
  (#443, @talex5)
- ci: add live streaming of logs (#449, @talex5)
- ci: allow reporting Prometheus metrics on a separate port (#452, @talex5)
- ci: include the PR's title in the PR status page
- ci: allow cancelling Docker builds (#462, @talex5)
- ci: add Docker.run (#462, @talex5)
- ci: evaluate terms in parallel (#464, @talex5)
- ci: add ANSI escape sequence parser for coloured logs (#466, @talex5)

**datakit-github**

- github: do not commit empty changes (#397, @samoht)
- github: use an unlimited number of fids and walk in parallel to speeds-up
  init time massively (#401, @samoht)
- github: enable Prometheus monitoring (#452, @talex5)

***datakit-client**

- client: add client-side caching (#400, @samoht)
- client: improve the memory representation of paths (#399, @samoht)

### 0.8.1 (2016-12-02)

**datakit-github**

- fix regression in comparison between build status (#388, @samoht @avsm)

### 0.8.0 (2016-12-02)

**datakit-ci**

- ci: add Prometheus metric reporting (#352, #353, @talex5)
- ci: allow hiding some arguments when logging commands (#369, @talex5)
- ci: add Term.wait_for, wait_for_all and without_logs (#370, @talex5)
- ci: report GC and system metrics (#379, @talex5)
- ci: better commit messages when updating the state (#385, @talex5

**datakit-github**

- github: set user-agents (#362, @samoht)
- github: Add a `--resync-interval` option to resync the database regularly
  (#368, @samoht)
- github: Add `.dirty` files to tell the bridge to resync on repo/prs
  (#368, @samoht)
- github: split the package into `datakit-github.client`,
  `datakit-github.server` and `datakit-github` (#375, @samoht)
- github: add `Snapshot.find` (#376, @samoht)
- github: add `Repo.Map`, `Repo.of_string` (#376, @samoht)
- github: add `Satus.compare_id` (#376, @samoht)
- github: replace `create` functions by `v` to be consistent (#376, @samoht)
- github: add `Index` modules for PRs, refs and build status (#377, @samoht)
- github: rename Commit.id into Commit.has (#378, @samoht)
- github: make Status.url an Uri.t option instead of string option
  (#383, @samoht)
- github: github: expose Conv.{pr,ref,status} (#386, @samoht)

**datakit-client**

- client: more consistent handling of urls arguments. `tcp:foo`
  becomes `tcp://foo` and `fd:42` becomes `fd://42` (#358, @samoht)
- client: client: add Datakit_path.{basename,dirname} (#373, @samoht)

**datakit-server**

- server: remove the `--sandbox` argument (#357, @samoht)
- server: more consistent handling of urls arguments. `tcp:foo`
  becomes `tcp://foo` and `fd:42` becomes `fd://42` (#358, @samoht)
- server: use hvsock 0.11.1 (#356, @djs55)

### 0.7.0 (2016-11-17)

The highlight of that release is `datakit-ci`: a new library to help
creating new CI pipelines built on top of DataKit.

**datakit-ci**

- ci: improve UI for viewing logs (#341, #342, @talex5)
- ci: specify a metadata store default that matches datakit (#330, @avsm)
- ci: let CI binaries specify custom CLI term info (#329, @avsm)
- ci: add GitHub-based login to web UI (#328, @talex5)
- ci: return some HTML body text for more errors (#325, @talex5)
- ci: add .monitor files automatically (#324, @talex5)
- ci: add setting to configure a public CI (#320, @talex5)
- ci: add a CI script to test DataKit itself (#314, @talex5)
- ci: allow configuring CI name, dashboard config in config file (@306, @talex5)
- ci: Add library for writing DataKit-based Continuous Integration systems
  (#302, @talex5)

**datakit-github**

- github: add a more specific error when there is no datakit-github token
  (#323, @avsm)
- github: major refactoring to use only one branch in datakit to persist
  the data and keep the rest in memory otherwise. Also use a stronger model
  of ownership to decide whether datakit or GitHub is right (#311, @samoht)

**datakit**

- datakit: update to irmin.0.12.0 to use faster native watch notifications
  instead of file-system polling and full scanning (#347, @samoht)

**datakit-client**

- client: simplify path handling: when creating things, pass the full
  path as one argument rather than a directory and a name. (#306, @talex5)

**datakit-server**

- server: Fix log-destination command-line arguments (#340, @samoht)
- server: improve named-pipe support (#333, by @simonferquel and @samoht)
- server: Catch top level Lwt.async exceptions (#327, @samoht)
- server: expose `Vfs.Logs` to expose the state of `Logs.Src` over 9p. This
  is used by datakit and datakit-bridge to expose `/debug` and let other
  datakit servers to easily do the same thing (#295, @samoht)

### 0.6.0 (2016-10-03)

- fix META files (#278, @dsj55)
- fix CI scripts (#262, @dave-tucker)
- create a new `datakit-server` library, to help adding runtime instrospection
  mechanism to servers without having to depend on irmin (#280)

**datakit-github**

- github: add documentation (#258, @talex5)
- github: add API resources capabilities (#279, @samoht)
- github: fix support for annotated tags (#274, @samoht)
- github: fix setting build status when description is larger than 140
  characters (#273, @samoht)
- github: prune the public branch too (#272, @samoht)
- github: read combined build status instead of the full build status
  history (#265)
- github: use ocaml-github 2.0.0 and ocaml-github-hooks (#264, @samoht)
- github: fix event loop (#259, #260, @talex5)

**datakit-client**

- client: speed-up 9p walks (#271, @samoht)

**datakit-server**

- server: revert back to active polling due to an bug in irmin-watcher's
  inotify support (#269, @samoht)
- server: add more debugging messages for the "GitHub auto-push" feature
  (#261, @talex5)
- server: expose the Irmin "REST" API over HTTP by using the `--listen-http`
  command-line argument (#281, @samoht)

### 0.5.0 (2016-09-02)

Split the package in 3: server-side, client-side and GitHub bridge
(#232, @samoht)

**datakit-github**

- github: add Dockerfiles (@talex5 and @samoht)
- github: Lots of stability improvement and bug fixes for the GitHub bridge,
  including:
  - Integrate the GH webhooks directly with the bridge process (#243, @samoht)
  - Track Git References in the GitHub bridge (#234, @samoht)
  - Be more rebust when GitHub API calls fail (#241, @samoht)
  - Fuzz testing of GitHub state and user requests (TODO)

**datakit-server**

- server: Add support for Windows named pipes (#92, @djs55)
- server: add support for Hyper-V socket connections (#94, @djs55)
- server: Backport mirage/ocaml-git#147: Less unefficient
  `Git_unix.read_file_with_read` (#88, @chambart)
- server: add Dockerfiles (@talex5 and @samoht)
- server: Generalise the `--eventlog` command-line argument into
  `--log-destination=(stderr|asl|eventlog)` (#115, @djs55)
- server: Improve speed of appending to large files (#143, @talex5)
- server: Fix races in stream handling (#170, @talex5)
- server: Make `--bare` the default behavior (#185, @samoht)
- server: Allow to auto-push datakit state to GitHub (#189, @samoht)
- client, server: Use maximum_write_payload funtion in recent ocaml-9p
  (#192, @talex5)
- server: Support inheriting a listening socket (#199, @djs55)
- server: Use latest hvsok >= 0.8.1 (#204, @samoht)
- server: Use platform-specific filesystem notification (fsevents, inotify)
  instead of polling for branch update notifications (#216, @samoht)
- server: Expose diffs in the Datakit API (#219, @samoht)
- server: Add `/debug` directory to control log levels (#239, @talex5)

**datakit-client**

- client: add Dockerfiles (@talex5 and @samoht)
- client: Improve mount options of datakit-mount (#105, @samoht)
- client, server, github: add Dockerfiles (@talex5 and @samoht)
- client, server: Add a way to trigger remote fetches within the API
  (#132, @samoht)
- client: Add Minimal Go API (#135, @djs55 and @samoht)
- client: Add OCaml client bindings (#148, @talex5)
- client, server: Support large writes (#151, @talex5)
- client: Fix leak warning in Go client (#159, @talex5)
- client: Golang bindings: expose Watch outside the package (#208, @djs55)
- client: Golang bindings: add SetMultiple method to set more than one field in
  a transaction (#208, @dave-tucker)
- client: Golang bindings: add a function to delete key in a transaction
  (#208, ebriney)
- client: Golang bindings: add a nil-able StringRefField (aka
  "the billion dollar mistake") (#208, @djs55)
- client: Expose diffs in the Datakit API (#219, @samoht)

### 0.4.0 (2016-04-25)

- Bindings to the Github events API to get notification on new pull requests
  (#63, @samoht)

    ```
while read PR do
  echo New PR: $PR
  echo pending > /db/github.com/docker/datakit/pr/status/test
  ...
done < /db/github.com/docker/datakit/pr/updates
    ```
- Allow to set the description and urls for pull request status (#64, @samoht)
- Update VFS to report file metadata (#66, @talex5)
- Add `stats` to file metadata (#66, @talex5)
- Replace "Buffer to small" error with a read of 0 items (#71, @talex5)
  Linux reads a directory like like:
    - Linux asks us to fill an 8K buffer.
    - We return nearly 8K (must return a whole number of items).
    - Linux asks us to fill the remaining few bytes.
    - We return nothing.
    - Linux flushes its buffer and asks for the next 8K.
    ...
  This means we get roughly twice as many roundtrips as needed, but it
  does work.
- Allow renaming files in a transaction (#73, @talex5)
- Metadata support (executable files and symlinks):
    - creation of executable files
    - creation of symlinks
    - chmod +x works
    - writing preserves the executable flag
    - merging now also merges metadata
  This appears to be sufficient to build Docker under Datakit  (#69, @talex5)
- Don't allow fast-forward to missing commit (#81, @talex5)
- Simplify the `Vfs.File.Stream` API and use it consistently through the
  codebase (#83, @samoht)
    - stream files have a state
    - when you open a stream file, you immediately get a line with the current
      state
    - as long as you keep the file open, you get a newline for every updates
    - you can have multiple reader (e.g. concurrently open files)
    - the writers as "publish" functions in the VFS API, you can have multiple
      concurrent writers. In that case, the behaviour is similar to condition
      broadcasts: every reader should receive the updates in the same order.

### 0.3.0 (2016-04-04)

- Fix a memory leak. This was due to a missing `free` in the readdir
  bindings in the upstream Lwt library, see
  https://github.com/ocsigen/lwt/issues/229 (#57 by @talex5 and @samoht)
- Fix a regression introduced in 0.2.0 where inode numbers were not stable
  anymore, which broke `cp` commands (#56 by @talex5)
- Enable verbose mode by setting the DATAKIT_VERBOSE environment variable
  and turn-on more verbose logging of underlying libraries (#53 by @samoht)
- Replace Irmin views with a more efficient representation which does not
  keep track of transaction's actions and remove the need for creating temporary
  dangling commits (#16, #51 by @talex5)
- Backport performance fixes from upstream `ocaml-git` (#43, #47 by @talex5)
- Minimal integration with the Github PR API using 9p  (#42 by @samoht)

    ```
~/mnt $ ls github.com/docker/datakit
41  42
~/mnt $ cat github.com/docker/datakit/pr/41/status/default
pending
~/mnt $ echo success > github.com/docker/datakit/pr/41/status/default
    ```
- Use Astring and Logs (#50, #43 by @talex5 and @samoht)

### 0.2.0 (2016-03-18)

- Add support for "Git remotes" in the filesystem (#21, @samoht). Now you can:

    ```
    ~/mnt $ cd remotes
    ~/mnt/remotes $ mkdir origin
    ~/mnt/remotes $ echo https://github.com/docker/datakit > origin/url
    ~/mnt/remotes $ echo master > origin/fetch
    ~/mnt/remotes $ cat origin/head
    4b6557542ec9cc578d5fe09b664110ba3b68e2c2
    ```

- Remove dependency to gmp (@samoht)

- Split the library into 3 parts (@samoht)
  - VFS: the virtual filesystem, live in memory and not version-controlled
  - I9p: project a Git-compatible database (Irmin) into a VFS hierarchy
  - 9pfs: expose the VFS over 9p

- Add CI scripts to test Datakit build using Datakit (@talex5)
- Restructure the build process (@samoht)
- Add a `-bare` option for storing local state in a bare Git repo (@talex5)
- Updates for latest ocaml-9p API changes (@talex5)

### 0.1.1 (2016-02-26)

- Fix OSX build
- Fix in-container build

### 0.1.0 (2016-02-26)

- Initial release. Code extracted from:

    ```
docker/pinata 56982b9e744da6f895a0a4682f2e870f526d2ea3
    ```
