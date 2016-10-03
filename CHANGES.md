### 0.6.0 (2016-10-03)

- fix META files (#278, @dsj55)
- fix CI scripts (#262, @dave-tucker)
- create a new `datakit-server` library, to help adding runtime instrospection
  mechanism to servers without having to depend on irmin (#280)

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

- client: speed-up 9p walks (#271, @samoht)

- server: revert back to active polling due to an bug in irmin-watcher's
  inotify support (#269, @samoht)
- server: add more debugging messages for the "GitHub auto-push" feature
  (#261, @talex5)
- server: expose the Irmin "REST" API over HTTP by using the `--listen-http`
  command-line argument (#281, @samoht)

### 0.5.0 (2016-09-02)

- Split the package in 3: server-side, client-side and GitHub bridge
  (#232, @samoht)
- [server] Add `/debug` directory to control log levels (#239, @talex5)

- [github] Lots of stability improvement and bug fixes for the GitHub bridge,
  including:
  - Integrate the GH webhooks directly with the bridge process (#243, @samoht)
  - Track Git References in the GitHub bridge (#234, @samoht)
  - Be more rebust when GitHub API calls fail (#241, @samoht)
  - Fuzz testing of GitHub state and user requests (TODO)

- [server] Use platform-specific filesystem notification (fsevents, inotify)
  instead of polling for branch update notifications (#216, @samoht)
- [server, client] Expose diffs in the Datakit API (#219, @samoht)
- [client] Golang bindings: expose Watch outside the package (#208, @djs55)
- [client] Golang bindings: add SetMultiple method to set more than one field in
  a transaction (#208, @dave-tucker)
- [client] Golang bindings: add a function to delete key in a transaction
  (#208, ebriney)
- [client] Golang bindings: add a nil-able StringRefField (aka
  "the billion dollar mistake") (#208, @djs55)
- [server] Use latest hvsok >= 0.8.1 (#204, @samoht)
- [server] Support inheriting a listening socket (#199, @djs55)
- [client, server] Use maximum_write_payload funtion in recent ocaml-9p
  (#192, @talex5)
- [server] Allow to auto-push datakit state to GitHub (#189, @samoht)
- [server] Make `--bare` the default behavior (#185, @samoht)
- [server] Fix races in stream handling (#170, @talex5)
- [client] Fix leak warning in Go client (#159, @talex5)
- [client, server] Support large writes (#151, @talex5)
- [clent] Add OCaml client bindings (#148, @talex5)
- [server] Improve speed of appending to large files (#143, @talex5)
- [client] Add Minimal Go API (#135, @djs55 and @samoht)
- [client, server] Add a way to trigger remote fetches within the API
  (#132, @samoht)
- [server] Generalise the `--eventlog` command-line argument into
  `--log-destination=(stderr|asl|eventlog)` (#115, @djs55)
- [client] Improve mount options of datakit-mount (#105, @samoht)
- [client, server, github] add Dockerfiles (@talex5 and @samoht)
- [server] add support for Hyper-V socket connections (#94, @djs55)
- [server] Add support for Windows named pipes (#92, @djs55)
- [server] Backport mirage/ocaml-git#147: Less unefficient
  `Git_unix.read_file_with_read` (#88, @chambart)

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
