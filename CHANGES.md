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
