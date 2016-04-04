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
