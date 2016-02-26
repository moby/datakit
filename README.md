## Datakit

### What is datakit

Datakit is an experiment with version-controlling application data, and exposing
versioned data through filesystem interfaces. The current prototype exposes the
contents of a Git repository as a 9p endpoint, where the usual Git operations
are encoded as file/directory actions. See the [db documentation](./db/README.md).

### Build

To compile the project, run:

```
make
```

This will build 2 images:

- `datakit-sdk`: an alpine-based container with i9p preinstalled
- `datakit-db`: a daemon to expose a Git repository as an 9p endpoint. Use
  `docker run --rm -it datakit-db --help` for details.


## Release

```
make release VERSION=<vX.Y.Z>
```