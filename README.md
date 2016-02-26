## Datakit

To compile the project, run:

```
make
```

This will build 2 images:

- `datakit-sdk`: an alpine-based container with i9p preinstalled
- `datakit-db`: a daemon to expose a Git repository as an 9p endpoint. Use
  `docker run --rm -it datakit-db --help` for details.
