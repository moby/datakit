## Datakit

To compile the project, run:

```
make
```

This will build 2 images:

- `datakit-sdk`: a Debian-based container with irmin9p preinstalled
- `datakit-db`: an Irmin daemon exposing a 9p API. Use
  `docker run --rm -it datakit-db --help` for details.
- `datakit-static`: a static dataflow engine, using simple text files
  as inputs.
