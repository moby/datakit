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

### Test

First, create a new Git-format volume to store the data:

    docker run --rm -it -u root -v datakit-data:/data ocaml/opam git init --bare /data/.git

Start the Datakit server:

    docker run -d --name datakit-db -p 5640:5640 -u root -v datakit-data:/data datakit-db --git /data/.git --bare --url tcp://0.0.0.0:5640/

On the host VM (`screen ~/Library/Containers/com.docker.docker/Data/com.docker.driver.amd64-linux/tty` for Moby):

    mkdir /mnt/datakit
    mount -t 9p -o trans=tcp,port=5640 127.0.0.1 /mnt/datakit

Run a container using Datakit:

    docker run --rm -it -v /mnt/datakit:/mnt/datakit ubuntu /bin/bash
    cd /mnt/datakit
    ...

## Release

```
make release VERSION=<vX.Y.Z>
```
