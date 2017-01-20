## DataKit Local-Git bridge

This service is a drop-in replacement for the DataKit-GitHub bridge that instead just monitors a local Git repository.
It is useful for testing a new DataKitCI configuration without having to configure GitHub integration first.

The local bridge monitors the state of one or more local Git repositories, writing the current head of each branch to DataKit.
DataKitCI can be configured to run the CI tests against the project each time a commit is made.

Once you are happy with the way the CI is working, you can replace this service with the GitHub bridge service to have the CI test a project hosted on GitHub instead.

Unlike the GitHub bridge, this service:

- only reports on branches, not tags or pull requests;
- does not report build statuses from other CI systems; and
- does not push the statuses set by the CI anywhere.

For an example test configuration using this bridge, see `ci/self-ci/docker-compose.yml`.


### Build

Build using the `Dockerfile.bridge-local-git` file at the root of this repository:

    docker build -t docker/datakit:bridge-local-git -f Dockerfile.bridge-local-git .

### Run

To see the help text:

    docker run -it --rm docker/datakit:bridge-local-git --help

To run it (after starting a DataKit container called "datakit"):

    docker run -it --rm \
      --link datakit:datakit \
      -v /path/to/repos:/repos \
      docker/datakit:bridge-local-git -v \
      me/my-project:/repos/my-project \
      --verbose \
      --webhook=http://my-ip

Replace:
- `/path/to/repos` with the path to your local repository or repositories.
- `me/my-project` (simulating the GitHub `http://github.com/my/my-project` repository) with the ID of your project.
