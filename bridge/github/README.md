## DataKit-GitHub bridge

The bridge monitors the state of one or more GitHub projects, writing the status (open PRs, branches and tags) to a DataKit branch.
It also monitors the branch and writes back any changes to GitHub.


### Build

Build using the `Dockerfile.github` file at the root of this repository:

    docker build -t datakit-github -f Dockerfile.github .

### Run

To see the help text:

    docker run -it --rm datakit-github --help

Create a GitHub API token:

    docker run -it --rm \
      -v /path/to/jar:/home/opam/.github/jar \
      --entrypoint opam \
      -u opam \
      datakit-github \
      config exec \
      git jar make my-user datakit

Replace `/path/to/jar` with the path of your new directory.
Replace `my-user` with your GitHub user name.

Using the GitHub web interface, edit the token to give it the `repo`, permission.
Also, ensure the user is an `admin` in the `Collaborators` settings.

Start a DataKit server running somewhere:

    mkdir test-store
    git init test-store/.git --bare
    datakit --git test-store --url tcp://127.0.0.1:6640 -v


To run it:

    docker run -it --rm \
      -v /path/to/jar/datakit:/run/secrets/datakit-github-cookie \
      datakit-github \
      --datakit=tcp:x.x.x.x:6640 \
      --verbose \
      --webhook=http://my-ip

Note: `/path/to/jar/datakit` MUST NOT have any "other" permissions set in its Unix permissions.
Otherwise, the bridge will refuse to start, saying that the file doesn't exist.

Replace:
- `/path/to/jar` with the path of your jar directory.
- `tcp:x.x.x.x:6640` with the path to your DataKit server.
- `http://my-ip` with a URL which GitHub can use to send events to the bridge.

### Start monitoring a repository

Connect to DataKit and create an empty file `repo/project/.monitor` on the `github-metadata` branch.
The bridge will immediately start querying GitHub and will populate the directory with information about the project.
