The scripts in this directory are used to run continuous integration tests on Datakit, using Datakit.

They assume that the Docker host has mounted the Datakit source repository on /srv/datakit and shared this directory with the container. The scripts monitor and build this local `master` branch. To test a commit (force) push it to there. Each build result appears as a new Git branch.

e.g. a typical workflow currently looks like:

    git commit -a -m 'My commit'
    git push datakit HEAD:master
    ...
    git fetch datakit
    gitk --all

TODO:

- We should monitor GitHub and build all PRs, not just our local branch.
- There should be a Datakit Docker volume plugin to set the 9p mount up automatically.
