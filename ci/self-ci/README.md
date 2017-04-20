The CI configuration for testing DataKit itself, using DataKitCI.
The `docker-compose.yml` file describes a configuration for testing the CI locally with `docker-compose`.
The `datakit-ci.yml` file describes the configuration we use to run <https://datakit.datakit.ci>, managed by `docker stack`.

# Local testing

To test it locally, use:

```
$ docker-compose up
ci_1       | 2017-01-23 14:15.55 APP [datakit-ci] >>> Configure the CI by visiting
ci_1       |                                      http://localhost:8080/auth/intro/...
```

Visit the URL shown to configure an admin user.

In this configuration:

- The bridge that normally syncs the CI state with GitHub is replaced by `datakit/local-bridge`, which tracks the local DataKit Git repository (`../../.git`).
- Only the master branch is tested (`--canary=moby/datakit/heads/master`).
- Plain HTTP connections are used, to avoid browser warnings about self-signed certificates when testing.
- The main executable is called with `--profile=localhost`, which affects some settings in `selfCI.ml` (search for `Localhost` to find the changes).

This mode is useful for testing changes to the CI itself, or for testing your changes before making a public PR.


# Docker Cloud / Swarm Mode configuration

To use this as a template for your own projects:

1. Edit `datakit-ci.yml`.
   - For the `ci` service:
     - Change `--web-ui=https://datakit.datakit.ci/` to the URL users should use to see the web user interface of your service.
   - For the `datakit` service:
     - Edit (or remove) the `--auto-push git@github.com:moby/datakit.logs` option to point at a new, empty, GitHub repository
       which will mirror the results.
   - For the bridge, change `--webhook http://HOST:PORT` to a public endpoint that GitHub can use to send web events.
     If you change the port, change *both* ports in the `ports` configuration below.

2. Edit `selfCI.ml` to specify the tests you require. See the [DataKitCI][] README for details.

3. Add the token that the bridge will use to access GitHub.
   Get a token with `git jar` and add it as a Docker secret with:
   `docker secret create datakit-github-cookie - < ~/.github/jar/datakit-github-cookie`
   See [ocaml-github][]'s README for details.

4. Use `docker stack deploy self-ci -c datakit-ci.yml` to deploy the stack.

5. Check the logs for the `ci` service. You should see a configuration URL displayed near the start.
   Open this in a browser (you'll probably have to click through a security warning, as the server
   generates itself a self-signed X.509 certificate by default).

5. Configure an admin password when prompted, then log in as the new "admin" user.

You will need to add some SSH keys and (optionally) X.509 certificates:

1. Populate the `datakit-ssh` volume with a fresh ssh key (run `ssh-keygen`).
   DataKit can use this to `git push` if you configured `--auto-push` above.
   You'll also need a `known_hosts` file so it can recognise GitHub.
   The easiest way to set this up is to run `git push` manually once.

2. Restore the `datakit-public-data` volume (optional).
   If you are restoring the database from a backup, use `git clone --bare --mirror backup`.

3. Replace the X.509 certificates in the `ci-secrets` volume (optional).
   `server.crt` and `server.key` will be generated on first run if missing.
   They are used for the web UI. You can replace these with a proper certificate and key when you get one (e.g. using [certbot][]).

On startup, the CI should commit to the `datakit-public-data` repository's `github-metadata` branch a request to monitor the projects it is testing.
The `bridge` service should then start populating the branch with information about the branches, tags and open PRs in the repository, and the CI will start testing them.

## Prometheus metrics

All the DataKit services are run with `--listen-prometheus=9090`, which means that they will provide Prometheus metrics on port 9090 at `/metrics`. You can configure a Prometheus server to monitor these ports.

[DataKitCI]: https://github.com/moby/datakit/tree/master/ci/self-ci
[ocaml-github]: https://github.com/mirage/ocaml-github
[certbot]: https://certbot.eff.org/
