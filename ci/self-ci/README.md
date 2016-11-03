The CI configuration for testing DataKit itself, using DataKitCI.

To use this as a template for your own projects:

1. Edit `docker-compose.yml`.
   - For the `ci` service:
     - Change `--web-ui=https://datakit.ci:8446/` to the URL users should use to see the web user interface of your service.
     - Adjust the first port in `ports` if you change the port.
   - For the `datakit` service:
     - Edit (or remove) the `--auto-push git@github.com:docker/datakit.logs` option to point at a new, empty, GitHub repository
       which will mirror the results.
   - For the bridge, change `--webhook http://datakit.ci:81` to a public endpoint that GitHub can use to send web events.
     If you change the port, change *both* ports in the `ports` configuration below.

2. Edit `selfCI.ml` to specify the tests you require. See the [DataKitCI][] README for details.

3. Populate the `data/bridge` directory as follows:
   - `data/bridge/_github/jar/datakit` should contain your GitHub token, allowing the bridge to access GitHub. See [ocaml-github][]'s README for details. Get a token with `git jar` and copy your `~/.github` directory as `data/bridge/_github`.

4. Populate `data/ci`:
  - `data/ci/repos/foo` should be a `git clone` of some repository "foo" you want to test. The CI will `git pull` here to get updates.
  - `data/ci/secrets/server.crt` and `ci/secrets/server.key` will be generated on first run if missing. They are used for the web UI. You can replace these with a proper certificate and key when you get one (e.g. using [certbot][]).
  - `data/ci/secrets/passwords.sexp` will contain your user's web UI passwords (hashed). You will be prompted to choose a password on first use if this is missing, but you'll need to run the `ci` container once in interactive mode so you can enter the password.

5. Populate `data/datakit`:
  - `data/datakit/data/.git` should be a bare, empty, Git repository (create with `git init --bare datakit/data/.git`).
  - `data/datakit/_ssh` should contain a fresh SSH key that DataKit can use to `git push` if you configured `--auto-push` above, plus a `known_hosts` file so it can recognise GitHub. The easiest way to set this up is to `docker-compose exec datakit bash` and check you can `git push` from the `/data` directory.

6. `docker-compose up` to get everything running.

7. Commit an empty `.monitor` file to the `github-metadata` branch of `data/datakit/data` for every repository you want to monitor. e.g. to monitor `docker/datakit`, we create the file `docker/datakit/.monitor`. As soon as this is committed, the `bridge` service should start populating the branch with information about the branches, tags and open PRs in the repository and the CI will start testing them.

[DataKitCI]: https://github.com/talex5/datakit/tree/self-ci/ci
[ocaml-github]: https://github.com/mirage/ocaml-github
[certbot]: https://certbot.eff.org/
