The CI configuration for testing DataKit itself, using DataKitCI.

To use this as a template for your own projects:

1. Edit `datakit-ci.yml`.
   - For the `ci` service:
     - Change `--web-ui=https://datakit.ci:8446/` to the URL users should use to see the web user interface of your service.
     - Adjust the first port in `ports` if you change the port.
   - For the `datakit` service:
     - Edit (or remove) the `--auto-push git@github.com:docker/datakit.logs` option to point at a new, empty, GitHub repository
       which will mirror the results.
   - For the bridge, change `--webhook http://HOST:PORT` to a public endpoint that GitHub can use to send web events.
     If you change the port, change *both* ports in the `ports` configuration below.

2. Edit `selfCI.ml` to specify the tests you require. See the [DataKitCI][] README for details.

3. Use `docker-cloud` to run the stack. Note that the bridge will probably fail to start as we haven't configured its GitHub token yet, but this will still create the volume in which we'll place the key.

4. Check the logs for the `ci` service. You should see a configuration URL displayed near the start.
   Open this in a browser (you'll probably have to click through a security warning, as the server
   generates itself a self-signed X.509 certificate by default).

5. Configure an admin password when prompted, then log in as the new "admin" user.

You will need to populate some data volumes of each service. This is a little tricky at present (but should become easier with Docker 1.13, which has built-in secrets management). For now, run a new container and mount the volumes of the container you want to edit. e.g.

```
$ ssh root@node.ip
root@node.ip# docker ps -a |grep datakit-ci
ab63056e5ac8        docker/datakit:latest                   "/usr/bin/datakit --g"   17 hours ago        Up 17 hours         5640/tcp                       datakit-1.datakit-ci.dd221543
c38556e34d04        editions/datakit-self-ci:latest         "/datakit-ci/datakit-"   17 hours ago        Up 17 hours         0.0.0.0:443->8443/tcp          ci-1.datakit-ci.bc411712
d17715d9f139        docker/datakit:github                   "/usr/bin/datakit-git"   18 hours ago        Up 14 hours         0.0.0.0:81->81/tcp, 5641/tcp   bridge-1.datakit-ci.5d4faf6f
f8e964c596e0        redis:latest                            "docker-entrypoint.sh"   5 days ago          Up 5 days           6379/tcp                       redis-1.datakit-ci.c489e542
root@node.ip# docker run --rm -it --volumes-from bridge-1.datakit-ci.5d4faf6f busybox
busybox$ ls /root/.github
jar
```

1. To populate `bridge`'s `/root/.github/jar` directory,
   get a token with `git jar` and copy your `~/.github` directory as `/root/.github`.
   This allows the bridge to access GitHub. See [ocaml-github][]'s README for details.

2. Populate `ci`'s `/data` directory:
  - `/secrets/server.crt` and `/secrets/server.key` will be generated on first run if missing. They are used for the web UI. You can replace these with a proper certificate and key when you get one (e.g. using [certbot][]).

3. Populate `datakit`'s `/root/.ssh` directory with a fresh ssh key (run `ssh-keygen`).
   DataKit can use to `git push` if you configured `--auto-push` above.
   Initialise `datakit`'s `/data/.git` directory to be a bare, empty, Git repository (create with `git init --bare datakit/data/.git`).
   If you are restoring a service from a backup, use `git clone --bare --mirror backup`.
   You'll also need a `known_hosts` file so it can recognise GitHub. The easiest way to set this up is to run `git push` manually once.

4. Redeploy the stack (if docker cloud offers to delete all your volumes at this point, you should decline its offer).

On startup, the CI should commit to the `data/datakit/data` repository's `github-metadata` branch a request to monitor the projects it is testing.
The `bridge` service should then start populating the branch with information about the branches, tags and open PRs in the repository, and the CI will start testing them.

## Prometheus metrics

To enable metrics monitoring, generate a long random token and store it in a file called `metrics_token` (in the same directory as `selfCI.ml`:

```
$ TOKEN=$(pwgen 20 1)
$ echo $TOKEN
Uot1boh6urae8ei2AhNg
$ python -c 'import sys, hashlib, base64; print base64.b64encode(hashlib.sha256(sys.argv[1]).digest())' $TOKEN > metrics_token
```

Then, configure your Prometheus instance to monitor the CI service by adding this to your `prometheus.yml`:

```
  - job_name: 'ci'
    scheme: https
    bearer_token: Uot1boh6urae8ei2AhNg
    static_configs:
      - targets: ['example.com:8443']
```

(set `bearer_token` to the value of `$TOKEN` printed above, and change `example.com` to the address of your CI web interface)


[DataKitCI]: https://github.com/talex5/datakit/tree/self-ci/ci
[ocaml-github]: https://github.com/mirage/ocaml-github
[certbot]: https://certbot.eff.org/
