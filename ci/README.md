# DataKitCI

DataKitCI is a continuous integration service that monitors your GitHub project and tests each branch, tag and pull request.
It displays the test results as status indicators in the GitHub UI.
It keeps all of its state and logs in [DataKit][], rather than a traditional relational database, allowing review with the usual Git tools.

A complete deployment is made up of several components:


    GitHub  <-------------------- User
      ^                            |
      |                            |
      |                            |
      V                            V
    GitHub ----> DataKit <----- DataKitCI
    bridge

1. Users create and update pull requests on GitHub.
2. A GitHub bridge service monitors these events and records them in DataKit.
3. DataKitCI monitors DataKit and runs the tests.
4. The user can monitor the progress of the tests with DataKitCI's web interface.
5. DataKitCI writes status updates to DataKit.
6. The GitHub bridge pushes the status updates back to GitHub.

Since DataKitCI does not interact with GitHub directly, you can always turn off the bridge and test DataKitCI against a local DataKit to see what it would do without affecting your GitHub project.
When you're happy with the results, simply start the GitHub bridge to sync everything again.


## Installation

The `self-ci` directory contains the configuration we use to test DataKit itself.
`self-ci/README.md` explains how to use Docker Compose to deploy your own instance.

The `skeleton` directory contains a very minimal example for this tutorial.
You can build the example using Docker:

    cd ci/skeleton
    docker build -t my-ci --build-arg CONFIG=exampleCI .

`--build-arg CONFIG=exampleCI` tells it to use the configuration in the file `exampleCI.ml`.
See below for information on writing configuration files.

Then run your new image:

    docker run --name=my-ci -p 8443:8443 my-ci --metadata-store tcp:127.0.0.1:5640

The arguments are:

- `--name=my-ci` is the name Docker will give to this container.
- `-p PORT:8443` tells Docker to expose port 8443 (the https web UI) on host port `PORT`.
- `my-ci` is the image you just built.
- `--metadata-store tcp:HOST:PORT` is the address of your running DataKit server.

On the first run, you will see a log message containing a setup URL, e.g.

```
2016-12-19 16:32.47 APP [datakit-ci] >>> Configure the CI by visiting
                                     https://127.0.0.1:8443/auth/intro/XK2qPqmIGnc3_VG1OQ_EDg==
```

To view your new service, open this URL in a web browser.

Note: The server automatically generates an X.509 certificate for itself on the first run.
Since this is self-signed, you'll probably have to click through some kind of security warning.
If you wish to use your own key and certificate, find the container's `secrets` volume, replace the files (`server.key` and `server.crt`) and restart.

Once the CI loads, it will prompt you to choose a password for the "admin" user.
Enter a password and then log in as your new user.

## Writing tests

DataKitCI is configured by writing a build expression.
Copy `exampleCI.ml` as `configCI.ml` and edit the copy.

The value `projects` is a [cmdliner][] term that describes how to create the configuration by parsing command-line arguments.
The result is a list of named projects and, for each one, a list of named tests (e.g. "build", "docs", etc).
Each test is of type `string Term.t`.
An `'a Term.t` is a term that can be evaluated in the context of a GitHub pull request or branch to produce a value of some type `'a`.
In this case, the `string` is the message to display on success (e.g. "All tests passed!").

Here's a trivial example:

```ocaml
(* An example test that just always returns success. *)
let my_test =
  Term.return "Success!"

(* The configuration for a project that has a single test called "my-test". *)
let my_project = [
  "my-test", my_test;
]

(* A list of GitHub projects to monitor and the tests to apply to the open PRs and branches in each one. *)
let my_projects = [
  "me/my-project", my_project;
]

(* Parsing of command-line options (none in this example). *)
let projects =
  Cmdliner.Term.pure my_projects

(* The main entry-point *)
let () =
  DataKitCI.Main.run projects
```

This parses no command-line arguments (it is a "pure" cmdliner value) and applies `my_test` to every open PR and branch in "https://github.com/me/my-project" (change `me/my-project` to your own project path, of course).

You can compile your new configuation with:

    docker build -t my-ci --build-arg CONFIG=configCI .

If you run DataKitCI now, you should see all your PRs reporting success for the "unit-tests" test on GitHub and in the web-UI (currently <https://localhost:8443>).

A Term can evaluate to a successful result (as here), to a failure, or to pending. e.g.

```ocaml
let my_test = Term.fail "Test failed!"
```

Note that DataKitCI keeps all open PRs up-to-date, so restarting DataKitCI after changing to the second example will switch all of your open PRs from success to failure.
You can set the status to pending in a similar way:

```ocaml
let my_test = Term.pending "Running your test now..."
```

More usefully, the result can depend on the pull request...

## The DKCI_git plugin

One very important term is `DKCI_git.fetch_head`, which fetches the PR's head commit to a local git repository, ready for testing:

```ocaml
open Term.Infix   (* Provides the >>= operator *)

let local_repo = DKCI_git.connect ~dir:"/tmp/example"

let my_test =
  DKCI_git.fetch_head local_repo >>= fun local_commit ->
  Term.return "Fetched head commit successfully"
```

The `>>=` operator is used to combine terms.
`x >>= f` first evaluates `x`.
If `x` is a success value then it evaluates to `f x`, otherwise it just reports the current status (pending or error) of `x`.

In this case, we `git fetch` the head commit into the `/tmp/example` repository (which must already exist as a clone of the remote repository).
This term will be pending while the `git fetch` is in progress and will then report success.

Use `DKCI_git.command` to configure a command to run on the commit (e.g. `make`) and use `DKCI_git.run` to execute it, e.g.

```
let one_hour = 60. *. 60.

let make = DKCI_git.command ~logs ~label:"make" ~timeout:one_hour ~clone:true [
    [| "make"; "build" |];
    [| "make"; "test" |];
  ]

let my_test =
  DKCI_git.fetch_head local_repo >>= fun src ->
  DKCI_git.run make src >>= fun () ->
  Term.return "Tests succeeded"
```

## Parallel execution

To evaluate two terms in parallel, use `Term.pair`:

```
let combine a b =
  Printf.sprintf "a=%d and b=%d" a b

let my_test =
  let a = Term.return 1 in
  let b = Term.return 2 in
  Term.pair a b >>= fun (a, b) ->
  Term.return (combine a b)
```

You can also use `$` (also from `Term.Infix`) to evaluate multiple terms in parallel and combine them. e.g.

```ocaml
let my_test =
  let a = Term.return 1 in
  let b = Term.return 2 in
  Term.return combine $ a $ b
```

This allows you to run various downloading, building and testing operations in parallel where possible.

## GitHub login

You can configure the CI to allow users to authenticate using their GitHub accounts using the `Settings` tab.

Once GitHub authentication is configured, you can write policies that depend on GitHub permissions. e.g.

    ~can_read:ACL.(
      any [
        username "admin";
        can_read_github "my-org/my-private-project";
      ]
    )

This will allow read access to the CI if the user is the local "admin" user, or
is a GitHub user who can read the "my-org/my-private-repository" repository.


## Extending DataKitCI

Various other terms are available. See the `src/dataKitCI.mli` and `src/dKCI_git.mli` files for details.
Other plugins are under development.
To make your own, you need to implement the `BUILDER` interface.
Consult the API documentation and the Git plugin example for more information.

[DataKit]: https://github.com/docker/datakit
[cmdliner]: http://erratique.ch/software/cmdliner/doc/Cmdliner
