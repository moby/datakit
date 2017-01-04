(** Datakit CI

    [Datakit_ci] is a library to describe reproducible, cachable and
    distributed computations. The library is specialiazed to handle
    build pipelines, where computations can take time, and where
    seeing the live outputs of the computation is as useful as the
    final step (think of live-logs of long-running builds).

    The {!core} of [Datakit_ci] are {{!Term}term}combinators to
    describe the composition of long-running computation jobs, with an
    attached, individual {{!Output}output} (which can be
    {{!livelogs}live}). A project {{!config}configuration} tied
    together a collection of job description and a simple description
    for the {{!web}web} interface, whose access is granted using
    {{!acl}ACLs}.

    TODO.
*)

(** {1:core Core} *)

module DK: Datakit_S.CLIENT with type error = Protocol_9p_error.error
(** Datakit client library. *)

module Live_log: sig

  (** {1:livelogs Live Logs} *)

  type manager
  (** The type for live-log manager. *)

  type t
  (** The type for live-logs. *)

  val create:
    ?switch:Lwt_switch.t -> pending:string -> branch:string -> title:string ->
    manager -> t
  (** [create ~pending ~branch ~title manager] is a fresh, empty log
      with pending reason [pending].  It is an error to have two live
      logs on the same branch at the same time (finish the other one
      first). *)

  val finish: t -> unit
  (** [finish t] prevents any further changes and notifies anyone
      waiting on [pending]. *)

  val write: t -> string -> unit
  (** [write t msg] appends [msg] to the log. *)

  val printf: t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [printf t fmt] appends a formatted message to the log. *)

  val log: t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [log t fmt] appends a formatted message to the log, with a
      newline added at the end. *)

  val heading: t -> ('a, Format.formatter, unit, unit) format4 -> 'a
  (** [heading t fmt] appends a formatted message to the log as a
      heading. *)

  val with_pending_reason: t -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_pending_reason t msg fn] calls [fn ()]. If it gets a
      sleeping thread, then it pushes [msg] onto the pending-reason
      stack, waits for the thread to finish, and then removes the
      pending message. *)

  val enter_with_pending_reason: t -> string -> (('a -> 'b Lwt.t) -> 'b Lwt.t)
    -> ('a -> 'b Lwt.t) -> 'b Lwt.t
  (** [enter_with_pending_reason t msg use fn] is like [use fn], but
      posts [msg] as the pending reason until [fn] is called (or [use]
      fails).  This is useful to give a pending reason while getting a
      mutex or pool resource. *)

  val contents: t -> string
  (** [contents t] is the current contents of the buffer. *)
end

module Output: sig

  (** {1:output Computation Output} *)

  (** The type for {!term} output saved in Datakit. *)
  type saved

  (** The type for {!term}'s output. *)
  type t =
    | Empty
    | Live of Live_log.t
    | Saved of saved
    | Pair of t * t

end

type 'a status = {
  result: ('a, [`Pending of string * unit Lwt.t | `Failure of string]) result;
  output: Output.t;
}
(** The type for term status. It is a mix between the usual error
    monad, but where we also keep a local log for every
    computation. Morever, computation can be long-running, so there is
    a new [`Pending] state and a continuation to run when the term
    complete. *)

type job_id
(** The type for job IDs. They are used to identfy the worker actually
    doing the computation. *)

module Target: sig

  (** {1:target Job Targets} *)

  open Datakit_github

  type t = [ `PR of PR.id | `Ref of Ref.id ]
  (** The type for computation targets. A target can either be a
      pull-request ID, e.g. a GitHub repository and a number; or a
      reference ID, e.g. a GitHub repository and a reference name
      (given as a list of string, e.g. ["heads/master"] should be
      split into [["heads"]; ["master"]]). *)

  val compare: t -> t -> int
  (** [compare] is the comparison function for GitHub targets. *)

  val repo: t -> Repo.t
  (** [repo t] is [t]'s repository. *)

  val id: t -> [`PR of int | `Ref of string list]
  (** [id t] is [t]'s ID, e.g either a pull-request number or a
      reference name split on ['/']. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for GitHub targets. *)

  type v = [ `PR of PR.t | `Ref of Ref.t ]
  (** The type for resolved GitHub targets. Resolved pull-request and
      references contains the head commit and other metadata (see
      {!Datakit_github}'s documentation for more details). *)

  val head: v -> Commit.t
  (** [head v] is the head commit of [v]. *)

  val compare_v: v -> v -> int
  (** [compare_v] compares values of type {!v}. *)

end

module Term: sig

  (** {1:term Computation Terms}

      A term is a unit of computation, which can have various state
      and a log output. Terms can be short-lived or long-running.  *)

  type 'a t
  (** The type for computation terms. A ['a t] is a term that
      evaluates to an ['a], fails, or explains what it's waiting
      for. *)

  val return: 'a -> 'a t
  (** [return x] is a term that evaluates successfully to [x]. *)

  val fail: ('a, Format.formatter, unit, 'b t) format4 -> 'a
  (** [fail fmt] is a term that fails with message [fmt]. *)

  val pending: ('a, Format.formatter, unit, 'b t) format4 -> 'a
  (** [pending fmt] is a term that reports it is waiting because of
      reason [fmt]. *)

  val state: 'a t -> ('a, [`Pending of string | `Failure of string]) result t
  (** [state x] immediately and successfully returns the current state
      of [x]. *)

  val of_state: ('a, [< `Pending of string | `Failure of string]) result -> 'a t
  (** [of_state x] is a term which evaluates to [x]. *)

  val catch: 'a t -> ('a, [`Failure of string]) result t
  (** [catch x] successfully returns a result showing whether [x]
      succeeded or failed. *)

  val of_lwt_quick: 'a Lwt.t -> 'a t
  (** [of_lwt_quick x] evaluates to the result of [x]. Note that the
      result is never pending, so this is only for quick
      operations. *)

  val of_lwt_slow: (unit -> 'a status Lwt.t) -> 'a t
  (** [of_lwt_slow check] is a term that evaluates [check ()] to get
      the current status.  If [Error (`Pending (message, ready))],
      another check is scheduled when [ready] terminates. *)

  val join: 'a t t -> 'a t
  (** [join tt] is the term to which [tt] evaluates. *)

  val pair: 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] evaluates to the pair of the results of successfully
      evaluating [a] and [b] (in parallel). If [a] is not successful,
      the result is the same as [a]. Otherwise, if [b] is not
      successful then the result is the same as [b]. *)

  val without_logs: 'a t -> 'a t
  (** [without_logs t] evaluates to the same result as [t], but does
      not link to its logs. This is useful if [t]'s logs are already
      being reported as part of another job. *)

  module Infix: sig

    (** {1 Infix operators} *)

    val ( $ ): ('a -> 'b) t -> 'a t -> 'b t
    (** [fn $ x] is the result of applying the evaluation of the term
        [fn] to the evaluation of the term [x]. [fn] and [x] are
        evaluated in parallel. While [fn] is not successful, the
        result is the same as [fn].  Otherwise, if [x] is not
        successful then the result is the same as [x].  You can use
        [$] multiple times to support functions of multiple arguments
        (e.g. [fn $ a $ b $ c]).  Use with [join] if [fn] itself
        returns a term. *)

    val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
    (** [x >>= fn] (bind) is [fn y] where [y] is the successful result
        of evaluating [x], or simply [x] if [x] is not successful. *)

    val ( >|= ): 'a t -> ('a -> 'b) -> 'b t
    (** [x >|= fn] (map) is [return (fn y)] where [y] is the
        successful result of evaluating [x], or simply [x] if [x] is
        not successful. *)
  end

  val list_map_p: ('a -> 'b t) -> 'a list -> 'b list t
  (** [list_map_p fn l] will map the [l] list of terms to [fn] in
      parallel and return the result list when all of them have
      completed. *)

  val wait_for: 'a t -> while_pending:string -> on_failure:string -> unit t
  (** [wait_for t ~while_pending ~on_failure] evaluates successfully
      to unit if [t] evaluates successfully. It is pending with reason
      [while_pending] while [t] is pending, and fails with error
      [on_failure] if [t] fails.  This is useful in the case where one
      test case depends on another, to avoid reporting the same status
      twice. Consider wrapping with [without_logs] if [t]'s logs will
      be reported as part of another job. *)

  val wait_for_all: (string * 'a t) list -> unit t
  (** [wait_for_all xs] evaluates successfully to unit if all terms in
      the assoc list [xs] evaluate successfully. Otherwise, it is
      pending or failed with a suitable message, based on the names of
      the terms it is waiting for.  Consider wrapping with
      [without_logs] if [xs]'s logs will be reported as part of
      another job. *)

  val job_id: job_id t
  (** [job_id] evaluates to the ID of the worker that evaluates the
      term. This is useful for logging. *)

  (** {1 Datakit Connection} *)

  val dk: (unit -> DK.t Lwt.t) t
  (** [dk] is a function for getting the current DataKit
      connection. *)

  (** {1 GitHub Integration} *)

  val target: Target.t -> Target.v t
  (** [target id] evaluates to the full metadata of the named
      target. *)

  val head: Target.t -> Datakit_github.Commit.t t
  (** [head target] evaluates to the commit at the head [target]. *)

  val branch_head: Datakit_github.Repo.t -> string -> Datakit_github.Commit.t t
  (** [branch_head repo b] evaluates to the commit at the head of
      branch [b] in the repository [repo]. *)

  val tag: Datakit_github.Repo.t -> string -> Datakit_github.Commit.t t
  (** [tag repo t] evaluates to the commit of tag [t] in the
      repository [repo]. *)

  val ci_status: string list -> Target.t ->
    [`Pending | `Success | `Failure | `Error] option t
  (** [ci_status ci target] is the status reported by CI [ci] for
      [target].  Note that even if the CI is e.g. pending, this
      returns a successful result with the value [`Pending], not a
      pending result. *)

  val ci_target_url: string list -> Target.t -> Uri.t option t
  (** [ci_target_url ci target] is the target URL reported by CI
      [ci]. *)

  val ci_success_target_url: string list -> Target.t -> Uri.t t
  (** [ci_success_target_url ci target] is the URL of the *successful*
      build [ci].  It is pending until a successful URL is
      available. *)
end

module ACL: sig

  (** {1:acl Access Control List} *)

  type t
  (** The type for GitHub ACLs. *)

  val everyone: t
  (** [everyone] is the ACL granting access to everyone *)

  val username: string -> t
  (** [username n] is the ACL granting access to the user [n]. *)

  val github_org: string -> t
  (** [github_org o] is the ACL granting access to all the users of
      the [o] GitHub organisation. Note: currently, only public
      organisation membership is used. *)

  val can_read_github: string -> t
  (** [can_read "org/repo"] is the ACL granting access to users who
      can see the [org/repo] GitHub repository. *)

  val any: t list -> t
  (** [any l] is the ACL granting access to an user iff there is at
      least one ACL in [l] granting access to that user. *)

end

module Web: sig

  (** {1:web Web Configuration} *)

  type config
  (** The type for web configuration. *)

  (** [config ~name ~state_repo ~can_read ~can_build ()] is a web
      configuration. If [name] is given, it is used as the main
      heading, and also as the name of the session cookie (useful if
      you run multiple CIs on the same host, on different ports).  If
      [state_repo] is given, it is used to construct links to the
      state repository on GitHub. If [metrics_token] is [Some
      (`SHA256 expected)] given then doing an HTTP GET on [/metrics]
      with an Authorization header containing "Bearer TTT" will return
      Prometheus-format metrics if sha256(TTT) = expected. There is no
      rate limiting, so pick a long [token]. *)
  val config:
    ?name:string ->
    ?state_repo:Uri.t ->
    ?metrics_token:[`SHA256 of string] ->
    ?listen_addr:[`HTTP of int | `HTTPS of int] ->
    can_read:ACL.t ->
    can_build:ACL.t ->
    unit -> config

end

module Config: sig

  (** {1:config Configuration} *)

  type t
  (** The type for the project configuration. *)

  type project
  (** The type for projects. *)

  type test = string Term.t
  (** The type for individual tests. *)

  val project:
    id:string ->
    ?dashboards:string list ->
    (Target.t -> (string * test) list) ->
    project
  (** [project ~id tests] is the configuration for a single GitHub
      project. [tests] is a list tests to apply to branches, tags and
      open PRs within the project. [dashboards] (default
      [["master"]]) is a list of branches to display in the main
      dashboard area. *)

  val v: web_config:Web.config -> projects:project list -> t
  (** [v ~web_config ~project] describe a complete project
      configuration. *)
end

val run: ?info:Cmdliner.Term.info -> Config.t Cmdliner.Term.t -> unit
(** [run ?info config] runs DataKitCI. [info] defaults to a term that
    describes the binary as [DataKitCI], but does not include any of
    the other metadata such as versioning. Call {!Cmdliner.Term.info}
    directly to obtain an [info] value that exposes all this extra
    data on the resulting command line. *)

val logs: Live_log.manager
(** The singleton log manager. *)

(** {1:extensions Extensions} *)

module Process: sig

  (** {1:process Extension of Lwt_process}

      Convenience wrappers around [Lwt_process]. *)

  val run_with_exit_status:
    ?switch:Lwt_switch.t -> ?log:Live_log.t -> ?cwd:string ->
    ?env:string array -> output:(string -> unit) ->
    ?log_cmd:Lwt_process.command -> Lwt_process.command ->
    Unix.process_status Lwt.t
  (** Run [cmd], passing each chunk of output it produces on stdout or
      stderr to [output]. A copy of the output is also streamed to
      our stdout. Returns the exit status of the process when
      completed. If [log_cmd] is given, it is displayed in all log
      messages instead of [cmd].  This is useful to hide secret
      tokens, etc. *)

  val run:
    ?switch:Lwt_switch.t -> ?log:Live_log.t -> ?cwd:string ->
    ?env:string array -> output:(string -> unit) ->
    ?log_cmd:Lwt_process.command -> Lwt_process.command ->
    unit Lwt.t
  (** Run [cmd], passing each chunk of output it produces on stdout or
      stderr to [output]. A copy of the output is also streamed to
      our stdout. Raises an exception if the process doesn't return
      an exit status of zero. *)

  val check_status: Lwt_process.command -> Unix.process_status -> unit
  (** [check_status cmd status] checks that [status] is a successful
      exit status.  If not, it raises an exception giving [cmd] as the
      cause. *)
end

module Monitored_pool: sig

  (** {1:pool Monitored Pool of Workers} *)

  type t
  (** The type for pools of workers. *)

  val create: string -> int -> t
  (** [create s n] create a new pool with [n] workers. *)

  val use: t -> ?log:Live_log.t -> ?label:string -> job_id ->
    (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [use t job fn] evaluates [fn ()] with one pool resource held.
      [job] (and [label]) will be displayed as the reason why the
      resource is in use. If [log] is provided then a message will be
      logged if we have to wait, and if the log is cancellable then
      the user will be able to cancel the operation. *)
end

module Git: sig

  (** {1:git Git Manipulation}

      [Git] extends {{!term}Term} with Git-related combinators.It
      defines types for {{!t}local Git repositories}, {{!commit}Git
      commits} inside these repositories and {{!command}shell command}
      to run on a given checkout.  *)

  type t
  (** The type for local non-bare git working directory (with a .git
      sub-directory). *)

  val v: ?remote:string -> logs:Live_log.manager -> string -> t
  (** [v ~remote ~logs dir] is the local Git repository at [dir].
      If [dir] does not exist, it is created by [git clone remote].
      If [remote] is not given and [dir] does not exist, an exception
      is raised. *)

  type commit
  (** The type for Git commits. *)

  val hash: commit -> string
  (** [hash c] is [c]'s hash. *)

  val is_after: old:string -> commit -> bool Lwt.t
  (** [is_after ~old c] is true iff [old] appears in the history of
      [c]. *)

  val fetch_head: t -> Target.t -> commit Term.t
  (** [fetch_head t target] evaluates to a local branch in [t] with a
      copy of [target]'s head commit (downloading it first if
      needed). *)

  val with_checkout:
    log:Live_log.t -> job_id:job_id -> commit -> (string -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_checkout ~log ~job_id commit fn] is [fn path], where [path]
      is the path of a local checkout of [commit]. [path] must not be
      used after [fn]'s thread terminates. The directory is locked while
      [fn] runs with [job_id] displayed to show why it is busy. *)

  val with_clone:
    log:Live_log.t -> job_id:job_id -> commit -> (string -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_clone] is similar to [with_checkout] but clones the
      repository to a temporary directory first. This means that the
      repository does not need to be locked while the callback function
      runs. *)

  type command
  (** A cache of executions of a shell command. *)

  val command:
    logs:Live_log.manager -> timeout:float -> label:string -> clone:bool ->
    string array list -> command
  (** [command ~logs ~timeout ~label ~clone cmds] is a caching executor
      of [cmds], which caches results in a branch whose name includes
      [label]. The command will be terminated if it exceeds [timeout]
      seconds. If [clone] is set then [with_clone] is used rather than
      [with_checkout]. Note: every command must have a unique label! *)

  val run: command -> commit -> unit Term.t
  (** [run cmd commit] succeeds if running [cmd] in a checkout of
      [commit] returns an exit status of zero. *)

end

(** {1:cache Cache} *)

module type BUILDER = sig

  (** {1 Builder} *)

  type t
  (** The type for builder values. A builder generates values from
      inputs (keys). A builder is typically used with a
      {{!Cache}cache}. *)

  module Key: Map.OrderedType
  (** Input describing what is to be built. *)

  type context
  (** For passing context parameters to the builder which aren't part
      of the key (e.g. timeouts or resource pools). *)

  type value
  (** Output of the builder. *)

  val name: t -> string
  (** A unique name for this builder. This is used for metric
      reporting. *)

  val title: t -> Key.t -> string
  (** [title t key] is a one-line summary of the operation that is
      performed by [generate]. It is used as the reason string for
      the pending state and as the title of the log. *)

  val generate: t -> switch:Lwt_switch.t -> log:Live_log.t ->
    DK.Transaction.t -> context -> Key.t ->
    (value, [`Failure of string]) result Lwt.t
  (** [generate t ~log trans ctx key] generates the value for [key]
      and stores it in [trans] under the "value" directory. It is
      called when the value is not in the cache, or when a rebuild has
      been requested. The value returned must be the same as the value
      that would be loaded by [load]. If the build is cancelled,
      [switch] will be turned off. If [generate] throws an exception
      then it is caught and treated as an error result. *)

  val load: t -> DK.Tree.t -> Key.t -> value Lwt.t
  (** [load t tr key] is the value [v] previously saved by
      [generate]. *)

  val branch: t -> Key.t -> string
  (** The name of the DataKit branch on which to store the result for
      this key.  If the branch exists and is up-to-date then we use
      that. *)
end

module Cache: sig

  (** {1:cache Cache}

      A cache for values computed (slowly) by terms. *)

  module Path: sig
    val value: Datakit_path.t
    (** Build results are stored in this directory *)
  end

  module Make(B: BUILDER): sig
    type t
    (** A [t] is a cache of values created by [B]. *)

    val create: logs:Live_log.manager -> B.t -> t
    (** [create ~logs b] is a fresh cache that maps keys of type
        [B.Key.t] to values of type [B.value]. *)

    val lookup:
      t -> (unit -> DK.t Lwt.t) -> rebuild:bool -> B.context -> B.Key.t ->
      B.value status Lwt.t
    (** [lookup t conn ~rebuild ctx key] returns the cached value of
        [key], or uses [B.generate ctx key] to start the process of
        calculating the value if this is the first time [key] has been
        requested. If [rebuild] is [true] then any complete cached
        result is ignored (we mark the result branch as needing a
        rebuild and build again anyway). *)

    val find: t -> B.context -> B.Key.t -> B.value Term.t
    (** [find t key] is the terms which evaluates to the result of
        looking up [key] in the cache (using [lookup]). *)
  end
end


(** {1 Utils}

    This should probably be replaced by something else or not be
    exposed at all. *)

module Utils: sig


  module Infix: sig

    val ( >>*= ):
      ('a, [< `Msg of string ]) result Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

    val ( >|*= ):
      ('a, [< `Msg of string ]) result Lwt.t -> ('a -> 'b) -> 'b Lwt.t

  end

  val chdir_lock: Lwt_mutex.t

  val ok: 'a -> ('a, 'b) result Lwt.t

  val return_error:
    ('a, Format.formatter, unit, ('b, string) result Lwt.t) format4 -> 'a

  val failf: ('a, Format.formatter, unit, 'b) format4 -> 'a

  val pp_exn: exn Fmt.t

  val with_timeout:
    ?switch:Lwt_switch.t -> float -> (Lwt_switch.t -> 'a Lwt.t) -> 'a Lwt.t

  val abs_path: string -> string

  val ensure_dir: mode:Unix.file_perm -> string -> unit

  val default: 'a -> 'a option -> 'a

  val with_tmpdir:
    ?prefix:string -> ?mode:Unix.file_perm -> (string -> 'a Lwt.t) -> 'a Lwt.t

  val ls: string -> string list Lwt.t

  val with_switch: (Lwt_switch.t -> 'a Lwt.t) -> 'a Lwt.t

  val cancel_when_off: Lwt_switch.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

(** / **)

module Private: sig
  (* This is only exposed for the unit-tests. *)

  type engine

  module Client9p: sig
    include Protocol_9p_client.S
    val connect:
      string -> string ->
      ?msize:int32 -> ?username:string -> ?aname:string -> ?max_fids:int32 ->
      unit -> t Protocol_9p_error.t Lwt.t
  end

  val connect: Client9p.t -> DK.t

  val test_engine: web_ui:Uri.t -> (unit -> DK.t Lwt.t) ->
    (Target.t -> string Term.t Astring.String.Map.t)
      Datakit_github.Repo.Map.t ->
    engine

  val listen: ?switch:Lwt_switch.t -> engine -> [`Abort] Lwt.t

  val create_logs: unit -> Live_log.manager
  val lookup_log: branch:string -> Live_log.manager -> Live_log.t option
  val cancel: Live_log.t -> (unit, string) result Lwt.t
  val read_log: DK.t -> Output.saved -> string DK.or_error Lwt.t

end
