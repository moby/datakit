open Datakit_github

(** Datakit/CI

    DatakitCI is a library to build CI systems. FIXME.
*)

open Astring

type job_id

module DK : Datakit_S.CLIENT with type error = Protocol_9p_error.error

module Live_log: sig

  (** {1 Live Logs} *)

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

module Github_hooks: sig

  module CI: sig
    type t = string list
    val circle_ci: t
    val datakit_ci: string -> t
  end
end

module Step_log: sig
  type saved

  type t =
    | Empty
    | Live of Live_log.t
    | Saved of saved
    | Pair of t * t
end

type 'a lwt_status =
  ('a, [`Pending of string * unit Lwt.t | `Failure of string]) result
  * Step_log.t
(** ['a lwt_status] is similar to ['a or_error], except that the
    pending state also indicates when it should be checked again. *)

module Target: sig
  type t = [ `PR of PR.id | `Ref of Ref.id ]
  val compare: t -> t -> int
  val repo: t -> Repo.t
  val id: t -> [`PR of int | `Ref of string list]
  val pp: t Fmt.t
  type v = [ `PR of PR.t | `Ref of Ref.t ]
end

module Term: sig

  (** {1 Term} *)

  type 'a t
  (** An ['a t] is a term that evaluates to an ['a], fails, or
      explains what it's waiting for. *)

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

  val of_lwt_slow: (unit -> 'a lwt_status Lwt.t) -> 'a t
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
  (** [job_id] evaluates to the job that evaluates the term.  This is
      useful for logging. *)

  val github_target: Target.t -> Target.v t
  (** [github_target id] evaluates to the GitHub metadata of the named
      target. Note that this is a snapshot. *)

  val head: Target.t -> Commit.t t
  (** [head target] evaluates to the commit at the head [target]. *)

  val branch_head: Repo.t -> string -> Commit.t t
  (** [branch_head repo b] evaluates to the commit at the head of
      branch [b] in the repository [repo]. *)

  val tag: Repo.t -> string -> Commit.t t
  (** [tag repo t] evaluates to the commit of tag [t] in the
      repository [repo]. *)

  val dk: (unit -> DK.t Lwt.t) t
  (** [dk] is a function for getting the current DataKit
      connection. *)

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
  type t

  val everyone: t
  val username: string -> t

  val github_org: string -> t
  (** [github_org user] matches users who are members of the [user]
      GitHub organisation.  Note: currently, only public organisation
      membership is used. *)

  val can_read_github: string -> t
  (** [can_read "org/repo"] matches users who can see the [org/repo]
      GitHub repository. *)

  val any: t list -> t
end

module Web: sig
  type config

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
    can_read:ACL.t ->
    can_build:ACL.t ->
    unit -> config
end

module Config: sig
  type t
  type project
  type test = string Term.t

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

  val ci:
    web_config:Web.config ->
    projects:project list ->
    t
end

module Main: sig
  val run: ?info:Cmdliner.Term.info -> Config.t Cmdliner.Term.t -> unit
  (** [run ?info config] runs DataKitCI. [info] defaults to a term
      that describes the binary as [DataKitCI], but does not include
      any of the other metadata such as versioning. Call
      {!Cmdliner.Term.info} directly to obtain an [info] value that
      exposes all this extra data on the resulting command line. *)

  val logs: Live_log.manager
  (** The singleton log manager. *)
end

module Utils: sig
  val chdir_lock: Lwt_mutex.t

  val ok: 'a -> ('a, 'b) result Lwt.t

  val ( >>*= ): ('a, [< `Msg of string ]) result Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t

  val ( >|*= ): ('a, [< `Msg of string ]) result Lwt.t -> ('a -> 'b) -> 'b Lwt.t

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

module Process: sig

  (** Convenience wrappers around [Lwt_process]. *)

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
  type t

  val create: string -> int -> t

  val use: t -> ?log:Live_log.t -> ?label:string -> job_id ->
    (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [use t job fn] evaluates [fn ()] with one pool resource held.
      [job] (and [label]) will be displayed as the reason why the
      resource is in use. If [log] is provided then a message will be
      logged if we have to wait, and if the log is cancellable then
      the user will be able to cancel the operation. *)
end

module type BUILDER = sig
  type t
  (** A builder generates values from inputs (keys). A builder is
      typically used with a cache. *)

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

  (** A cache for values computed (slowly) by terms. *)

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
      B.value lwt_status Lwt.t
    (** [lookup t conn ~rebuild ctx key] returns the cached value of
        [key], or uses [B.generate ctx key] to start the process of
        calculating the value if this is the first time [key] has been
        requested.  If [rebuild] is [true] then any complete cached
        result is ignored (we mark the result branch as needing a
        rebuild and build again anyway). *)

    val term: t -> B.context -> B.Key.t -> B.value Term.t
    (** [term t key] evaluates to the result of looking up [key] in
        the cache (using [lookup]). *)
  end
end

(** / **)

module Private: sig
  (* This is only exposed for the unit-tests. *)

  type engine

  module Client9p: sig
    include Protocol_9p_client.S
    val connect:
      string -> string ->
      ?msize:int32 -> ?username:string -> ?aname:string -> unit ->
      t Protocol_9p_error.t Lwt.t
  end

  val connect: Client9p.t -> DK.t

  val test_engine: web_ui:Uri.t -> (unit -> DK.t Lwt.t) ->
    (Target.t -> string Term.t String.Map.t) Repo.Map.t ->
    engine

  val listen: ?switch:Lwt_switch.t -> engine -> [`Abort] Lwt.t

  val create_logs: unit -> Live_log.manager
  val lookup_log: branch:string -> Live_log.manager -> Live_log.t option
  val cancel: Live_log.t -> (unit, string) result Lwt.t
  val read_log: DK.t -> Step_log.saved -> string DK.or_error Lwt.t

end
