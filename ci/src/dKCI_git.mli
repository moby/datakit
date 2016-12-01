(** Manipulating Git repositories with DataKit CI.

    [DKCI_git] extends [DKCI] with Git-related terms. It defines types
    for {{!t}local Git repositories}, {{!commit}Git commits} inside
    these repositories and {{!command}shell command} to run on a given
    checkout.
*)

open DKCI

type t
(** The type for local non-bare git working directory (with a .git
    sub-directory). *)

val v: logs:Live_log.manager -> dir:string -> t
(** [v ~logs ~dir] is the local Git repository at [dir]. *)

type commit
(** The type for Git commits. *)

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
