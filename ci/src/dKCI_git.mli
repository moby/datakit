open DataKitCI

type t
(** A local non-bare git working directory (with a .git sub-directory). *)

module Commit : sig
  (** A particlar commit with a [t]. *)

  include Map.OrderedType

  val hash : t -> string

  val includes_lwt : t -> commit:string -> bool Lwt.t
  (** [includes_lwt t ~commit] returns [true] iff [x] is a commit in the history of [t]. *)

  val includes : t -> commit:string -> bool Term.t
  (** [includes t ~commit] is the term version of [includes_lwt] *)
end

val connect : logs:Live_log.manager -> dir:string -> t
(** [connect ~logs ~dir] is the local Git repository at [dir]. *)

val fetch_head : t -> Commit.t Term.t
(** [fetch_head] evaluates to a local branch with a copy of the context PR's head commit (downloading it first if needed). *)

val with_checkout : log:Live_log.t -> reason:string -> Commit.t -> (string -> 'a Lwt.t) -> 'a Lwt.t
(** [with_checkout ~log ~reason commit fn] is [fn path], where [path] is the path of a local checkout of [commit].
    [path] must not be used after [fn]'s thread terminates.
    The directory is locked while [fn] runs with [reason] displayed to show why it is busy. *)

val with_clone : log:Live_log.t -> Commit.t -> (string -> 'a Lwt.t) -> 'a Lwt.t
(** [with_clone] is similar to [with_checkout] but clones the repository to a temporary directory first.
    This means that the repository does not need to be locked while the callback function runs. *)

type command
(** A cache of executions of a shell command. *)

val command : logs:Live_log.manager -> timeout:float -> label:string -> clone:bool -> string array list -> command
(** [command ~logs ~timeout ~label ~clone cmds] is a caching executor of [cmds], which caches results in a branch whose name includes [label].
    The command will be terminated if it exceeds [timeout] seconds.
    If [clone] is set then [with_clone] is used rather than [with_checkout].
    Note: every command must have a unique label! *)

val run : command -> Commit.t -> unit Term.t
(** [run cmd commit] succeeds if running [cmd] in a checkout of [commit] returns an exit status of zero. *)

