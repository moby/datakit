open CI_utils

type 'a lwt_status = ('a, [`Pending of string * unit Lwt.t | `Failure of string]) result * CI_result.Step_log.t
(** ['a lwt_status] is similar to ['a or_error], except that the pending state also indicates when
    it should be checked again. *)

type job_id = CI_target.Full.t * string
(** Used in logging and monitoring to identify the owning job. *)

module type CONTEXT = sig
  type t
  (** A [ctx] is a context in which a term is evaluated. *)

  val watch : t -> unit Lwt.t -> unit
  (** [watch t thread] is called to indicate that the term will need to be recalculated
      when [thread] finishes. *)
end

module type TERM = sig
  type context
  (** A context in which a term can be evaluated. *)

  type 'a key
  (** A key within a context. *)

  type 'a t
  (** An ['a t] is a term that evaluates to an ['a], fails, or explains what it's waiting for. *)

  val return : 'a -> 'a t
  (** [return x] is a term that evaluates successfully to [x]. *)

  val fail : ('a, Format.formatter, unit, 'b t) format4 -> 'a
  (** [fail fmt] is a term that fails with message [fmt]. *)

  val pending : ('a, Format.formatter, unit, 'b t) format4 -> 'a
  (** [pending fmt] is a term that reports it is waiting because of reason [fmt]. *)

  val state : 'a t -> ('a, [`Pending of string | `Failure of string]) result t
  (** [state x] immediately and successfully returns the current state of [x]. *)

  val of_state : ('a, [< `Pending of string | `Failure of string]) result -> 'a t
  (** [of_state x] is a term which evaluates to [x]. *)

  val catch : 'a t -> ('a, [`Failure of string]) result t
  (** [catch x] successfully returns a result showing whether [x] succeeded or failed. *)

  val value : 'a key -> 'a t
  (** [value key] evaluates to the result of looking up [key] in the context. *)

  val of_lwt_quick : 'a Lwt.t -> 'a t
  (** [of_lwt_quick x] evaluates to the result of [x].
      Note that the result is never pending, so this is only for quick operations. *)

  val of_lwt_slow : (unit -> 'a lwt_status Lwt.t) -> 'a t
  (** [of_lwt_slow check] is a term that evaluates [check ()] to get the current status.
      If [Error (`Pending (message, ready))], another check is scheduled when [ready] terminates. *)

  val join : 'a t t -> 'a t
  (** [join tt] is the term to which [tt] evaluates. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair a b] evaluates to the pair of the results of successfully evaluating [a] and [b] (in parallel).
      If [a] is not successful, the result is the same as [a].
      Otherwise, if [b] is not successful then the result is the same as [b]. *)

  val without_logs : 'a t -> 'a t
  (** [without_logs t] evaluates to the same result as [t], but does not link to its logs.
      This is useful if [t]'s logs are already being reported as part of another job. *)

  module Infix : sig
    val ( $ ) : ('a -> 'b) t -> 'a t -> 'b t
    (** [fn $ x] is the result of applying the evaluation of the term [fn] to the evaluation of the term [x].
        [fn] and [x] are evaluated in parallel.
        While [fn] is not successful, the result is the same as [fn].
        Otherwise, if [x] is not successful then the result is the same as [x].
        You can use [$] multiple times to support functions of multiple arguments (e.g. [fn $ a $ b $ c]).
        Use with [join] if [fn] itself returns a term. *)

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    (** [x >>= fn] (bind) is [fn y] where [y] is the successful result of evaluating [x], or simply [x] if [x] is not successful. *)

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    (** [x >|= fn] (map) is [return (fn y)] where [y] is the successful result of evaluating [x], or simply [x] if [x] is not successful. *)
  end

  val list_map_p : ('a -> 'b t) -> 'a list -> 'b list t
  (** [list_map_p fn l] will map the [l] list of terms to [fn] in parallel and
      return the result list when all of them have completed. *)

  val wait_for : 'a t -> while_pending:string -> on_failure:string -> unit t
  (** [wait_for t ~while_pending ~on_failure] evaluates successfully to unit if
      [t] evaluates successfully. It is pending with reason [while_pending] while
      [t] is pending, and fails with error [on_failure] if [t] fails.
      This is useful in the case where one test case depends on another, to avoid
      reporting the same status twice. Consider wrapping with [without_logs] if [t]'s
      logs will be reported as part of another job. *)

  val wait_for_all : (string * 'a t) list -> unit t
  (** [wait_for_all xs] evaluates successfully to unit if all terms in the assoc
      list [xs] evaluate successfully. Otherwise, it is pending or failed with
      a suitable message, based on the names of the terms it is waiting for.
      Consider wrapping with [without_logs] if [xs]'s logs will be reported as
      part of another job. *)
end

module type BUILDER = sig
  type t
  (** A builder generates values from inputs (keys). A builder is typically used with a cache. *)

  module Key : Map.OrderedType
  (** Input describing what is to be built. *)

  type context
  (** For passing context parameters to the builder which aren't part of the key (e.g. timeouts
      or resource pools). *)

  type value
  (** Output of the builder. *)

  val name : t -> string
  (** A unique name for this builder.
      This is used for metric reporting. *)

  val title : t -> Key.t -> string
  (** [title t key] is a one-line summary of the operation that is performed by [generate].
      It is used as the reason string for the pending state and as the title of the log. *)

  val generate : t -> switch:Lwt_switch.t -> log:CI_live_log.t -> DK.Transaction.t -> context -> Key.t -> (value, [`Failure of string]) result Lwt.t
  (** [generate t ~log trans ctx key] generates the value for [key] and stores it in [trans] under
      the "value" directory. It is called when the value is not in the cache, or when a rebuild has been requested.
      The value returned must be the same as the value that would be loaded by [load].
      If the build is cancelled, [switch] will be turned off.
      If [generate] throws an exception then it is caught and treated as an error result. *)

  val load : t -> DK.Tree.t -> Key.t -> value Lwt.t
  (** [load t tr key] is the value [v] previously saved by [generate]. *)

  val branch : t -> Key.t -> string
  (** The name of the DataKit branch on which to store the result for this key.
      If the branch exists and is up-to-date then we use that. *)
end
