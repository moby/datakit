open CI_utils

type 'a status = {
  result: ('a, [`Pending of string * unit Lwt.t | `Failure of string]) result;
  output: CI_output.t
}

type state = {
  status: Datakit_github.Status_state.t;
  logs  : CI_output.t;
  descr : string;
}

type job_id = CI_target.t * string
(** Used in logging and monitoring to identify the owning job. *)

module type CONTEXT = sig
  type t
  (** A [ctx] is a context in which a term is evaluated. *)

  val watch : t -> unit Lwt.t -> unit
  (** [watch t thread] is called to indicate that the term will need
      to be recalculated when [thread] finishes. *)
end

module type TERM = sig
  type context
  type 'a key
  type 'a t
  val return: 'a -> 'a t
  val fail: ('a, Format.formatter, unit, 'b t) format4 -> 'a
  val pending: ('a, Format.formatter, unit, 'b t) format4 -> 'a
  val state: 'a t -> ('a, [`Pending of string | `Failure of string]) result t
  val of_state: ('a, [< `Pending of string | `Failure of string]) result -> 'a t
  val catch: 'a t -> ('a, [`Failure of string]) result t
  val value: 'a key -> 'a t
  val of_lwt_quick: 'a Lwt.t -> 'a t
  val of_lwt_slow: (unit -> 'a status Lwt.t) -> 'a t
  val join: 'a t t -> 'a t
  val pair: 'a t -> 'b t -> ('a * 'b) t
  val without_logs: 'a t -> 'a t
  module Infix: sig
    val ( $ ): ('a -> 'b) t -> 'a t -> 'b t
    val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
    val ( >|= ): 'a t -> ('a -> 'b) -> 'b t
  end
  val list_map_p: ('a -> 'b t) -> 'a list -> 'b list t
  val wait_for: 'a t -> while_pending:string -> on_failure:string -> unit t
  val wait_for_all: (string * 'a t) list -> unit t
end

module type BUILDER = sig
  type t
  module Key: sig type t end
  type context
  type value
  val name: t -> string
  val title: t -> Key.t -> string
  val generate:
    t -> switch:Lwt_switch.t -> log:CI_live_log.t -> DK.Transaction.t ->
    context -> Key.t -> (value, [`Failure of string]) result Lwt.t
  val load: t -> DK.Tree.t -> Key.t -> value Lwt.t
  val branch: t -> Key.t -> string
end
