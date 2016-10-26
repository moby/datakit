open Astring

type t

val pools : unit -> t String.Map.t

val create : string -> int -> t

val use : t -> reason:string -> ?log:CI_live_log.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** [use t ~reason fn] evaluates [fn ()] with one pool resource held.
    [reason] will be displayed as the reason why the resource is in use.
    If [log] is provided then a message will be logged if we have to wait, and
    if the log is cancellable then the user will be able to cancel the operation. *)

val active : t -> int

val capacity : t -> int

val users : t -> (string * CI_live_log.t option) list
(** [users t] is the list of reasons why resources are being used, one per resource, and (optionally) its
    log (through which it may be possible to cancel the job). *)
