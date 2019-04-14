open Astring

type t

val pools : unit -> t String.Map.t

val create : string -> int -> t

val use :
  t ->
  ?log:CI_live_log.t ->
  ?label:string ->
  CI_s.job_id ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t
(** [use t job fn] evaluates [fn ()] with one pool resource held.
    [job] (and [label]) will be displayed as the reason why the resource is in use.
    If [log] is provided then a message will be logged if we have to wait, and
    if the log is cancellable then the user will be able to cancel the operation. *)

val active : t -> int

val capacity : t -> int

val qlen : t -> int

val users : t -> ((CI_s.job_id * string option) * CI_live_log.t option) list
(** [users t] is the list of reasons why resources are being used, one per resource, and (optionally) its
    log (through which it may be possible to cancel the job). *)
