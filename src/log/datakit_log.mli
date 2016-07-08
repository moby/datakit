(** Datakit loggers. *)

(** Type for datakit log destination: standard error, ASL, or Window
    event logs. *)
type t =
  | Stderr
  | Eventlog
  | ASL

val log_destination: t Cmdliner.Term.t
(** [log_destination] is [--log-destination] command-line argument,
    which sets-up a native log destination. *)

val setup: Fmt.style_renderer option -> t -> Logs.level option -> unit
(** [setup s t l] setups the log rendering options. [s] specifies
    colors settings, [t] is the native log destination and [l] is the log
    level. *)
