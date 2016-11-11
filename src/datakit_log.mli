(** Datakit loggers. *)

(** Type for datakit log destination: quiet (simple outputs on
    stdout), timestamp (timestamped outputs on stdout), ASL, or Window
    event logs. *)
type t =
  | Quiet
  | Timestamp
  | Eventlog
  | ASL

val log_destination: t Cmdliner.Term.t
(** [log_destination] is [--log-destination] command-line argument,
    which sets-up a native log destination. *)

val setup: Fmt.style_renderer option -> t -> Logs.level option ->
  [`Posix | `Monotonic] -> unit
(** [setup s t l] setups the log rendering options. [s] specifies
    colors settings, [t] is the native log destination and [l] is the log
    level. *)

val log_clock: [`Posix | `Monotonic] Cmdliner.Term.t
