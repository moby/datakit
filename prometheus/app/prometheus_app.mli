(** Report metrics for Prometheus.
    See: https://prometheus.io/

    Notes:

    - This module is intended to be used by applications that export Prometheus metrics.
      Libraries should only link against the `Prometheus` module.

    - This module automatically initialises itself and registers some standard collectors relating to
      GC statistics, as recommended by Prometheus.
 *)

type config

module TextFormat_0_0_4 : sig
  val output : Prometheus.CollectorRegistry.snapshot Fmt.t
  (** Format a snapshot in Prometheus's text format, version 0.0.4. *)
end

val serve : config -> unit Lwt.t list
(** [serve config] starts a Cohttp server according to config.
    It returns a singleton list containing the thread to monitor,
    or an empty list if no server is configured. *)

val opts : config Cmdliner.Term.t
(** [opts] is the extra command-line options to offer Prometheus
    monitoring. *)
