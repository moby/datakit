(** Collect metrics for Prometheus.
    See: https://prometheus.io/

    Notes:

    - The Prometheus docs require that client libraries are thread-safe. We interpret this to mean safe
      with Lwt threads, NOT with native threading.

    - The [time] functions are unfortunate, as they create a dependency on [Unix].
      Maybe they should be moved elsewhere.

    - This module automatically initialises itself and registers some standard collectors relating to
      GC statistics, as recommended by Prometheus.

    - The API is rather limited. In particular, it is not yet possible to write your own collectors.
*)

module CollectorRegistry : sig
  type t
  (** A collection of metrics to be monitored. *)

  type snapshot
  (** The result of reading all the metrics. *)

  val create : unit -> t
  (** [create ()] is a fresh registry. This is mostly useful for testing. *)

  val default : t
  (** The default registry. *)

  val collect : t -> snapshot
  (** Read the current value of each metric. *)
end

module TextFormat_0_0_4 : sig
  val output : CollectorRegistry.snapshot Fmt.t
  (** Format a snapshot in Prometheus's text format, version 0.0.4. *)
end

module type METRIC = sig
  type family
  (** A collection of metrics that are the same except for their labels.
      e.g. "Number of HTTP responses" *)

  type t
  (** A particular metric.
      e.g. "Number of HTTP responses with code=404" *)

  val v_labels : label_names:string array -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  (** [v_labels ~label_names ~help ~namespace ~subsystem name] is a family of metrics with full name
      [namespace_subsystem_name] and documentation string [help]. Each metric in the family will provide
      a value for each of the labels.
      The new family is registered with [registry] (default: [CollectorRegistry.default]). *)

  val labels : family -> string array -> t
  (** [labels family label_values] is the metric in [family] with these values for the labels.
      The order of the values must be the same as the order of the [label_names] passed to [v_labels];
      you may wish to write a wrapper function with labelled arguments to avoid mistakes.
      If this is called multiple times with the same set of values, the existing metric will be returned. *)

  val v_label : label_name:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  (** [v_label] is a convenience wrapper around [v_labels] for the case where there is a single label.
      The result is a function from the single label's value to the metric. *)

  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
  (** [v] is a convenience wrapper around [v_labels] for the case where there are no labels. *)
end

module Counter : sig
  (** A counter is a cumulative metric that represents a single numerical value that only ever goes up. *)

  include METRIC
  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases [t] by [v], which must be non-negative. *)
end

module Gauge : sig
  (** A gauge is a metric that represents a single numerical value that can arbitrarily go up and down. *)

  include METRIC

  val inc_one : t -> unit
  val inc : t -> float -> unit

  val dec_one : t -> unit
  val dec : t -> float -> unit

  val set : t -> float -> unit

  val track_inprogress : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  val time : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module Summary : sig
  (** A summary is a metric that records both the number of readings and their total.
      This allows calculating the average. *)

  include METRIC

  val observe : t -> float -> unit
  val time : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end
