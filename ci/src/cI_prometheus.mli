module CollectorRegistry : sig
  type t
  type snapshot
  val create : unit -> t
  val default : t
  val collect : t -> snapshot
end

module TextFormat_0_0_4 : sig
  val output : CollectorRegistry.snapshot Fmt.t
end

module type METRIC = sig
  type family
  type t
  val v_labels : label_names:string array -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  val labels : family -> string array -> t

  val v_label : label:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  (** [v_label] is a convenience wrapper around [v_labels] for the case where there is a single label.
      The result is a function from the single label's value to the metric. *)

  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
  (** [v] is a convenience wrapper around [v_labels] for the case where there are no labels. *)
end

module Counter : sig
  include METRIC
  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases [t] by [v], which must be non-negative. *)
end

module Gauge : sig
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
  include METRIC

  val observe : t -> float -> unit
  val time : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end
