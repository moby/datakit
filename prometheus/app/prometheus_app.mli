type config

val serve : config -> unit Lwt.t list
(** [serve config] starts a Cohttp server according to config.
    It returns a singleton list containing the thread to monitor,
    or an empty list if no server is configured. *)

val opts : config Cmdliner.Term.t
(** [opts] is the extra command-line options to offer Prometheus
    monitoring. *)
