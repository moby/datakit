val run : ?info:Cmdliner.Term.info -> CI_config.t Cmdliner.Term.t -> 'a
(** [run ?info config] runs DataKitCI.  [info] defaults to a term
    that describes the binary as [DataKitCI], but does not include any
    of the other metadata such as versioning. Call {!Cmdliner.Term.info}
    directly to obtain an [info] value that exposes all this extra data
    on the resulting command line. *)

val logs : CI_live_log.manager
(** The singleton log manager. *)
