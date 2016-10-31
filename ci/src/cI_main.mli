val run : CI_config.t Cmdliner.Term.t -> 'a
(** [run config] runs DataKitCI. *)

val logs : CI_live_log.manager
(** The singleton log manager. *)
