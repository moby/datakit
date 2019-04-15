val run : ?info:Cmdliner.Term.info -> CI_config.t Cmdliner.Term.t -> 'a

val logs : CI_live_log.manager
