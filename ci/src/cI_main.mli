val run : web_config:CI_web_templates.t -> (string * (string * string CI_term.t) list) list Cmdliner.Term.t -> unit
(** [run ~web_config projects] runs DataKitCI, monitoring [projects].
    Each item in the list is a GitHub project and the test to apply to branches, tags and open PRs within it. *)

val logs : CI_live_log.manager
(** The singleton log manager. *)
