val run : (string * (string * string CI_term.t) list) list Cmdliner.Term.t -> unit
(** [run projects] runs DataKitCI, monitoring [projects].
    Each item in the list is a GitHub project and the test to apply to PRs within it. *)

val logs : CI_live_log.manager
(** The singleton log manager. *)
