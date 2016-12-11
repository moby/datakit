open CI_s

type t
val v: logs:CI_live_log.manager -> dir:string -> t
type commit
val fetch_head: t -> CI_target.t -> commit CI_term.t
val with_checkout:
  log:CI_live_log.t -> job_id:job_id -> commit -> (string -> 'a Lwt.t) -> 'a Lwt.t
val with_clone:
  log:CI_live_log.t -> job_id:job_id -> commit -> (string -> 'a Lwt.t) -> 'a Lwt.t
type command
val command:
  logs:CI_live_log.manager -> timeout:float -> label:string -> clone:bool ->
  string array list -> command
val run: command -> commit -> unit CI_term.t
