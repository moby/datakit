(** Convenience wrappers around [Lwt_process]. *)

val run_with_exit_status :
  ?switch:Lwt_switch.t ->
  ?log:CI_live_log.t ->
  ?cwd:string ->
  ?env:string array ->
  ?stdin:Lwt_process.redirection ->
  output:(string -> unit) ->
  ?stderr:(string -> unit) ->
  ?log_cmd:Lwt_process.command ->
  Lwt_process.command ->
  Unix.process_status Lwt.t
(** Run [cmd], passing each chunk of output it produces on stdout to [output] and each chunk on stderr to [stderr].
    If [stderr] is not given, [output] is used for both.
    Returns the exit status of the process when completed.
    If [log_cmd] is given, it is displayed in all log messages instead of [cmd].
    This is useful to hide secret tokens, etc. *)

val run :
  ?switch:Lwt_switch.t ->
  ?log:CI_live_log.t ->
  ?cwd:string ->
  ?env:string array ->
  ?stdin:Lwt_process.redirection ->
  output:(string -> unit) ->
  ?stderr:(string -> unit) ->
  ?log_cmd:Lwt_process.command ->
  Lwt_process.command ->
  unit Lwt.t
(** Run [cmd], passing each chunk of output it produces on stdout to [output] and each chunk on stderr to [stderr].
    If [stderr] is not given, [output] is used for both.
    Raises an exception if the process doesn't return an exit status of zero. *)

val check_status : Lwt_process.command -> Unix.process_status -> unit
(** [check_status cmd status] checks that [status] is a successful exit status.
    If not, it raises an exception giving [cmd] as the cause. *)
