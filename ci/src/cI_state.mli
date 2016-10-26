type t = {
  status : Datakit_S.status_state;
  logs : CI_result.Step_log.t;
  descr : string;
}

val pp_status : Datakit_S.status_state Fmt.t

val status_of_string : string -> Datakit_S.status_state
