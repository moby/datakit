open Datakit_github

type t = {
  status : Status_state.t;
  logs : CI_result.Step_log.t;
  descr : string;
}
