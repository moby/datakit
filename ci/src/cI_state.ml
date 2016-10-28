open CI_utils

type t = {
  status : Datakit_S.status_state;
  logs : CI_result.Step_log.t;
  descr : string;
}

let pp_status f = function
  | `Pending -> Fmt.string f "pending"
  | `Success -> Fmt.string f "success"
  | `Error -> Fmt.string f "error"
  | `Failure -> Fmt.string f "failure"

let status_of_string = function
  | "pending" -> `Pending
  | "success" -> `Success
  | "failure" -> `Failure
  | "error" -> `Error
  | x ->
    Log.err (fun f -> f "Invalid state %S" x);
    `Error

