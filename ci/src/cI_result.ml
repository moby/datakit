type error =
  [ `Failure of string          (* A permanent error (unless an input changes) *)
  | `Pending of string ]      (* A problem that is expected to resolve itself with time *)

type 'a t = ('a, error) result

let pp_error f = function
  | `Failure x -> Fmt.pf f "Failure: %s" x
  | `Pending x -> Fmt.pf f "Pending: %s" x

let pp ok f = function
  | Ok x -> ok f x
  | Error e -> pp_error f e

let descr = function
  | Ok x -> x
  | Error (`Failure x | `Pending x) -> x

let v status descr =
  match status with
  | `Success -> Ok descr
  | `Pending -> Error (`Pending descr)
  | `Failure -> Error (`Failure descr)

let status = function
  | Ok _ -> `Success
  | Error (`Pending _) -> `Pending
  | Error (`Failure _) -> `Failure

let string_of_status = function
  | `Pending -> "pending"
  | `Success -> "success"
  | `Failure -> "failure"

let json_of t =
  `Assoc [
    "status", `String (status t |> string_of_status);
    "descr", `String (descr t);
  ]

let of_json = function
  | `Assoc ["status", `String "success"; "descr", `String d] -> Ok d
  | `Assoc ["status", `String "pending"; "descr", `String d] -> Error (`Pending d)
  | `Assoc ["status", `String "failure"; "descr", `String d] -> Error (`Failure d)
  | json -> CI_utils.failf "Invalid results JSON: %a" (Yojson.Basic.pretty_print ?std:None) json
