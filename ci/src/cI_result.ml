module Step_log = struct
  type saved = {
    title : string;
    commit : string;
    branch : string;
    failed : bool;
    rebuild : unit Lwt.t Lazy.t;
  }

  type t =
    | Empty
    | Live of CI_live_log.t
    | Saved of saved
    | Pair of t * t
end

type error =
  [ `Failure of string          (* A permanent error (unless an input changes) *)
  | `Pending of string ]        (* A problem that is expected to resolve itself with time *)

type 'a t = ('a, error) result

let pp_error f = function
  | `Failure x -> Fmt.pf f "Failure: %s" x
  | `Pending x -> Fmt.pf f "Pending: %s" x

let pp ok f = function
  | Ok x -> ok f x
  | Error e -> pp_error f e
