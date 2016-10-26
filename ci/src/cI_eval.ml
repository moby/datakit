open Lwt.Infix

type 'a or_error = 'a CI_result.t

module L = CI_result.Step_log

module Make(C:CI_s.CONTEXT) = struct
  type context = C.t
  type 'a key = C.t -> 'a

  type 'a t = C.t -> ('a or_error * L.t) Lwt.t

  let return x _ = Lwt.return (Ok x, L.Empty)

  let fail fmt =
    fmt |> Fmt.kstrf @@ (fun x _ -> Lwt.return (Error (`Failure x), L.Empty))

  let pending fmt =
    fmt |> Fmt.kstrf @@ (fun x _ -> Lwt.return (Error (`Pending x), L.Empty))

  let state x ctx =
    x ctx >|= fun (x, x_logs) -> Ok x, x_logs

  let of_state x _ = Lwt.return ((x :> 'a or_error), L.Empty)

  let catch x ctx =
    x ctx >>= fun (x, x_logs) ->
    match x with
    | Error (`Pending _) as x -> Lwt.return (x, x_logs)
    | Ok _ | Error (`Failure _) as x -> Lwt.return (Ok x, x_logs)

  let run ctx t =
    t ctx

  let value key ctx =
    Lwt.return (Ok (key ctx), L.Empty)

  let of_lwt_quick x _ctx =
    x >>= fun x -> Lwt.return (Ok x, L.Empty)

  let of_lwt_slow check ctx =
    check () >|= fun (x, logs) ->
    match x with
    | Ok _ | Error (`Failure _) as x -> x, logs
    | Error (`Pending (message, ready)) ->
      C.watch ctx ready;
      Error (`Pending message), logs

  let pair a b ctx =
    a ctx >>= fun (a, a_logs) ->
    b ctx >>= fun (b, b_logs) ->
    let logs = L.Pair (a_logs, b_logs) in
    match a, b with
    | Ok a, Ok b -> Lwt.return (Ok (a, b), logs)
    | Error _ as problem, _ -> Lwt.return (problem, logs)
    | Ok _, (Error _ as problem) -> Lwt.return (problem, logs)

  module Infix = struct
    let ( >>= ) x f ctx =
      x ctx >>= fun (x, x_logs) ->
      match x with
      | Error _ as problem -> Lwt.return (problem, x_logs)
      | Ok x ->
        f x ctx >|= fun (f_result, f_logs) ->
        (f_result, L.Pair (x_logs, f_logs))

    let ( >|= ) x f =
      x >>= fun x ->
      return (f x)

    let ( $ ) f x =
      pair f x >|= fun (f, x) -> f x

  end

  open! Infix

  let join t =
    t >>= fun x -> x

  let list_map_p f l =
    List.fold_left (fun acc x ->
        pair acc (f x) >|= fun (acc, x) -> x :: acc
      ) (return []) l

end
