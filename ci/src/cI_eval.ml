open Lwt.Infix

type 'a or_error = 'a CI_result.t

module L = CI_output

module Make (C: CI_s.CONTEXT) = struct
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
    check () >|= fun { CI_s.result; output } ->
    match result with
    | Ok _ | Error (`Failure _) as x -> x, output
    | Error (`Pending (message, ready)) ->
      C.watch ctx ready;
      Error (`Pending message), output

  let pair a b ctx =
    a ctx >>= fun (a, a_logs) ->
    b ctx >>= fun (b, b_logs) ->
    let logs = L.Pair (a_logs, b_logs) in
    match a, b with
    | Ok a, Ok b -> Lwt.return (Ok (a, b), logs)
    | Error _ as problem, _ -> Lwt.return (problem, logs)
    | Ok _, (Error _ as problem) -> Lwt.return (problem, logs)

  let without_logs x ctx =
    x ctx >|= fun (x, _) -> x, L.Empty

  let wait_for (x: 'a t) ~while_pending ~on_failure ctx =
    x ctx >|= fun (x, x_logs) ->
    match x with
    | Ok _ -> Ok (), x_logs
    | Error (`Pending _) -> Error (`Pending while_pending), x_logs
    | Error (`Failure _) -> Error (`Failure on_failure), x_logs

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

  let pp_names = Fmt.(list ~sep:(const string ", ") string)

  let wait_for_all l =
    let partition (ps, fs) (name, state) =
      match state with
      | Ok _ -> (ps, fs)
      | Error (`Pending _) -> (name :: ps), fs
      | Error (`Failure _) -> ps, (name :: fs)
    in
    let get_state (name, x) = state x >|= fun s -> (name, s) in
    list_map_p get_state l >>= fun states ->
    match List.fold_left partition ([], []) states with
    | [], [] -> return ()
    | [], fs -> fail "%a failed" pp_names fs
    | ps, [] -> pending "Waiting for %a" pp_names ps
    | ps, fs -> pending "%a failed (still waiting for %a)" pp_names fs pp_names ps
end
