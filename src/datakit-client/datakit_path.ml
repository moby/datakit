open Result

type t = string list

let empty = []

let validate = function
  | "" | "." | ".." as x -> Error (Fmt.strf "Invalid path component %S" x)
  | x when String.contains x '/' -> Error (Fmt.strf "'/' in path step %S" x)
  | _ -> Ok ()

let of_steps steps =
  let rec aux = function
    | [] -> Ok steps
    | x :: xs ->
      match validate x with
      | Ok () -> aux xs
      | Error _ as e -> e in
  aux steps

let of_string path =
  of_steps (Astring.String.cuts ~sep:"/" path)

let of_string_exn path =
  match of_string path with
  | Ok x -> x
  | Error msg -> raise (Invalid_argument msg)

let pp = Fmt.(list ~sep:(const string "/") string)

let of_steps_exn steps =
  match of_steps steps with
  | Ok x -> x
  | Error msg ->
    raise (Invalid_argument (Fmt.strf "Bad path %a: %s" pp steps msg))

let unwrap x = x

let to_hum = Fmt.to_to_string pp

let compare = compare

module Set = Set.Make(struct type t = string list let compare = compare end)
module Map = Map.Make(struct type t = string list let compare = compare end)

module Infix = struct

  let ( / ) path s =
    match validate s with
    | Ok () -> path @ [s]
    | Error msg -> raise (Invalid_argument msg)

  let ( /@ ) = ( @ )

end
