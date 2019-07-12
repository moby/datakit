open Astring

type step = string

let step_t = Irmin.Type.string

let pp_step ppf x = Fmt.string ppf x

let step_of_string = function "" -> Error (`Msg "Empty step!") | s -> Ok s

type t = string list

let t = Irmin.Type.(list string)

let empty = []

let is_empty l = l = []

let cons s t = t @ [ s ]

let rcons t s = s :: t

let rdecons = function [] -> None | h :: t -> Some (t, h)

let decons l =
  match List.rev l with [] -> None | h :: t -> Some (h, List.rev t)

let map l f = List.map f l

let v x = List.rev x

let pp ppf t = Fmt.(list ~sep:(unit "/") string) ppf (List.rev t)

(* XXX: slow *)
let of_string s =
  List.filter (( <> ) "") (String.cuts s ~sep:"/") |> List.rev |> fun x ->
  Ok x

module X = struct
  type nonrec t = t

  let compare = Irmin.Type.compare t
end

module Set = Set.Make (X)
