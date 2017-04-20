(* FIXME: upstream that module *)

open Astring

type perm = [ `Normal | `Exec | `Link ]

module Path: Irmin.Path.S with type step = string = struct

  type step = string
  let step_t = Irmin.Type.string
  let pp_step ppf x = Fmt.string ppf x
  let step_of_string = function
    | "" -> Error (`Msg "Empty step!")
    | s  -> Ok s


  type t = string list
  let t = Irmin.Type.(list string)

  let empty = []
  let is_empty l = (l = [])
  let cons s t = t @ [s]
  let rcons t s = s :: t

  let rdecons = function
    | []   -> None
    | h::t -> Some (t, h)

  let decons l =
    match List.rev l with
    | []   -> None
    | h::t -> Some (h, List.rev t)

  let map l f = List.map f l
  let v x = List.rev x
  let pp ppf t = Fmt.(list ~sep:(unit "/") string) ppf (List.rev t)

  (* XXX: slow *)
  let of_string s =
    List.filter ((<>)"") (String.cuts s ~sep:"/")
    |> List.rev
    |> fun x -> Ok x

end

type step = Path.step
type path = Path.t

module type S = Irmin.S
  with type key = path
   and type contents = Ivfs_blob.t
   and type branch = string
   and type step = step
   and type metadata = perm

module Blobs = struct
  include Ivfs_blob
  let merge = Irmin.Merge.(option @@ default t)
  let of_string x = Ok (of_string x)
end

module type MAKER =
  functor (C: Irmin.Contents.S) ->
  functor (P: Irmin.Path.S) ->
  functor (B: Irmin.Branch.S) ->
    Irmin.S with type key = P.t
             and type step = P.step
             and module Key = P
             and type contents = C.t
             and type branch = B.t
             and type metadata = perm

module Make (M: MAKER) = M(Blobs)(Path)(Irmin.Branch.String)
