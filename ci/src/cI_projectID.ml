open Astring
open Asetmap
open Sexplib.Std

module ID = struct
  type t = {
    user : string;
    project : string;
  } [@@deriving sexp]

  let compare a b =
    match String.compare a.user b.user with
    | 0 -> String.compare a.project b.project
    | r -> r
end

include ID

let v ~user ~project = { user; project }

let pp f { user; project } =
  Fmt.pf f "%s/%s" user project

let path { user; project } =
  let open! Datakit_path.Infix in
  Datakit_path.empty / user / project

let of_string_exn s =
  match String.cuts ~sep:"/" s with
  | [user; project] -> { user; project }
  | _ -> CI_utils.failf "Project specifier %S not in the form USER/PROJECT" s

module Map = Map.Make(ID)
module Set = Set.Make(ID)
