open Datakit_github
open !Asetmap

type t = [ `PR of PR.id | `Ref of Ref.id ]
val pp : t Fmt.t
val compare : t -> t -> int
val arg : t Cmdliner.Arg.converter
val repo : t -> Repo.t
val id : t -> [`PR of int | `Ref of string list ]
module Set: Set.S with type elt = t
val map_of_list : t list -> Set.t Repo.Map.t

type v = [ `PR of PR.t | `Ref of Ref.t ]
val head: v -> Commit.t
val compare_v: v -> v -> int
