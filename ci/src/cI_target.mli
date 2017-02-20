open Datakit_github
open !Asetmap

type t = [ `PR of PR.id | `Ref of Ref.id ]
val pp : t Fmt.t
val compare : t -> t -> int
val arg : t Cmdliner.Arg.converter
val repo : t -> Repo.t
val id : t -> [`PR of int | `Ref of string list ]
val path: t -> string

module Set: Set.S with type elt = t
val map_of_list : t list -> Set.t Repo.Map.t

type v = [ `PR of PR.t | `Ref of Ref.t ]
val head: v -> Commit.t
val compare_v: v -> v -> int
val path_v: v -> string
val repo_v : v -> Repo.t
val unescape_ref: string -> Ref.name
val pp_v : v Fmt.t
val status_branch_v : v -> string
(** [status_branch_v target] is the DataKit branch in which to store the status of [target]'s jobs. *)
