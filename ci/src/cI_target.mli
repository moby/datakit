open Datakit_github
open! Asetmap

type t = [ `PR of PR.id | `Ref of Ref.id ]

val pp : t Fmt.t

val compare : t -> t -> int

val equal : t -> t -> bool

val arg : t Cmdliner.Arg.converter

val repo : t -> Repo.t

val id : t -> [ `PR of int | `Ref of string list ]

val path : ?test:string -> t -> Uri.t

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

val map_of_list : t list -> Set.t Repo.Map.t

type v = [ `PR of PR.t | `Ref of Ref.t ]

val head : v -> Commit.t

val compare_v : v -> v -> int

val path_v : v -> Uri.t

val repo_v : v -> Repo.t

val unescape_ref : string -> Ref.name

val pp_v : v Fmt.t

val of_v : v -> t

module Branch_escape : sig
  val pp_sub : t Fmt.t
  (** [pp_sub t] formats the PR or ref part as a branch name, without the repository information. *)

  val parse_sub : repo:Repo.t -> string -> t option
  (** [parse_sub ~repo s] is the reverse of [pp_sub]. *)
end

val status_branch : t -> string
(** [status_branch target] is the DataKit branch in which to store the status of [target]'s jobs. *)

val of_status_branch : string -> t
(** [of_status_branch branch_name] reverses [status_branch_v]. *)
