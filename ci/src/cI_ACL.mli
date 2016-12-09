open Datakit_github

(** Access control lists. *)

type t =
  [ `Everyone
  | `Username of string
  | `Github_org of string
  | `Can_read of Repo.t
  | `Any of t list
  ]

val everyone : t
val username : string -> t
val github_org : string -> t
val can_read_github : string -> t

val any : t list -> t
