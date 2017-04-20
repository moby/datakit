(** Expose Irmin internal trees (to be upstreamed).

    Irmin's public API exposes a key/value store, but we need to see
    directory nodes too. This module uses Irmin's private API to get
    that information.
*)

module Path: Irmin.Path.S with type step = string

type step = Path.step
type path = Path.t
type perm = [ `Normal | `Exec | `Link ]

module type S = Irmin.S
  with type key = path
   and type contents = Ivfs_blob.t
   and type branch = string
   and type step = step
   and type metadata = perm

(* to avoid a direct dependency with Irmin_git *)
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

module Make (M: MAKER): S
