(** An immutable Cstruct-like type that allows more efficient
    modification. *)

open Result
type t
val empty: t
val write: t -> offset:int64 -> Cstruct.t -> (t, Vfs.Error.t) result
val len: t -> int64
val truncate: t -> int64 -> (t, Vfs.Error.t) result
val read: t -> offset:int64 -> count:int -> (Cstruct.t, Vfs.Error.t) result
val ro_cstruct: Cstruct.t -> t
val to_ro_cstruct: t -> Cstruct.t
val string: string -> t
val to_string: t -> string
val compare: t -> t -> int

include Irmin.Contents.S with type t := t
