(** An immutable Cstruct-like type that allows more efficient modification. *)

open Result

type t
(** A [t] is an immutable sequence of bytes. *)

val empty: t
(** The empty blob. *)

val write: t -> offset:int64 -> Cstruct.t -> (t, Vfs.Error.t) result
(** [write t ~offset data] is the blob [t] overwritten with [data] at [offset].
    The new blob is extended and zero-filled as necessary.
    If [offset = len t] then this is very fast. *)

val len : t -> int64

val truncate: t -> int64 -> (t, Vfs.Error.t) result
(** [truncate t l] is a blob of length [l]. If [l <= len t] then it is the prefix
    of [t] of length [l]. If [l > len t] then it is [t] followed by enough zero
    bytes to make up the length.
    Returns an error if the requested length is negative. *)

val read: t -> offset:int64 -> count:int -> (Cstruct.t, Vfs.Error.t) result
(** [read t ~offset ~count] is the [count] bytes in [t] starting at [offset].
    If the blob is not long enough to return [count] bytes, then it returns
    as many as possible.
    Returns an error if [offset < 0 || offset > len t]. *)

val of_ro_cstruct: Cstruct.t -> t
(** [of_ro_cstruct c] is a blob containing the data [c].
    Note that we take a reference to [c] rather than copying, so [c] MUST NOT be modified
    after this call. [c] may also be shared with modified versions of the resulting blob. *)

val to_ro_cstruct: t -> Cstruct.t
(** [to_ro_cstruct t] is a read-only Cstruct with the same data as [t].
    DO NOT modify this - it may corrupt the blob or other blobs sharing data
    with this one if you do. *)

val of_string: string -> t
(** [of_string s] is a blob containing the same data as [s]. *)

val to_string: t -> string
(** [to_string t] is a string containing the same data as [t]. *)
