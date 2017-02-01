type t
(** A base string with an index into it, representing the rest of the string from that point. *)

val of_string : string -> t
(** [of_string s] is a cursor at the start of [s]. *)

val to_string : t -> string
(** [to_string t] is the substring from [t] to the end of the input. *)

val skip : t -> t
(** [skip t] is the stream without its first character. [t] must be non-empty. *)

val skip_all : t -> t
(** [skip_all t] is the empty stream at the end of [t]. *)

val find : t -> char -> t option
(** [find t c] is a stream from the first occurance of [c] in [t], if any. *)

val avail : t -> int
(** [avail t] is the number of remaining characters in the stream. *)

val is_empty : t -> bool
(** [is_empty t] is [avail t = 0]. *)

val next : t -> (char * t) option
(** [next t] is [Some (c, t2)], where [c] is the next character in the stream and [t2] is [skip t],
    or [None] if [is_empty t]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff the streams [a] and [b] are at the same offset in the same base string. *)

type span = string * int * int
(** [(s, a, b)] represents the span of [s] from index [a] up to but excluding [b]. *)

val (--) : t -> t -> span
(** [a -- b] is the span from [a] (inclusive) to [b] (exclusive).
    [a] must not have a higher offset than [b]. *)

val string_of_span : span -> string
(** [string_of_span (s, a, b)] is the sub-string of [s] from [a] to [b]. *)
