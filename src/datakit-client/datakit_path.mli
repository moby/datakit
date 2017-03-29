(** Locate files and directories within a DataKit tree. *)

open Result

module Step : sig
  type t = private string
  (** One component in a path *)

  val of_string : string -> (t, string) result
  val of_string_exn : string -> t
  val to_string : t -> string

  val compare : t -> t -> int
  val pp : t Fmt.t
end

type t = Step.t list
(** A [path] identifies a file or directory (relative to some other directory).
    No component may be empty or contain a '/' character. "." and ".." steps
    are not permitted in a path. *)

val empty : t
(** The empty path. *)

val of_steps : string list -> (t, string) result
(** Converts a list of the form ["a"; "b"; "c"] to a path. *)

val of_steps_exn : string list -> t
(** Converts a list of the form ["a"; "b"; "c"] to a path. *)

val of_string : string -> (t, string) result
(** Converts a path of the form ["a/b/c"] to a path. *)

val of_string_exn : string -> t

val unwrap : t -> string list
(** Cast to a list of strings *)

val pop : t -> (t * string) option
(** [pop (dir / leaf)] is [Some (dir, leaf)].
    [pop empty] is [None]. *)

val pp : t Fmt.t
(** [pp] is a formatter for human-readable paths. *)

val compare: t -> t -> int
(** [compare] is the comparison function for paths. *)

val to_hum : t -> string
(** Convert to a string, in the same format as [pp]. *)

val basename: t -> string option
(** [basename t] is [t]'s basename. *)

val dirname: t -> t
(** [dirname t] is [t]'s dirname. *)

module Set: Set.S with type elt = t
(** Sets of paths. *)

module Map: Map.S with type key = t
(** Maps of paths. *)

module Infix: sig

  val ( / ) : t -> string -> t
  (** [a / b] is the path [a] with step [b] appended. Raises an
      exception if [b] is not a valid step, so this should only be
      used with string constants, not user input. *)

  val ( /@ ) : t -> t -> t
  (** [a /@ b] is the concatenation of paths [a] and [b]. *)

end
