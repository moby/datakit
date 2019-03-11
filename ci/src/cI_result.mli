type error =
  [ `Failure of string          (* A permanent error (unless an input changes) *)
  | `Pending of string ]        (* A problem that is expected to resolve itself with time *)

type 'a t = ('a, error) result

val pp_error: error Fmt.t
val pp: 'a Fmt.t -> 'a t Fmt.t

val v : [< `Success | `Pending | `Failure] -> string -> string t
val status : _ t -> [> `Success | `Pending | `Failure]
val descr : string t -> string
val json_of : string t -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> string t
