open Asetmap

type t = private {
  user : string;
  project : string;
}
(** A project on GitHub *)

val v : user:string -> project:string -> t

val pp : t Fmt.t

val path : t -> Datakit_path.t

val of_string_exn : string -> t
(** [of_string_exn s] parses a string of the form "user/project". *)

module Map : Map.S with type key = t
