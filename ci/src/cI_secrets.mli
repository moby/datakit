type t
(** A directory containing a private key and certificate. *)

val private_key_path : t -> string
(** [private_key_path t] is the path of the PEM-encoded private key. *)

val certificate_path : t -> string
(** [certificate_path t] is the path of the PEM-encoded X.509 certificate for the key. *)

val passwords_path : t -> string
(** [passwords_path t] is the path of the file in which passwords, roles, etc should be saved. *)

val create : key_bits:int -> string -> t Lwt.t
(** [create ~key_bits secrets_dir] connects to [secrets_dir], creating a new [key_bits] long RSA pair-key and
    self-signed certificate if there isn't one there already. *)
