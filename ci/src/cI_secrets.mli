type t
(** A collection of secrets. *)

type github_auth = {
  client_id : string;
  client_secret : string;
  callback : Uri.t option;
}

type 'a secret
(* A secret that can be read or written. *)

val get : 'a secret -> 'a option
(* Read the current value of the secret. This operation is quick (does not access the disk). *)

val set : 'a secret -> 'a option -> unit Lwt.t

val private_key_path : t -> string
(** [private_key_path t] is the path of the PEM-encoded private key. *)

val certificate_path : t -> string
(** [certificate_path t] is the path of the PEM-encoded X.509 certificate for the key. *)

val passwords_path : t -> string
(** [passwords_path t] is the path of the file in which passwords, roles, etc should be saved. *)

val github_auth : t -> github_auth secret Lwt.t

val create : key_bits:int -> string -> t Lwt.t
(** [create ~key_bits secrets_dir] connects to [secrets_dir], creating a new [key_bits] long RSA pair-key and
    self-signed certificate if there isn't one there already. *)

val const : 'a option -> 'a secret
(** [const v] is a secret whose value is always [v] and which cannot be set. Mainly useful for unit-tests. *)
