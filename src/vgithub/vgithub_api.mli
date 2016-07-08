(** [Vgithub.API] implementation using [ocaml-github] bindings. *)

include Vgithub.API with type token = Github.Token.t
