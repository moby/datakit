(** {!API} implementation using [ocaml-github] bindings. *)

include Datakit_github.API with type token = Github.Token.t
