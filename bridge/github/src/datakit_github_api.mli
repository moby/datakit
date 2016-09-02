(** [Vgithub.API] implementation using [ocaml-github] bindings. *)

open Datakit_github

include API with type token = Github.Token.t
