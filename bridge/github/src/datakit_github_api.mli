(** [Vgithub.API] implementation using [ocaml-github] bindings. *)

open Datakit_github

include API with type token = Github.Token.t

val event: Github_t.event -> Event.t
(** [event e] is a datakit view of GitHub event. *)
