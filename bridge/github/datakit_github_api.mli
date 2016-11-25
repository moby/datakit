(** {!API} implementation using [ocaml-github] bindings. *)


type token

val token: user_agent:string -> token:Github.Token.t -> token

include Datakit_github.API with type token := token
