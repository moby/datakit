(** A DataKit client that use a Git directory directly. *)

include Datakit_client.S

val connect:
  ?head:string -> ?bare:bool -> ?level:int -> ?dot_git:string ->
  ?author:string -> string -> t Lwt.t
