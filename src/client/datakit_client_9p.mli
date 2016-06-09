(** A DataKit client that connects to the server over a 9p connection. *)

module Make(P9p : Protocol_9p_client.S) : sig
  include Datakit_S.CLIENT with type error = Protocol_9p_error.error

  val connect : P9p.t -> t
  (** [connect c] is a Datakit connection using the 9p connection [c]. *)
end
