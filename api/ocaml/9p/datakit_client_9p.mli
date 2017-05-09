(** A DataKit client that connects to the server over a 9p connection. *)

module Make(P9p : Protocol_9p.Client.S) : sig
  include Datakit_client.S
  val connect : P9p.t -> t
  (** [connect c] is a Datakit connection using the 9p connection [c]. *)
end
