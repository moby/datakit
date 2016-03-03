(** Filesystem server. *)

open I9p_misc

(** The server signature. *)
module type S = sig

  type flow
  (** The type for communication "channels" between the clients and
      the server. *)

  type inode
  (** The type of filesystem inodes. *)

  module Inode: I9p_inode.S with type t = inode
  (** A server should implement the inode signature. *)

  val accept: root:Inode.dir -> flow -> unit or_error Lwt.t
  (** [accept root f] accepts connection on [f]. *)

end

(** Server builder. *)
module Make (Log: Protocol_9p.S.LOG) (Flow: V1_LWT.FLOW):
  S with type flow = Flow.flow
