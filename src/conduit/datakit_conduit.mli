(** Conduit helpers. *)

(** [accept_forever ~make_root url] starts a server which accepts Unix
    domain socket, TCP socket, HyperV socket and Named pipes
    connections and serves 9p filesystem described by [make_root
    ()]. *)
val accept_forever:
  sandbox:bool -> serviceid:string -> make_root:(unit -> Vfs.Dir.t) ->
  string -> unit Lwt.t
