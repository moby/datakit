(** Conduit helpers. *)

(** The type for supported conduit endpoints. *)
type t = [
  | `NamedPipe of string
  | `Fd of int
  | `File of string
  | `Tcp of string * int
  | `HyperV_connect of Uri.t
  | `HyperV_accept of Uri.t
]

val pp: t Fmt.t
(** [pp] is the pretty-printer for conduit endpoits. *)

val parse: default_tcp_port:int -> string -> [`Ok of t | `Error of string]
(** [parse] parses conduit endpoint descriptions. *)

(** [accept_forever ~make_root url] starts a server which accepts Unix
   \ domain socket, TCP socket, HyperV socket and Named pipes
    connections and serves 9p filesystem described by [make_root
    ()]. *)
val accept_forever:
  ?backlog:int -> serviceid:string -> make_root:(unit -> Vfs.Dir.t) ->
  t -> unit Lwt.t
