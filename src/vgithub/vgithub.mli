val create: (unit -> Github.Token.t) -> Vfs.Inode.t

(** TODO: remove this and replace it by a proper Datakit client at the
    VFS level. *)
module Hack: sig
  type 'a state = (module Datakit_S.CLIENT with type t = 'a) * 'a
  type t = E: 'a state -> t
   val init: (unit -> (t, string) Result.result Lwt.t) -> unit
end
