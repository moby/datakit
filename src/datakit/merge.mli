type path = Path.t

type step = Path.step

type blob = Blob.t

type perm = Metadata.t

module type RW = sig
  type t

  val update_force : t -> path -> step -> blob * perm -> unit Lwt.t

  val remove_force : t -> path -> step -> unit Lwt.t
end

module Make (Store : Store.S) (RW : RW) : sig
  val merge :
    ours:Store.t ->
    theirs:Store.t ->
    base:Store.t option ->
    RW.t ->
    Path.Set.t Lwt.t
  (** [merge ~ours ~theirs ~base result] updates [result] (which
        initially is a copy of [ours]) to our best attempt at a merge.
        Returns the set of paths with conflicts. *)
end
