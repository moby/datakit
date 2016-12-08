type path = Ivfs_tree.path
type step = Ivfs_tree.step

module PathSet : Set.S with type elt = path


module type RW = sig
  type t
  val update_force : t -> path -> step -> Ivfs_blob.t * Ivfs_tree.perm -> unit Lwt.t
  val remove_force : t -> path -> step -> unit Lwt.t
end

module Make (Store : Ivfs_tree.STORE) (RW : RW) : sig
  val merge :
    ours:Store.t ->
    theirs:Store.t ->
    base:Store.t option ->
    RW.t -> PathSet.t Lwt.t
    (** [merge ~ours ~theirs ~base result] updates [result] (which initially is a
        copy of [ours]) to our best attempt at a merge.
        Returns the set of paths with conflicts. *)
end
