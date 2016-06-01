module PathSet : Set.S with type elt = Irmin.Path.String_list.t

module type RW = sig
  type t
  val update_force : t -> Ivfs_tree.path -> string -> Ivfs_blob.t * Ivfs_tree.perm -> unit Lwt.t
  val remove_force : t -> Ivfs_tree.path -> string -> unit Lwt.t
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
