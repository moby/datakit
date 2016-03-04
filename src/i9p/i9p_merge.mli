module PathSet : Set.S with type elt = Irmin.Path.String_list.t

module Make (Store : I9p_tree.STORE) (View : Irmin.VIEW with type db = Store.t and type key = string list and type value = string) : sig
  val merge :
    ours:Store.t ->
    theirs:Store.t ->
    base:Store.t option ->
    View.t -> PathSet.t Lwt.t
  (** [merge ~ours ~theirs ~base result] updates [result] (which initially is a
      copy of [ours]) to our best attempt at a merge.
      Returns the set of paths with conflicts. *)
end
