module Make (Store : Ivfs_tree.S): sig

  val create: ?init:(string * string) list -> Store.Repo.t -> Vfs.Dir.t
  (** Create the /remotes/ virtual directory. [init] is a pair of
      remote names and urls. *)

end
