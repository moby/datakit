module Make (Store : Ivfs_tree.STORE): sig

  val create: ?init:(string * string) list ->
    string Irmin.Task.f -> Store.Repo.t -> Vfs.Dir.t
  (** Create the /remotes/ virtual directory. [init] is a pair of
      remote names and urls. *)

end
