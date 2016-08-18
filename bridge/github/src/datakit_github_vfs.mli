module Make (API: Datakit_github.API): sig

  val create: API.token -> Vfs.Inode.t
  (** [create token] is the virtual filesystem in which GitHub API calls
      are replaced by filesystem accesses. *)

end
