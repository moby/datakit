module Make (API: Datakit_github.API): sig

  val root: API.token -> Vfs.Dir.t
  (** [root token] is the root of the virtual filesystem in which
      GitHub API calls are replaced by filesystem accesses. *)

end
