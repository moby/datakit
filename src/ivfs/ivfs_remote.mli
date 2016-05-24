(** Managing Git remotes. *)

module Make (Store : Ivfs_tree.STORE): sig

  type t
  (** The type for values holding state about Git remotes. *)

  type remote = {
    name: string;
    url : string option;
  }
  (** The type for individual Git remotes. *)

  val list: t -> remote list
  (** [list t] is the list of remotes stored in [t]. *)

  val add: t -> remote -> unit
  (** [add t r] adds the remote [r] to [t]. This does nothing if a
      remote with the same name already exists. *)

  val fetch: t -> name:string -> branch:string -> unit Vfs.or_err
  (** [fetch t n b] fetches [b]'s branch from the remote named [n]. It
      adds [r] in [t] if not already there. If a remote with the same
      name but a different url already exists, it just overwrite it. *)

  val root: t -> Vfs.Dir.t
  (** [root t] is the [/remotes/] virtual directory. *)

  val create: ?init:remote list -> Store.Repo.t -> t
  (** [create ?init r] is a fresh value holding state for Git remotes
      for the repository [r]. [init] is a pair of initial remotes. *)

end
