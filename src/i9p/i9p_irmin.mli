(** Expose an Irmin store as a 9p filesystem. *)

(** The signature of an Irmin9p server. *)
module type S = sig

  type repo
  (** The type for repositories. *)

  type dir
  (** The type for directories. *)

  val create: string Irmin.Task.f -> repo -> dir
  (** [create task repo] is the root directory of the 9p filesystem for the
      Irmin repository [repo].
      [task] is used to create timestamped commit messages for changes. *)

end

module Make (Inode : I9p_inode.S) (Store : I9p_tree.STORE):
  S with type repo = Store.Repo.t and type dir = Inode.dir
