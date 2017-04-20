(** Expose an Irmin store as a VFS directory. *)

(** The signature of an Irmin VFS servers. *)
module type S = sig

  type repo
  (** The type for repositories. *)

  val create: info:(string -> Irmin.Info.t) -> repo -> Vfs.Dir.t
  (** [create ~info repo] is the root directory of the filesystem for
      the Irmin repository [repo]. [info] is used to create timestamped
      commit messages for changes. *)

end

module Make (Store : Ivfs_tree.S): S with type repo = Store.Repo.t
