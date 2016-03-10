(** Expose Irmin internal trees (to be upstreamed).

    Irmin's public API exposes a key/value store, but we need to see
    directory nodes too.  This module uses Irmin's private API to get
    that information.
*)

type leaf = string
type path = string list

module type STORE = Irmin.S
  with type key = string list
   and type value = string
   and type branch_id = string
   and module Key = Irmin.Path.String_list
   and type Private.Node.Val.step = string

module type S = sig

  type store
  (** The type for Irmin stores. *)

  type file
  (** The type for file contents. *)

  type repo
  (** The type for Irmin repositories. *)

  type dir
  (** An immutable directory node.
      Note: normally we treat an empty directory as an error, but we do allow it
      for the root. *)

  val empty : repo -> dir

  val snapshot : store -> dir Lwt.t
  (** Convert a branch, which may update while we read it, to a fixed directory
      by reading its current root. *)

  val ty : dir -> leaf -> [`File | `Directory | `None] Lwt.t
  (** Check the type of a path. *)

  val node : dir -> path -> [`File of file | `Directory of dir | `None ] Lwt.t
  (** Look up an item by path. *)

  val get_dir : dir -> path -> dir option Lwt.t
  (** Look up a sub-directory node. *)

  val ls : dir -> ([`File | `Directory] * leaf) list Lwt.t
  (** List the contents of a directory with the type of each item. *)

  val equal : dir -> dir -> bool
  (** Quick equality check (compared hashes). *)

  val repo : dir -> repo
  (** [repo dir] is the repository in which [dir] lives. *)

  (** {1 Directory IDs} *)

  type hash
  (** The type for directory IDs (hashes). *)

  val of_dir_hash : repo -> hash option -> dir
  (** [of_dir_hash repo h] is the directory whose hash is [h] in
      [repo]. *)

  val hash : dir -> hash option
  (** [hash dir] is the [dir]'s hash. *)

end

module Make (Store: STORE):
  S with type store = Store.t
     and type file = Store.Private.Node.Val.contents
     and type repo = Store.Repo.t
     and type hash = Store.Private.Node.Key.t
