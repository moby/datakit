(** Expose Irmin internal trees (to be upstreamed).

    Irmin's public API exposes a key/value store, but we need to see
    directory nodes too.  This module uses Irmin's private API to get
    that information.
*)

open Astring

type step = string
type path = string list
type perm = [ `Normal | `Exec | `Link ]

module type STORE = Irmin.S
  with type key = string list
   and type value = string
   and type branch_id = string
   and module Key = Irmin.Path.String_list
   and type Private.Node.Val.step = string
   and type Private.Node.Val.Metadata.t = perm

module type S = sig

  type store
  (** The type for Irmin stores. *)

  type repo
  (** The type for Irmin repositories. *)

  module File : sig
    type t
    (** The type for file contents. *)

    type hash
    (** The type for file IDs (hashes). *)

    val of_data : repo -> Ivfs_blob.t -> t
    (** [of_data repo data] is a file containing [data], which may later
        be stored in [repo]. *)

    val hash : t -> hash Lwt.t
    (** [hash f] is the hash of the contents of [f]. If [f] is not yet in [repo], calling this
        will cause it to be written. *)

    val content : t -> Ivfs_blob.t Lwt.t

    val equal : t -> t -> bool
    (** Quick equality check (compares hashes). *)

    val pp_hash : Format.formatter -> hash -> unit

    val size : t -> int64 Lwt.t
  end

  module Dir : sig
    type t
    (** An immutable directory node.
        Note: normally we treat an empty directory as an error, but we do allow it
        for the root. *)

    type hash
    (** The type for directory IDs (hashes). *)

    val empty : repo -> t

    val ty : t -> step -> [`File | `Directory | `None] Lwt.t
    (** Check the type of a path. *)

    val lookup : t -> step -> [`File of File.t * perm | `Directory of t | `None ] Lwt.t
    (** Look up an immediate child by name. *)

    val lookup_path : t -> path -> [`File of File.t * perm | `Directory of t | `None ] Lwt.t
    (** Look up an item by path. *)

    val get : t -> path -> t option Lwt.t
    (** Look up a sub-directory node. *)

    val map : t -> [`File of File.t * perm | `Directory of t] String.Map.t Lwt.t
    (** The contents of the directory. *)

    val ls : t -> ([`File | `Directory] * step) list Lwt.t
    (** List the contents of a directory with the type of each item. *)

    val of_hash : repo -> hash -> t
    (** [of_hash repo h] is the directory whose hash is [h] in [repo]. *)

    val hash : t -> hash Lwt.t
    (** [hash dir] is [dir]'s hash. This writes [dir] to the repository if it's not
        yet stored. *)

    val repo : t -> repo
    (** [repo dir] is the repository in which [dir] lives. *)

    val with_child : t -> step -> [`File of File.t * perm | `Directory of t] -> t Lwt.t
    (** [with_child dir name child] is a copy of [dir] except that [name] links to [child]. *)

    val without_child : t -> step -> t Lwt.t
    (** [without_child dir name] is a copy of [dir] except that it has no child called [name]. *)

  end

  val snapshot : store -> Dir.t Lwt.t
  (** Convert a branch, which may update while we read it, to a fixed directory
      by reading its current root. *)
end

module Make (Store: STORE):
  S with type store = Store.t
     and type repo = Store.Repo.t
     and type Dir.hash = Store.Private.Node.Key.t
