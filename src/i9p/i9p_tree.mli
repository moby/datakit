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

  module File : sig
    type t
    (** The type for file contents. *)

    val content : t -> string Lwt.t

    val equal : t -> t -> bool
    (** Quick equality check (compares hashes). *)

    val pp_hash : Format.formatter -> t -> unit
  end

  type repo
  (** The type for Irmin repositories. *)

  module Dir : sig
    type t
    (** An immutable directory node.
        Note: normally we treat an empty directory as an error, but we do allow it
        for the root. *)

    type hash
    (** The type for directory IDs (hashes). *)

    val empty : repo -> t

    val ty : t -> leaf -> [`File | `Directory | `None] Lwt.t
    (** Check the type of a path. *)

    val node : t -> path -> [`File of File.t | `Directory of t | `None ] Lwt.t
    (** Look up an item by path. *)

    val get : t -> path -> t option Lwt.t
    (** Look up a sub-directory node. *)

    val ls : t -> ([`File | `Directory] * leaf) list Lwt.t
    (** List the contents of a directory with the type of each item. *)

    val equal : t -> t -> bool
    (** Quick equality check (compares hashes). *)

    val of_hash : repo -> hash option -> t
    (** [of_hash repo h] is the directory whose hash is [h] in
        [repo]. *)

    val hash : t -> hash option
    (** [hash dir] is the [dir]'s hash. *)

    val repo : t -> repo
    (** [repo dir] is the repository in which [dir] lives. *)

  end

  val snapshot : store -> Dir.t Lwt.t
  (** Convert a branch, which may update while we read it, to a fixed directory
      by reading its current root. *)
end

module Make (Store: STORE):
  S with type store = Store.t
     and type repo = Store.Repo.t
     and type Dir.hash = Store.Private.Node.Key.t
