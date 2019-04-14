(** [DataKit] is a concrete implementation of an Irmin store. It also
    allow to expose such store as a VFS directory. *)

(** {1 Concrete Store} *)

(** [Hash] is SHA1. *)
module Hash : Irmin.Hash.S with type t = Irmin.Hash.SHA1.t

(** [Path] are list of strings, with constant-time [rdecons]
    operations (e.g. to have efficient basename/dirname split). *)
module Path : Irmin.Path.S with type step = string

(** [Metadata] are similar to Git metadata. *)
module Metadata : Irmin.Metadata.S with type t = [ `Normal | `Exec | `Link ]

(** [Branch] are ASCII strings where ['/'] is also allowed. *)
module Branch : Irmin.Branch.S with type t = string

(** [Blob] are lists of cstructs, with constant-time append
    operations. This is optimized to store large files where contents
    is always appended (e.g. log files). *)
module Blob : sig
  include Irmin.Contents.S

  val empty : t
  (** The empty blob. *)

  val write : t -> offset:int64 -> Cstruct.t -> (t, Vfs.Error.t) result
  (** [write t ~offset data] is the blob [t] overwritten with [data] at
      [offset].  The new blob is extended and zero-filled as necessary.
      If [offset = len t] then this is very fast. *)

  val len : t -> int64

  val truncate : t -> int64 -> (t, Vfs.Error.t) result
  (** [truncate t l] is a blob of length [l]. If [l <= len t] then it is
      the prefix of [t] of length [l]. If [l > len t] then it is [t]
      followed by enough zero bytes to make up the length. Return an
      error if the requested length is negative. *)

  val read : t -> offset:int64 -> count:int -> (Cstruct.t, Vfs.Error.t) result
  (** [read t ~offset ~count] is the [count] bytes in [t] starting at
      [offset]. If the blob is not long enough to return [count] bytes,
      then it returns as many as possible. Return an error if [offset <
      0 || offset > len t]. *)

  val ro_cstruct : Cstruct.t -> t
  (** [ro_cstruct c] is a blob containing the data [c]. Note that we
      take a reference to [c] rather than copying, so [c] MUST NOT be
      modified after this call. [c] may also be shared with modified
      versions of the resulting blob. *)

  val to_ro_cstruct : t -> Cstruct.t
  (** [to_ro_cstruct t] is a read-only Cstruct with the same data as
      [t]. DO NOT modify this - it may corrupt the blob or other blobs
      sharing data with this one if you do. *)

  val string : string -> t
  (** [string s] is a blob containing the same data as [s]. *)

  val to_string : t -> string
  (** [to_string t] is a string containing the same data as [t]. *)

  val compare : t -> t -> int
  (** [compare] is the comparison function for blobs. It is {b very}
      slow, so use it with care! *)
end

(** {1 Types} *)

(** The type for SHA1 hashes. *)
type hash = Hash.t

(** The type for store paths *)
type path = Path.t

(** The type for store steps. *)
type step = Path.step

(** The type for file permissions. *)
type perm = Metadata.t

(** The type for branch names. *)
type branch = Branch.t

(** The type for blob contents. *)
type blob = Blob.t

(** The type for DataKit stores. Similar to [Irmin_git.S] but with
    specialized contents (optimized for append-operations), paths and
    branches. *)
module type S =
  Irmin.S
  with type key = path
   and type contents = blob
   and type branch = string
   and type step = step
   and type metadata = perm
   and type Commit.Hash.t = hash
   and type Tree.Hash.t = hash
   and type Contents.Hash.t = hash

(** Make an DataKit store from a normal Irmin backend. *)
module Make (M : Irmin.S_MAKER) : S

(** Similar to [Irmin_git.S_MAKER] *)
module type GIT_S_MAKER = functor
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  -> Irmin.S
     with type key = P.t
      and type step = P.step
      and module Key = P
      and type contents = C.t
      and type branch = B.t
      and type metadata = perm
      and type Commit.Hash.t = hash
      and type Tree.Hash.t = hash
      and type Contents.Hash.t = hash

(** Make a DataKit store from a Git-like Irmin backend. *)
module Make_git (M : GIT_S_MAKER) : S

(** {1 VFS} *)

(** The signature of an Irmin VFS servers. *)
module type VFS = sig
  (** The type for repositories. *)
  type repo

  val create : info:(string -> Irmin.Info.t) -> repo -> Vfs.Dir.t
  (** [create ~info repo] is the root directory of the filesystem for
      the Irmin repository [repo]. [info] is used to create timestamped
      commit messages for changes. *)
end

(** Writable directories. *)
module Dir (Store : S) : sig
  (** The type for writable directories. *)
  type t

  val v : Store.Repo.t -> Store.tree -> t
  (** [v t tree] is the directory initially containing the contents of
      [tree] in the store [t]. *)

  val root : t -> Store.tree
  (** [root t] is the tree corresponding to the current directory. *)

  val update :
    t ->
    Path.t ->
    string ->
    Blob.t * [ Metadata.t | `Keep ] ->
    (unit, [ `Is_a_directory | `Not_a_directory ]) result Lwt.t
  (** [update t dir leaf data] makes [dir/leaf] be the file [data].
      Missing directories may be created. If [dir/leaf] is a file then
      it is overwritten. Fails if [dir/leaf] is a directory, or any
      component of [dir] is not a directory. *)

  val remove :
    t -> Path.t -> string -> (unit, [ `Not_a_directory ]) result Lwt.t
  (** [remove t dir leaf] ensures that [dir/leaf] does not exist.
      Fails if any component of [dir] is not a directory. *)

  val chmod :
    t ->
    Path.t ->
    string ->
    Vfs.perm ->
    (unit, [ `Is_a_directory | `Not_a_directory | `No_such_item ]) result Lwt.t
  (** [chmod t dir leaf perm] changes the type of [dir/leaf] to
      [perm]. Fails if any component of [dir] is not a directory, or
      [perm] is incompatible with the type of the item being
      changed. *)

  val update_force : t -> path -> string -> blob * perm -> unit Lwt.t
  (** [update_force t path leaf value] ensures that [path/leaf] is a
      file containing [value]. Any existing files and directories that
      are in the way are destroyed. *)

  val remove_force : t -> path -> string -> unit Lwt.t
  (** [remove_force t path leaf] ensures that [path/leaf] does not
      exist. This will delete the entire subtree if [path/leaf] is a
      directory. It does nothing if [path/leaf] does not exist. *)

  val rename :
    t ->
    path ->
    old_name:string ->
    new_name:string ->
    (unit, [ `Is_a_directory | `Not_a_directory | `No_such_item ]) result Lwt.t
  (** [rename t path ~old_name ~new_name] ensures that [path/new_name]
        points to whatever [path/old_name] previously did, and that
        [path/old_name] no longer exists (atomically).  It is an error
        if [path/old_name] does not exist or if [path/new_name] already
        exists as a directory. *)
end

(** Create a full VFS from a DataKit store. *)
module Vfs (Store : S) : VFS with type repo = Store.Repo.t
