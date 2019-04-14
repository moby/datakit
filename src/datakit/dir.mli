open Result

type path = Path.t

type perm = Metadata.t

type blob = Blob.t

module Make (Store : Store.S) : sig
  type t

  val v : Store.Repo.t -> Store.tree -> t

  val root : t -> Store.tree

  val update :
    t ->
    path ->
    string ->
    Blob.t * [ perm | `Keep ] ->
    (unit, [ `Is_a_directory | `Not_a_directory ]) result Lwt.t

  val remove : t -> path -> string -> (unit, [ `Not_a_directory ]) result Lwt.t

  val chmod :
    t ->
    path ->
    string ->
    Vfs.perm ->
    (unit, [ `Is_a_directory | `Not_a_directory | `No_such_item ]) result Lwt.t

  val update_force : t -> path -> string -> blob * perm -> unit Lwt.t

  val remove_force : t -> path -> string -> unit Lwt.t

  val rename :
    t ->
    path ->
    old_name:string ->
    new_name:string ->
    (unit, [ `Is_a_directory | `Not_a_directory | `No_such_item ]) result Lwt.t
end
