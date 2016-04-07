open Result

module Make (Tree : I9p_tree.S) : sig
  type t

  val of_dir : Tree.Dir.t -> t

  val root : t -> Tree.Dir.t

  val update : t -> I9p_tree.path -> string -> Cstruct.t -> (unit, [`Is_a_directory | `Not_a_directory]) result Lwt.t
  (** [update t dir leaf data] makes [dir/leaf] be the file [data].
      Missing directories may be created. If [dir/leaf] is a file then it is overwritten.
      Fails if [dir/leaf] is a directory, or any component of [dir] is not a directory. *)

  val remove : t -> I9p_tree.path -> string -> (unit, [`Not_a_directory]) result Lwt.t
  (** [remove t dir leaf] ensures that [dir/leaf] does not exist.
      Fails if any component of [dir] is not a directory. *)

  val update_force : t -> I9p_tree.path -> string -> Cstruct.t -> unit Lwt.t
  (** [update_force t path leaf value] ensures that [path/leaf] is a file containing [value].
      Any existing files and directories that are in the way are destroyed. *)

  val remove_force : t -> I9p_tree.path -> string -> unit Lwt.t
  (** [remove_force t path leaf] ensures that [path/leaf] does not exist.
      This will delete the entire subtree if [path/leaf] is a directory.
      It does nothing if  [path/leaf] does not exist. *)

  val rename : t -> I9p_tree.path -> old_name:string -> new_name:string ->
    (unit, [`Is_a_directory | `Not_a_directory | `No_such_item]) result Lwt.t
  (** [rename t path ~old_name ~new_name] ensures that [path/new_name] points to whatever
      [path/old_name] previously did, and that [path/old_name] no longer exists (atomically).
      It is an error if [path/old_name] does not exist or if [path/new_name] already exists
      as a directory. *)
end
