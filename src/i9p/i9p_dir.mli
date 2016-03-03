(** Directory in the filesystem. *)

open I9p_misc

module InodeMap : Map.S with type key = string

module Make(Inode : I9p_inode.S) : sig
  type t = Inode.dir
  (** A directory. *)

  val fixed: Inode.t list -> t

  val of_map_ref : Inode.t InodeMap.t ref -> t
  (** [of_map m] is a read-only directory that follows the contents of [m],
      which may be updated at any time. *)

  val directories:
    (remover:unit Lwt.t lazy_t -> string -> t or_err Lwt.t) ->
    Inode.t list -> t
  (** [directories make init] is a directory that initially contains [init].
      If the user tries to make a new subdirectory, [make ~remover name] is called to create it.
      To delete the new directory, force [remover]. *)

  val read_only:
    ls:(unit -> Inode.t list or_err Lwt.t) ->
    lookup:(string -> Inode.t or_err Lwt.t) ->
    remove:(unit -> unit or_err Lwt.t) ->
    ?rename: (Inode.t -> string -> unit or_err Lwt.t) ->
    unit ->
    t

  val dir_only:
    ls: (unit -> Inode.t list or_err Lwt.t) ->
    mkdir: (string -> Inode.t or_err Lwt.t) ->
    lookup: (string -> Inode.t or_err Lwt.t) ->
    remove: (unit -> unit or_err Lwt.t) ->
    ?rename: (Inode.t -> string -> unit or_err Lwt.t) ->
    unit ->
    t

  val read_write:
    ls: (unit -> Inode.t list or_err Lwt.t) ->
    create : (string -> Inode.t or_err Lwt.t) ->
    mkdir: (string -> Inode.t or_err Lwt.t) ->
    lookup: (string -> Inode.t or_err Lwt.t) ->
    remove: (unit -> unit or_err Lwt.t) ->
    t
end

(** {1 Errors} *)

val err_ro: 'a or_err Lwt.t
val err_dir_only: 'a or_err Lwt.t
