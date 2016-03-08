(** Virtual filesystem.

    A virtual filesystem is an abstract description of {!{files}File},
    {!{directories}Dir}, {!{inodes}Inode} and {!{error codes}Error}}.
*)

open Result

(** Error codes. *)
module Error: sig

  type err = { errno: int32 option; descr: string }
  (** The type for generic errors. *)

  (** The type for FS errors. *)
  type t =
    | Noent                                    (** No such file or directory. *)
    | Isdir                                     (** The entry is a directory. *)
    | Read_only_file                               (** The file is read-only. *)
    | Perm                                (** The operation is not permitted. *)
    | Other of err                                (** Generic error function. *)

  (** Infix operators. *)
  module Infix: sig
    val (>>*=):
      ('a, t) result Lwt.t -> ('a -> ('b, t) result Lwt.t) -> ('b, t) result Lwt.t
  end

  val noent: ('a, t) result
  (** [enoent] is [Error Noent]. *)

  val isdir: ('a, t) result
  (** [isdir] is [Error Isdir]. *)

  val read_only_file: ('a, t) result
  (** [read_only_file] is [Error Read_only_file]. *)

  val perm: ('a, t) result
  (** [perm] is [Error Perm]. *)

  val other: ?errno:int32 -> ('a, unit, string, ('b, t) result) format4 -> 'a
  (** [Other ~errno descr] is [Error { errno; descr }]. [errno] is 0
      if not set. *)

end

type 'a or_err = ('a, Error.t) Result.result Lwt.t
(** The type of errors. *)

(** File operations. *)
module File: sig

  type fd
  (** The type for open files, e.g. file descriptors. *)

  val read: fd -> offset:int64 -> count:int -> Cstruct.t or_err
  (** [read f ~offset ~count] reads an open file. *)

  val write : fd -> offset:int64 -> Cstruct.t -> unit or_err
  (** [write f ~offset] writes in a open file. *)

  type t
  (** The type for files. *)

  val create:
    size:(unit -> int64 or_err) ->
    open_:(unit -> fd or_err) ->
    remove:(unit -> unit or_err) ->
    truncate:(int64 -> unit or_err) ->
    t
  (** [create] is the file [t] such that FIXME. *)

  val size: t -> int64 or_err
  (** [size t] is [t]'s size. *)

  val open_: t -> fd or_err
  (** [open_ t] if a file-descriptor for [t]. *)

  val remove: t -> unit or_err
  (** [remove t] removes [t]. *)

  val truncate: t -> int64 -> unit or_err
  (** [truncate t len] sets the length of [t] to [len].  If the new
       length is shorter, the file is truncated.  If longer, it is
       padded with zeroes. *)

  (** {1 Basic constructors} *)

  val ro_of_string: string -> t
  (** [ro_of_string s] is the static file containing [s]. *)

  val rw_of_string: string -> t * (unit -> string)
  (** [rw_of_string init] is a mutable file that initially contains
        [init] and a function which can be called to get the current
        contents. *)

  val status: (unit -> string Lwt.t) -> t
  (** [status f] is the file containing the result of [f]. [f] is
      evaluated everytime the file is open. *)

  val command: (string -> string or_err) -> t
  (** [command f] is the file containing the result of [f]. [f] is
      evaluated on every write. *)

  (** {1 K/V stores.} *)

  val of_kv:
    read:(unit -> Cstruct.t option or_err) ->
    write:(Cstruct.t -> unit or_err) ->
    remove:(unit -> unit or_err) -> t
  (** [of_kv ~read ~write ~remove read] interprets values from a k/v
      store as files. Handles reading and writing regions of the
      file. *)

  val of_kvro: read:(unit -> Cstruct.t option or_err) -> t
  (** [of_kvro ~read] is similar to {!read_write} but for read-only
      values. *)

  (** {1 Streams} *)

  module Stream: sig

    type t
    (** The type of streams.  *)

    (** A stream is a reader and a writer. *)
    val create:
      read:(int -> Cstruct.t or_err) -> write:(Cstruct.t -> unit or_err) -> t

  end

  val of_stream: (unit -> Stream.t Lwt.t) -> t
  (** [of_stream s] is the file which will be, once opened, similar to
      the stream [s ()]. *)

end

(** Directory operations. *)
module rec Dir: sig

  type t
  (** The type for directories. *)

  val ls: t -> Inode.t list or_err
  (** The [ls] commands. *)

  val mkfile: t -> string -> Inode.t or_err
  (** The [mkfile] command. *)

  val lookup: t -> string -> Inode.t or_err
  (** The [lookup] command. *)

  val mkdir: t -> string -> Inode.t or_err
  (** The [mkdir] command. *)

  val remove: t -> unit or_err
  (** The [remove] command. FIXME: shouldn't it be string -> unit? *)

  val rename: t -> Inode.t -> string -> unit or_err
  (** The [rename] command. *)

  val empty: t
  (** [empty] is the empty directory. *)

  val of_list: Inode.t list -> t
  (** [of_list l] is a read-only, static directory containing only the
      inodes [l]. *)

  val of_map: Inode.t Map.Make(String).t ref -> t
  (** [of_map tbl] is a read-only directory containing the inodes in
      [tbl]. The content of the directory is computed dynamically by
      accessing elements in the table on every access. *)

  val directories:
    make:(remover:unit Lwt.t lazy_t -> string -> t or_err) ->
    init:Inode.t list -> t
  (** [directories ~make ~init] is a directory that initially contains
      [init].  If the user tries to make a new subdirectory, [make
      ~remover name] is called to create it. To delete the new
      directory, force [remover]. *)

  (** [read_only] is a read-only directory. FIXME. *)
  val read_only:
    ls:(unit -> Inode.t list or_err) ->
    lookup:(string -> Inode.t or_err) ->
    remove:(unit -> unit or_err) ->
    t

  (** [dir_only] is a directory which contains only
      directories. FIXME. *)
  val dir_only:
    ls:(unit -> Inode.t list or_err) ->
    lookup:(string -> Inode.t or_err) ->
    mkdir:(string -> Inode.t or_err) ->
    remove:(unit -> unit or_err) ->
    rename:(Inode.t -> string -> unit or_err) ->
    t

  (** [creae] is a generic directory. *)
  val create:
    ls:(unit -> Inode.t list or_err) ->
    mkfile:(string -> Inode.t or_err) ->
    lookup:(string -> Inode.t or_err) ->
    mkdir:(string -> Inode.t or_err) ->
    remove:(unit -> unit or_err) ->
    rename:(Inode.t -> string -> unit or_err) ->
    t

  (** {1 Errors} *)
  val err_read_only: 'a or_err
  val err_already_exists: 'a or_err
  val err_dir_only: 'a or_err
  val err_enoent: 'a or_err

end and Inode: sig

  (** Inode.t operations. *)

  type t
  (** The type for inodes. *)

  type kind = [`File of File.t | `Dir of Dir.t]
  (** The type for inode kinds. *)

  val file: string -> File.t -> t
  (** [file name f] is the inode [t] such that [basename t] is [name]
      and [kind t] is [File f]. *)

  val dir: string -> Dir.t -> t
  (** [dir name d] is the inode [t] such that [basename t] is [name]
      and [kind t] is [Dir d]. *)

  val basename: t -> string
  (** [basenane t] is [t]'s basename. *)

  val kind: t -> kind
  (** [kind t] is [t]'s kind. *)

end
