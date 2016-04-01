(** Virtual filesystem.

    A virtual filesystem is an abstract description of {{!File}files},
    {{!Dir}directoires}, {{!Inode}inodes} and {{!Error}error codes}.
*)

open Astring
open Result

(** Error codes. *)
module Error: sig

  type err = { errno: int32 option; descr: string }
  (** The type for generic errors. *)

  (** The type for FS errors. *)
  type t =
    | Noent                                    (** No such file or directory. *)
    | Isdir                                     (** The entry is a directory. *)
    | Notdir                                (** The entry is not a directory. *)
    | Read_only_file                               (** The file is read-only. *)
    | Perm                                (** The operation is not permitted. *)
    | Other of err                                (** Generic error function. *)

  (** Infix operators. *)
  module Infix: sig
    val (>>*=):
      ('a, t) result Lwt.t -> ('a -> ('b, t) result Lwt.t) -> ('b, t) result Lwt.t
  end

  val no_entry: ('a, t) result
  (** [no_entry] is [Error Noent]. *)

  val is_dir: ('a, t) result
  (** [is_dir] is [Error Isdir]. *)

  val not_dir: ('a, t) result
  (** [not_dir] is [Error Notdir]. *)

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

val ok: 'a -> 'a or_err
(** [ok x] is [Lwt.return (Ok x)] *)

val error: ('a, unit, string, 'b or_err) format4 -> 'a
(** [error fmt] is [Lwt.return (Error <fmt>)]. *)

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

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for files. *)

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

  val command: ?init:string -> (string -> string or_err) -> t
  (** [command ?init f] is the file containing the result of [f]. [f]
      is evaluated on every write, with the contents of the file as
      argument. Initially the file contains [init]. *)

  (** {1 K/V stores.} *)

  val of_kv:
    read:(unit -> Cstruct.t option or_err) ->
    write:(Cstruct.t -> unit or_err) ->
    remove:(unit -> unit or_err) -> t
  (** [of_kv ~read ~write ~remove read] interprets values from a k/v
      store as files. Handles reading and writing regions of the
      file. *)

  val of_kvro: read:(unit -> Cstruct.t option or_err) -> t
  (** [of_kvro ~read] is similar to {!of_kv} but for read-only
      values. *)

  (** {1 Streams} *)

  module Stream: sig

    type t
    (** The type of streams.  *)

    (** A stream is a reader and a writer. *)
    val create:
      read:(int -> Cstruct.t or_err) -> write:(Cstruct.t -> unit or_err) -> t

    (** A watch stream file that initially Lwt.returns
        [Fmt.to_to_string pp initial]. Further reads call [wait x],
        where [x] is the value previously Lwt.returned and then
        Lwt.return that, until the file is closed. *)
    val watch: 'a Fmt.t -> init:'a -> wait:('a -> 'a Lwt.t) -> t

  end

  val of_stream: (unit -> Stream.t Lwt.t) -> t
  (** [of_stream s] is the file which will be, once opened, similar to
      the stream [s ()]. *)

  (** {1 Errors} *)

  val err_no_entry: 'a or_err

end

(** Directory operations. *)
module rec Dir: sig

  type t
  (** The type for directories. *)

  val pp: t Fmt.t
  (** [pp] is a pretty-printer for directories. *)

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

  val of_list: (unit -> Inode.t list) -> t
  (** [of_list l] is a read-only, static directory containing only the
      inodes [l]. The sub-directories are re-evaluated on every [ls]
      and [read]. *)

  val of_map_ref: Inode.t String.Map.t ref -> t
  (** [of_map_ref m] is a read-only directory containing the inodes
      defined in [m]. The content of the directory is computed
      dynamically by accessing elements in the map on every access. *)

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
  val err_no_entry: 'a or_err

end and Inode: sig

  (** Inode.t operations. *)

  type t
  (** The type for inodes. *)

  val pp: t Fmt.t
  (** [pp] is the pretty-printer for inodes. *)

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

  val set_basename: t -> string -> unit
  (** [set_basename t name] changes [t]'s basename to [name]. *)

  val kind: t -> kind
  (** [kind t] is [t]'s kind. *)

end
