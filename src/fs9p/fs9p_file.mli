(** File in the filesystem. *)

open Fs9p_misc

type t
(** The type for files. *)

type open_file
(** The type for open files. *)

val size: t -> int64 or_err Lwt.t
(** [size t] is [t]'s size. *)

val open_: t -> open_file or_err Lwt.t
(** [open_ t] opens [t]. *)

val remove: t -> unit or_err Lwt.t
(** [remove t] removes [t]. *)

val read: open_file -> offset:int64 -> count:int -> Cstruct.t or_err Lwt.t
(** [read f ~offset ~count] reads an open file. *)

val write : open_file -> offset:int64 -> Cstruct.t -> unit or_err Lwt.t
(** [write f ~offset] writes in a open file. *)

val truncate: t -> int64 -> unit or_err Lwt.t
(** [truncate t len] sets the length of [t] to [len].
    If the new length is shorter, the file is truncated.
    If longer, it is padded with zeroes. *)

(** {1 Basic constructors} *)

val static: Cstruct.t -> t
(** [static x] is the static file containing [x]. *)

val static_string: string -> t
(** [static_string] is similar to [static] but for strings. *)

val mutable_string: string -> t * (unit -> string)
(** [mutable_string init] is a mutable file that initially contains [init]
    and a function which can be called to get the current contents. *)

val status: (unit -> string Lwt.t) -> t
(** [status f] is the file containing the result of [f]. [f] is
    evaluated everytime the file is open. *)

val command: (string -> string or_err Lwt.t) -> t
(** [command f] is the file containing the result of [f]. [f] is
    evaluated on every write. *)

(** {1 K/V stores.} *)

val read_write:
  read:(unit -> Cstruct.t option or_err Lwt.t) ->
  write:(Cstruct.t -> unit or_err Lwt.t) ->
  remove:(unit -> unit or_err Lwt.t) -> t
(** [read_write ~read ~write ~remove read] interprets values from a
    k/v store as files. Handles reading and writing regions of the
    file. *)

val read_only: read:(unit -> Cstruct.t option or_err Lwt.t) -> t
(** [read_only ~read] is similar to {!read_write} but for read-only
    values. *)

(** {1 Streams} *)

type stream =
  (int -> Cstruct.t or_err Lwt.t) * (Cstruct.t -> unit or_err Lwt.t)
(** The type of streams. A stream is a pair of a reader and a
    writer. *)

val open_file_of_stream: stream -> open_file
(** [open_file_of_stream s] is the open file obtained reading and
    writing the stream. *)

val of_stream: (unit -> stream Lwt.t) -> t
(** [of_stream s] is the file which will be, once open, similar to the
    stream [s ()]. *)
