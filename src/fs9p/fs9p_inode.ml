(** Objects (file or directory) in the filesystem. *)

open Fs9p_misc

module type S = sig
  type t

  type dir = <
    ls : t list or_err Lwt.t;
    (* [ls] returns the files and sub-directories. *)

    create : string -> t or_err Lwt.t;
    (* [create name] create a new child. *)

    mkdir : string -> t or_err Lwt.t;
    lookup : string -> t or_err Lwt.t;
    remove : unit or_err Lwt.t;
    rename : t -> string -> unit or_err Lwt.t;
    (* [rename inode new_name] renames direct child [inode] to [new_name]. *)
  >

  val of_file: string -> Fs9p_file.t -> t
  val of_dir: string -> dir -> t
  val basename: t -> string
end
