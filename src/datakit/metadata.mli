(** Similar to Irmin.Branch.String but allow '/' in branch names. *)

include Irmin.Metadata.S with type t = [ `Normal | `Exec | `Link ]
