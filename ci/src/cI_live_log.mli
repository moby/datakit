(** An in-memory buffer where log messages are written during a build. *)

type manager

val create_manager : unit -> manager

type t
type stream

val create : ?switch:Lwt_switch.t -> pending:string -> branch:string -> title:string -> manager -> t
(** [create ~pending ~branch ~title manager] is a fresh, empty log with pending reason [pending].
    It is an error to have two live logs on the same branch at the same time (finish the other one first). *)

val title : t -> string
(** [title t] is the title, as passed to [create]. *)

val lookup : branch:string -> manager -> t option
(** [lookup ~branch manager] is the currently-active log for [branch]. *)

val branch : t -> string
(** [branch t] is the branch to which this log will be written when finished. *)

val stream : t -> stream Lwt.t
(** [stream t] reads the contents of the log as a stream. *)

val write : t -> string -> unit
(** [write t msg] appends [msg] to the log. *)

val printf : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [printf t fmt] appends a formatted message to the log. *)

val log : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [log t fmt] appends a formatted message to the log, with a newline added at the end. *)

val heading : t -> ('a, Format.formatter, unit, unit) format4 -> 'a
(** [heading t fmt] appends a formatted message to the log as a heading. *)

val contents : t -> string
(** [contents t] is the current contents of the buffer. *)

val pending : t -> string * [`Continue of unit Lwt.t | `Stop]
(** [pending t] is the current pending reason of the buffer and a thread that will
    resolve next time it changes. If it returns [`Stop] then there will be no further changes. *)

val with_pending_reason : t -> string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
(** [with_pending_reason t msg fn] calls [fn ()]. If it gets a sleeping thread, then it
    pushes [msg] onto the pending-reason stack, waits for the thread to finish, and then
    removes the pending message. *)

val enter_with_pending_reason : t -> string -> (('a -> 'b Lwt.t) -> 'b Lwt.t) -> ('a -> 'b Lwt.t) -> 'b Lwt.t
(** [enter_with_pending_reason t msg use fn] is like [use fn], but posts [msg] as the pending reason until [fn] is called
    (or [use] fails).
    This is useful to give a pending reason while getting a mutex or pool resource. *)

val finish : t -> unit
(** [finish t] prevents any further changes and notifies anyone waiting on [pending]. *)

val can_cancel : t -> bool
(** [can_cancel t] indicates whether [cancel t] will succeed. *)

val cancel : t -> (unit, string) result Lwt.t
(** [cancel t] turns off [t]'s switch, or returns an error if [t] cannot be cancelled. *)
