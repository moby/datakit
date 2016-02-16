(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Store API. *)

type t
(** The type for store handlers. *)

val remote: ?uri:Uri.t -> unit -> t Lwt.t
(** Create a remote store handler, using Irmin's HTTP client. [uri] is
    the location of the Irmin daemon. *)

val local: ?root:string -> unit -> t Lwt.t
(** Create a local store handler, using Irmin's Git on-disk
    backend. [root] is the filesystem location to the Git repository
    holding the store contents. *)

type 'a callback = 'a -> unit Lwt.t
(** The type for callbacks. *)

type cancel = unit callback
(** The type for watch cancelling functions. *)

val with_transaction:
  ?retry:int -> t -> string ->  (t -> unit Lwt.t) -> unit Lwt.t
(** [with_transaction t f] executes [f t] in a transaction and commit
    the final result if the transaction is successful. Will retry
    multiple times in case of conflict (default is 5) with an
    exponential back-off. Raise [Invalid_argument] if the transaction
    is still not successful after all the retries. *)

val cancel_all_watches: t -> unit Lwt.t
(** [cancel_all_watches t] cancel all the watches set-up on [t]. *)

val nb_watches: t -> int
(** [nb_watches t] is the number of watches set on [t]. *)

(** The signature for objects kept in the store. *)
module type S = sig

  type id
  (** Type type for stable identifier of values kept in the store. *)

  type value
  (** The type for values kept in the store. *)

  val add: t -> value -> unit Lwt.t
  (** [add t v] adds [v] to the store [t]. *)

  val mem: t -> id -> bool Lwt.t
  (** [mem t id] is true if a value with the stable identifer [id] is
      stored in [t]. *)

  val get: t -> id -> value Lwt.t
  (** [get t id] is the value stored in [t] with the stable identifier
      [id]. Raise [Invalid_argument] if [id] is invalid. *)

  val list: t -> id list Lwt.t
  (** [list t] is the list of all the values stored in [t]. *)

  val forget: t -> id -> unit Lwt.t
  (** [forget t id] removes all metadata about the value. *)

end

(** Persisting state for workers. *)
module Worker: sig

  include S with type id := Worker.id and type value := Worker.t

  val tick: t -> Worker.id -> float -> unit Lwt.t
  (** [tick t w f] updates the worker [w]'s status with the timestamp
      [f]. [f] is supposed to tbe the local worker time, i.e. the
      current time since 00:00:00 GMT, Jan. 1, 1970, in seconds in the
      worker referential. *)

  val status: t -> Worker.id -> Worker.status option Lwt.t
  (** [job t w] is the worker [w]'s current job. [None] means that the
      worker is not alive anymore. *)

  val start_job: t -> Worker.id -> Job.id -> unit Lwt.t
  (** [start_job t w j] asks the worker [w] to start working on the
      build job [j]. *)

  val start_task: t -> Worker.id -> Task.id -> unit Lwt.t
  (** [start_task t w ta] asks the worker [w] to start working on the
      task [ta]. *)

  val idle: t -> Worker.id -> unit Lwt.t
  (** [idle t w] registers that [w] is idle. *)

  type diff = [`Added of Worker.t | `Removed of Worker.id]
  (** The type for worker diffs. *)

  val watch: t -> diff callback -> cancel Lwt.t
  (** [watch t f] calls [f] everytime a new worker is added. *)

  val watch_status: t -> Worker.id -> Worker.status option callback -> cancel Lwt.t
  (** [watch_status t w f] calls [f] everytime [w]'s status is
      updated. [None] means that the worker is not alive anymore. *)

  val watch_ticks: t -> Worker.id -> float callback -> cancel Lwt.t
  (** [watch_ticks t w f] calls [f] everytime the worker [w] calls
      {!tick}. Return a cancel function. *)

end

(** Persisting state for tasks. *)
module Task: sig

  include S with type id := Task.id and type value := Task.t

  val refresh_status: t -> Task.id -> unit Lwt.t
  (** [refresh_status t id] refreshes [id]'s status by looking at the
      status of its jobs. See {!Job.task_status}. *)

  val update_status: t -> Task.id -> Task.status -> unit Lwt.t
  (** [update_status t id s] set [id]'s status to [s]. *)

  val reset: t -> Task.id -> unit Lwt.t
  (** [reset t task] resets the status of [t] to be [`New]. *)

  val dispatch_to: t -> Task.id -> [`Worker] Id.t -> unit Lwt.t
  (** [dispatch_to t id w] dispatches the task [id] to the worker
      [w]. Set [id]'s state to be [`Dispatched (w, `Pending)]. *)

  val ack: t -> Task.id -> [`Worker] Id.t -> unit Lwt.t
  (** [ack t id w] is the acknoledgement of [w] that it is starting to
      work on [id]. Set [id]'s state to be [`Dispatched (w,
      `Started)]. *)

  val status: t -> Task.id -> Task.status option Lwt.t
  (** [status t task] is [task]'s status in [t]. *)

  val add_job: t -> Task.id -> Job.id -> unit Lwt.t
  (** [add_job t id j] add the job [j] to [id]. *)

  val jobs: t -> Task.id -> Job.id list Lwt.t
  (** [jobs t task] are [task]'s jobs in [t]. *)

  val watch: t -> Task.t callback -> cancel Lwt.t
  (** [watch t f] calls [f] on every task added in the store. *)

  val watch_status: t -> Task.id -> Task.status option callback -> cancel Lwt.t
  (** [watch_status t ta f] calls [f] everytime [ta]'s status is
      updated. *)

end

(** Persisting state for jobs. *)
module Job: sig

  include S with type id := Job.id and type value := Job.t

  val status: t -> Job.id -> Job.status option Lwt.t
  (** [status t job] is [job]'s status in [t]. *)

  val pending: t -> Job.id -> unit Lwt.t
  (** [pending t j] sets [j]'s status to [`Pending]. *)

  val runnable: t -> Job.id -> unit Lwt.t
  (** [runnable t j] set [j]'s status to [`Runnable]. *)

  val dispatch_to: t -> Job.id -> [`Worker] Id.t -> unit Lwt.t
  (** [dispatch_to t j w] sets [j]'s status to [`Dispatched (w,
      `Pending)]`. *)

  val ack: t -> Job.id -> [`Worker] Id.t -> unit Lwt.t
  (** [start t j w] sets [j]'s status to [`Dispatched (w,
      `Started)]. *)

  val success: t -> Job.id -> unit Lwt.t
  (** [success t j] sets [j]'s status to [`Success]. *)

  val failure: t -> Job.id -> unit Lwt.t
  (** [failure t j] set [j]'s status to [`Failure]. *)

  val add_output: t -> Job.id -> Source.t -> unit Lwt.t
  (** [add_output t j o] adds [o] to the object store and to the list
      of objects created by the job [j]. *)

  val outputs: t -> Job.id -> Source.id list Lwt.t
  (** [outputs t job] are [job]'s output objects. *)

  val watch: t -> Job.t callback -> cancel Lwt.t
  (** [watch t f] calls [f] on every job added in the store. *)

  val watch_status: t -> Job.id -> Job.status option callback -> cancel Lwt.t
  (** [watch_status t j f] calls [f] everytime [j]'s status is
      updated. *)

end

module Source: S with type id := Source.id and type value := Source.t
