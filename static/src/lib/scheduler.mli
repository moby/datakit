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

(** Scheduler.

    The scheduler looks for task, job and worker events in the store
    and distribute work to the workers.

*)

(** The signature for schedulers. *)
module type S = sig

  type t
  (** The type for schedulers. *)

  type value
  (** The type of values which are scheduled. *)

  val start: Store.t -> t Lwt.t
  (** [start s] starts the event scheduler. *)

  val stop: t -> unit Lwt.t
  (** [stop t] stops the scheduler [t]. *)

  val list: t -> value list
  (** [list t] lists the values which are being scheduled. *)

  val is_runnable: t -> value -> bool
  (** [is_runnable t v] checks if [v] can be scheduled by [t]. *)

end

(** Task scheduler. *)
module Task: sig

  (** Tasks can only be added. When a new task is submitted by the
      users, the task scheduler start managing it. A task can later be
      cancelled. *)

  include S with type value := Task.t

  val peek: t -> Task.t option
  (** [peel t] picks a task if it is available. *)

  val peek_s: t -> Task.t Lwt.t
  (** [peek_s t] blocks until a task becomes available. *)

end

(** Job scheduler. *)
module Job: sig
  (** Jobs can only be added. Jobs are added by workers resolving new
      tasks (which then become pending). The job scheduler manages new
      jobs and check which ones are runnable. It also manage user
      cancellation. *)

  include S with type value := Job.t

  val peek: t -> Host.t -> Job.t option
  (** [peek t host] picks a job if it is runnable on the given host
      configuration. *)

  val peek_s: t -> Host.t -> Job.t Lwt.t
  (** [peek_s t host] blocks until a job become runnable on the given
      host configuration. *)

end

(** Worker scheduler. *)
module Worker: sig
  (** Workers can be added and can become inactive. The worker
      scheduler manage new workers, keep track of idle workers and
      remove inactive workers. *)

  include S with type value := Worker.t

  val peek: t -> Worker.kind -> Worker. t option
  (** [peek t k] picks a worker of kind [k]. *)

  val peek_s: t -> Worker.kind -> Worker.t Lwt.t
  (** [peek_s t k] blocks until a worker of kind [k] becomes
      available. *)

end

type t
(** The type for global schedulers. *)

val job: t -> Job.t
(** [job t] is [t]'s job scheduler. *)

val task: t -> Task.t
(** [task t] is [t]'s task scheduler. *)

val worker: t -> Worker.t
(** [worker t] is [t]'s work scheduler. *)

val start: Store.t -> t Lwt.t
(** [start s] connects the three schedulers: the {{!Worker.t}worker},
    the {{!Task.t}task} and the {{!Job.t}job} ones. *)

val stop: t -> unit Lwt.t
(** [stop t] stops the three schdulers. *)
