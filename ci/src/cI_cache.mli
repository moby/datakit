(** A cache for values computed (slowly) by terms. *)

open CI_utils

module Path : sig
  val log  : Datakit_client.Path.t                    (* The job's log output *)
  val value: Datakit_client.Path.t   (* Store build results in this directory *)
end

module Make(B : CI_s.BUILDER) : sig
  type t
  val create: logs:CI_live_log.manager -> B.t -> t
  val lookup:
    t -> (unit -> DK.t Lwt.t) -> B.context -> B.Key.t ->
    B.value CI_s.status Lwt.t
  val find: t -> B.context -> B.Key.t -> B.value CI_term.t
end

val read_log : DK.t -> CI_output.saved -> string DK.result
(** [read_log dk log] is the contents of the saved log [log]. *)
