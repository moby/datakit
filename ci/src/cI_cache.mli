(** A cache for values computed (slowly) by terms. *)

open CI_utils

module Path : sig
  val value : Datakit_path.t    (* Store build results in this directory *)
end

module Make(B : CI_s.BUILDER) : sig
  type t
  (** A [t] is a cache of values created by [B]. *)

  val create : logs:CI_live_log.manager -> B.t -> t
  (** [create ~logs b] is a fresh cache that maps keys of type [B.Key.t] to values of type [B.value]. *)

  val lookup : t -> (unit -> DK.t Lwt.t) -> rebuild:bool -> B.context -> B.Key.t -> B.value CI_s.lwt_status Lwt.t
  (** [lookup t conn ~rebuild ctx key] returns the cached value of [key], or uses [B.generate ctx key] to start
      the process of calculating the value if this is the first time [key] has been
      requested.
      If [rebuild] is [true] then any complete cached result is ignored (we
      mark the result branch as needing a rebuild and build again anyway). *)

  val term : t -> B.context -> B.Key.t -> B.value CI_term.t
  (** [term t key] evaluates to the result of looking up [key] in the cache (using [lookup]). *)
end

val read_log : DK.t -> CI_result.Step_log.saved -> string DK.or_error Lwt.t
(** [read_log dk log] is the contents of the saved log [log]. *)
