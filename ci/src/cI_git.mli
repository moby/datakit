open CI_s

type t

val v : ?remote:string -> logs:CI_live_log.manager -> string -> t

module Commit : sig
  type t

  val pp : t Fmt.t

  val pp_short : t Fmt.t
end

type commit = Commit.t

val hash : commit -> string

val is_after : old:string -> commit -> bool Lwt.t

val fetch_head : t -> CI_target.t -> commit CI_term.t

val with_checkout :
  log:CI_live_log.t ->
  job_id:job_id ->
  commit ->
  (string -> 'a Lwt.t) ->
  'a Lwt.t

val with_clone :
  log:CI_live_log.t ->
  job_id:job_id ->
  commit ->
  (string -> 'a Lwt.t) ->
  'a Lwt.t

type command

val command :
  logs:CI_live_log.manager ->
  timeout:float ->
  label:string ->
  clone:bool ->
  string array list ->
  command

val run : command -> commit -> unit CI_term.t
