type t

val create : logs:CI_live_log.manager -> pool:CI_monitored_pool.t -> timeout:float -> label:string -> string -> t

module Image : sig
  type t
  val id : t -> string
  val pp : t Fmt.t
end

val build : t -> ?from:Image.t -> CI_git.commit -> Image.t CI_term.t
