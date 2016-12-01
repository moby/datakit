module CI : sig
  type t = string list
  val circle_ci : t
  val datakit_ci : string -> t
end
