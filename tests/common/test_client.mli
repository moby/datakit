module type S = sig
  include Datakit_client.S

  val run : (t -> unit Lwt.t) -> unit
end

module Make (DK : S) : sig
  val test_set : unit Alcotest.test_case list
end
