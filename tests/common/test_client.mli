module type S = sig
  include Datakit_S.CLIENT
  val run: (t -> unit Lwt.t) -> unit
end

module Make (DK: S): sig
  val test_set : Alcotest.test_case list
end
