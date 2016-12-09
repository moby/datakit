module Make(C:CI_s.CONTEXT) : sig
  include CI_s.TERM with
    type context = C.t and
  type 'a key = C.t -> 'a

  val run : context -> 'a t -> ('a CI_result.t * CI_result.Step_log.t) Lwt.t
  (** [run context term] is the result of evaluating [term] in [context]. *)
end
