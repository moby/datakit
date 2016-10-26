module ID : sig
  type t = [ `PR of int | `Ref of Datakit_path.t ]
  val pp : t Fmt.t
  val compare : t -> t -> int
end

module ID_Set : Set.S with type elt = ID.t

module Full : sig
  type t
  val arg : t Cmdliner.Arg.converter
  val map_of_list : t list -> ID_Set.t CI_projectID.Map.t
end
