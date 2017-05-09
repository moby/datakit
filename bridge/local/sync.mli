module Make
    (S : Irmin.S with type branch = string)
    (DK : Datakit_client.S) : sig
  val run : DK.t -> (Datakit_github.Repo.t * S.Repo.t) list -> 'a Lwt.t
end
