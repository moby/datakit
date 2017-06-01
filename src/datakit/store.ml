module type S0 = Irmin.S
  with type metadata = Metadata.t
   and type Commit.Hash.t = Irmin.Hash.SHA1.t
   and type Tree.Hash.t = Irmin.Hash.SHA1.t
   and type Contents.Hash.t = Irmin.Hash.SHA1.t

module type S = S0
  with type contents = Blob.t
   and type key = Path.t
   and type step = Path.step
   and type branch = Branch.t

module type GIT_S_MAKER =
  functor (C: Irmin.Contents.S) ->
  functor (P: Irmin.Path.S) ->
  functor (B: Irmin.Branch.S) ->
    S0 with type contents = C.t
        and type key = P.t
        and type step = P.step
        and module Key = P
        and type branch = B.t

module Make_git (M: GIT_S_MAKER) = M(Blob)(Path)(Branch)
