open Datakit_github
module Conv = Datakit_github_conv.Make(CI_utils.DK)

open CI_utils
open! Astring
open Lwt.Infix

module CI = struct
  type t = string list
  let circle_ci = ["ci"; "circleci"]
  let datakit_ci x = ["ci"; "datakit"; x]
end

module Snapshot = struct

  type t =DK.Tree.t
  let pr = Conv.pr
  let status = Conv.status
  let ref = Conv.ref

  let prs t repo =
    Conv.prs t ~repos:(Repo.Set.singleton repo) >|= fun prs ->
    let prs = PR.index prs in
    match Repo.Map.find repo prs with
    | Some i -> i
    | None   -> PR.Index.empty

  let refs t repo =
    Conv.refs t ~repos:(Repo.Set.singleton repo) >|= fun refs ->
    let refs = Ref.index refs in
    match Repo.Map.find repo refs with
    | Some i -> i
    | None   -> Ref.Index.empty

  let (>?=) x f = x >|= function None -> None | Some x -> Some (f x)

  let target t = function
    | `PR id  -> pr t id  >?= fun x -> `PR x
    | `Ref id -> ref t id >?= fun x -> `Ref x

end
