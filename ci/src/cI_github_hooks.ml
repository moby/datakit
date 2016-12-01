open Datakit_github
module Conv = Datakit_github_conv.Make(CI_utils.DK)

let metadata_branch = "github-metadata"

open CI_utils
open Result
open! Astring
open Lwt.Infix

type t = DK.t

module CI = struct
  type t = string list
  let circle_ci = ["ci"; "circleci"]
  let datakit_ci x = ["ci"; "datakit"; x]
end

let connect gh = gh

let update_status t ~message s =
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.with_transaction metadata (fun t ->
      Conv.update_elt t (`Status s) >>= fun () ->
      Log.debug (fun f -> f "set_state: %s" message);
      DK.Transaction.commit t ~message
    ) >>*= Lwt.return

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

let snapshot t =
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.head metadata >|*= function
  | None   -> failf "Metadata branch does not exist!"
  | Some c -> DK.Commit.tree c

let enable_monitoring t repos =
  DK.branch t metadata_branch >>*= fun metadata_branch ->
  DK.Branch.with_transaction metadata_branch (fun t ->
      Lwt_list.iter_s (fun r -> Conv.update_elt t (`Repo r)) repos >>= fun () ->
      let commit () = DK.Transaction.commit t ~message:"Add .monitor files" in
      DK.Transaction.parents t >>*= function
      | []   -> commit ()
      | h::_ ->
        DK.Transaction.diff t h >>*= fun diff ->
        if diff <> [] then commit ()
        else DK.Transaction.abort t >|= fun () -> Ok ()
    ) >>*= Lwt.return

let monitor t ?switch fn =
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.wait_for_head metadata ?switch (function
      | None   -> ok `Again
      | Some c -> fn (DK.Commit.tree c) >>= fun () -> ok `Again
    )
  >|*= function
  | `Abort -> `Abort
  | `Finish `Never -> assert false
