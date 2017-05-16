open Lwt.Infix
open Datakit_github

let src = Logs.Src.create "bridge-local-git.sync" ~doc:"Local Git bridge sync for Datakit"
module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (S : Irmin.S with type branch = string)
    (DK : Datakit_client.S)
= struct
  module Conv = Datakit_github_conv.Make(DK)

  let ( >>*= ) x f =
    x >>= function
    | Ok x -> f x
    | Error e -> Lwt.fail (Failure (Fmt.to_to_string DK.pp_error e))

  type t = {
    repos : Repo.Set.t;
    metadata_branch : DK.Branch.t;
    mutable known : Commit.t Ref.Index.t;
    cond : unit Lwt_condition.t;        (* Fires when [known] changes. *)
  }

  let on_change t repo_id irmin_repo branch =
    Log.debug (fun f -> f "Notification for %S" branch);
    S.Branch.find irmin_repo branch >|= fun head ->
    let old = t.known in
    let id = (repo_id, ["heads"; branch]) in
    let next =
      match head with
      | None      -> Ref.Index.remove id old
      | Some head ->
        Ref.Index.add
          id (Commit.v repo_id (Fmt.to_to_string S.Commit.pp head)) old
    in
    if t.known != next then (
      Log.debug (fun f -> f "Update for %S" branch);
      t.known <- next;
      Lwt_condition.broadcast t.cond ();
    )

  let watch t (name, repo) =
    let callback branch _diff = on_change t name repo branch in
    S.Branch.watch_all ~init:[] repo callback >>= fun (_w: S.watch) ->
    (* XXX: In theory, we should be able to pass [~init:[]] and have Irmin notify us
       of the initial state. However, Irmin's [watch_branches] is buggy. *)
    S.Repo.branches repo >>= Lwt_list.iter_s (fun b -> on_change t name repo b)

  let read_refs t tr =
    DK.Transaction.parents tr >>*= function
    | [] -> Lwt.return Ref.Set.empty
    | [p] -> DK.Commit.tree p >>*= Conv.refs ~repos:t.repos
    | _ -> assert false (* We never make merge transactions. *)

  let update_ref tr ~changelog ~new_state existing_ref =
    Log.debug (fun f -> f "Updating ref %a" Ref.pp existing_ref);
    let id = Ref.id existing_ref in
    match Ref.Index.find id !new_state with
    | None ->
      Log.info (fun f -> f "Branch %a no longer exists" Ref.pp existing_ref);
      Buffer.add_string changelog (Fmt.strf "Removing deleted branch %a@." Ref.pp existing_ref);
      Conv.remove_elt tr (`Ref (Ref.id existing_ref));
    | Some new_head ->
      new_state := Ref.Index.remove id !new_state;
      if Commit.equal new_head (Ref.commit existing_ref) then Lwt.return ()
      else (
        let r = Ref.v new_head (Ref.name existing_ref) in
        Log.debug (fun f -> f "Updating ref to %a" Ref.pp r);
        Buffer.add_string changelog (Fmt.strf "Updating existing branch to %a@." Ref.pp r);
        Conv.update_elt tr (`Ref r)
      )

  let add_ref tr ~changelog (id, commit) =
    Log.info (fun f -> f "Tracking new branch %a" Ref.pp_id id);
    let r = Ref.v commit (snd id) in
    Buffer.add_string changelog (Fmt.strf "Tracking new branch %a@." Ref.pp r);
    Conv.update_elt tr (`Ref r)

  let sync t new_state =
    Log.info (fun f -> f "Copy state to DataKit");
    DK.Branch.with_transaction t.metadata_branch (fun tr ->
        let changelog = Buffer.create 128 in
        read_refs t tr >>= fun old_refs ->
        let new_state = ref new_state in
        Lwt_list.iter_s (update_ref tr ~changelog ~new_state) (Ref.Set.elements old_refs) >>= fun () ->
        let new_state = !new_state in
        Lwt_list.iter_s (add_ref tr ~changelog) (Ref.Index.bindings new_state) >>= fun () ->
        match Buffer.contents changelog with
        | "" ->
          Log.info (fun f -> f "No updates needed");
          DK.Transaction.abort tr
        | message ->
          DK.Transaction.commit tr ~message
      )
    >>*= Lwt.return

  let run dk repos =
    DK.branch dk "github-metadata" >>*= fun metadata_branch ->
    let cond = Lwt_condition.create () in
    let monitored = List.map fst repos |> Repo.Set.of_list in
    let t = { repos = monitored; metadata_branch; known = Ref.Index.empty; cond } in
    Lwt_list.iter_p (watch t) repos >>= fun () ->
    let rec aux () =
      let next = Lwt_condition.wait t.cond in
      sync t t.known >>= fun () ->
      next >>= aux
    in
    aux ()
end
