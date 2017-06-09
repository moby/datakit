open Datakit_github

let src = Logs.Src.create "bridge-github" ~doc:"Github to Git bridge"
module Log = (val Logs.src_log src : Logs.LOG)

module Make (API: API) = struct

  open Lwt.Infix

  type token = {
    t: API.token;
    c: Capabilities.t;
  }

  let token t c = { t; c }
  let ok x = Lwt.return (Ok x)
  let capabilities t = t.c
  let with_capabilities c t = { t with c }

  let status_of_commits token commits =
    let api_status token c =
      Log.info (fun l -> l "API.status %a" Commit.pp c);
      if not (Capabilities.check token.c `Read `Commit) then ok Status.Set.empty
      else
        API.status token.t c >|= function
        | Error e   -> Error (c, e)
        | Ok status ->
          let status =
            List.filter (fun s ->
                Capabilities.filter_elt token.c `Read (`Status s)
              ) status
          in
          Ok (Status.Set.of_list status)
    in
    Lwt_list.map_p (api_status token) (Commit.Set.elements commits)
    >|= fun status ->
    List.fold_left (fun status -> function
        | Ok s         -> Status.Set.union status s
        | Error (c, e) ->
          Log.err (fun l -> l "API.status %a: %s" Commit.pp c e);
          status
      ) Status.Set.empty status

  let new_prs token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.prs %a" Repo.pp r);
        if not (Capabilities.check token.c `Read `PR) then ok PR.Set.empty
        else
          API.prs token.t r >|= function
          | Error e -> Error (r, e)
          | Ok prs  ->
            List.filter (fun pr -> pr.PR.state = `Open) prs
            |> PR.Set.of_list
            |> fun x -> Ok x
      ) repos_l
    >|= fun new_prs ->
    List.fold_left (fun new_prs -> function
        | Ok prs       -> PR.Set.union prs new_prs
        | Error (r, e) ->
          Log.err (fun l -> l "API.prs %a: %s" Repo.pp r e);
          new_prs
      ) PR.Set.empty new_prs

  let new_refs token repos =
    let repos_l = Repo.Set.elements repos in
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.refs %a" Repo.pp r);
        if not (Capabilities.check token.c `Read `Ref) then ok Ref.Set.empty
        else
          API.refs token.t r >|= function
          | Error e -> Error (r, e)
          | Ok refs -> Ok (Ref.Set.of_list refs)
      ) repos_l
    >|= fun new_refs ->
    List.fold_left (fun new_refs -> function
        | Ok refs      -> Ref.Set.union refs new_refs
        | Error (r, e) ->
          Log.err (fun l -> l "API.refs %a: %s" Repo.pp r e);
          new_refs
      ) Ref.Set.empty new_refs

  let read_prs token ids =
    Lwt_list.map_p (fun pr ->
        Log.info (fun l -> l "API.pr %a" PR.pp_id pr);
        if not (Capabilities.check token.c `Read `PR) then ok None
        else
          API.pr token.t pr >|= function
          | Error e      -> Error (pr, e)
          | Ok None      -> Ok None
          | Ok (Some pr) ->
            if pr.PR.state = `Open then Ok (Some pr) else Ok None
      ) (PR.IdSet.elements ids)
    >|= fun new_prs ->
    List.fold_left (fun new_prs -> function
        | Ok (Some pr)  -> PR.Set.add pr new_prs
        | Ok None       -> PR.Set.empty
        | Error (pr, e) ->
          Log.err (fun l -> l "API.pr %a: %s" PR.pp_id pr e);
          new_prs
      ) PR.Set.empty new_prs

  let read_refs token ids =
    Lwt_list.map_p (fun r ->
        Log.info (fun l -> l "API.ref %a" Ref.pp_id r);
        if not (Capabilities.check token.c `Read `PR) then ok None
        else
          API.ref token.t r >|= function
          | Error e -> Error (r, e)
          | Ok r    -> Ok r
      ) (Ref.IdSet.elements ids)
    >|= fun new_refs ->
    List.fold_left (fun new_refs -> function
        | Ok (Some r)  -> Ref.Set.add r new_refs
        | Ok None      -> Ref.Set.empty
        | Error (r, e) ->
          Log.err (fun l -> l "API.ref %a: %s" Ref.pp_id r e);
          new_refs
      ) Ref.Set.empty new_refs

  (* Import http://github.com/usr/repo state. *)
  let import token t ids =
    let repos = Elt.IdSet.repos ids in
    new_prs token repos >>= fun new_prs ->
    new_refs token repos >>= fun new_refs ->
    let prs = Elt.IdSet.prs ids in
    let refs = Elt.IdSet.refs ids in
    read_prs token prs >>= fun prs ->
    let new_prs = PR.Set.union prs new_prs in
    read_refs token refs >>= fun refs ->
    let new_refs = Ref.Set.union refs new_refs in
    let new_commits =
      let (++) = Commit.Set.union in
      PR.Set.commits new_prs ++ Ref.Set.commits new_refs
    in
    status_of_commits token new_commits >|= fun new_status ->
    let new_t =
      Snapshot.v ~repos ~prs:new_prs ~refs:new_refs ~commits:new_commits
        ~status:new_status
    in
    Log.debug (fun l -> l "State.import %a@;@[<2>new:%a@]"
                  Repo.Set.pp repos Snapshot.pp new_t);
    let base = Snapshot.without_repos repos t in
    let repos = Repo.Set.union (Snapshot.repos t) repos in
    let prs = PR.Set.union (Snapshot.prs base) new_prs in
    let refs = Ref.Set.union (Snapshot.refs base) new_refs in
    let commits = Commit.Set.union (Snapshot.commits base) new_commits in
    let status = Status.Set.union (Snapshot.status base) new_status in
    Snapshot.v ~repos ~prs ~commits ~refs ~status

  let api_set_pr token pr =
    Log.info (fun l -> l "API.set-pr %a" PR.pp pr);
    if not (Capabilities.check token.c `Write `PR) then Lwt.return_unit
    else
      API.set_pr token.t pr >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-pr %a: %s" PR.pp pr e)

  let api_remove_ref token (repo, name as r) =
    Log.info (fun l -> l "API.remove-ref %a" Ref.pp_id r);
    if not (Capabilities.check token.c `Write `Ref) then Lwt.return_unit
    else
      API.remove_ref token.t repo name >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.remove-ref %a: %s" Ref.pp_id r e)

  let api_set_ref token r =
    Log.info (fun l -> l "API.set-ref %a" Ref.pp r);
    if not (Capabilities.check token.c `Write `Ref) then Lwt.return_unit
    else
      API.set_ref token.t r >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-ref %a: %s" Ref.pp r e)

  let api_set_status token s =
    Log.info (fun l -> l "API.set-status %a" Status.pp s);
    if not (Capabilities.check token.c `Write (`Status (Status.context s)))
    then Lwt.return_unit
    else
      API.set_status token.t s >|= function
      | Ok ()   -> ()
      | Error e -> Log.err (fun l -> l "API.set-status %a: %s" Status.pp s e)

  (* Read DataKit data and call the GitHub API to sync the world with
     what DataKit think it should be. *)
  let apply token diff =
    Log.debug (fun l -> l "State.apply@;@[%a@]" Diff.pp diff);
    let prs = Diff.update diff |> Elt.Set.prs in
    Lwt_list.iter_p (api_set_pr token) (PR.Set.elements prs)
    >>= fun () ->
    let closed_refs = Elt.IdSet.refs (Diff.remove diff) in
    Lwt_list.iter_p (api_remove_ref token) (Ref.IdSet.elements closed_refs)
    >>= fun () ->
    let refs = Elt.Set.refs (Diff.update diff) in
    Lwt_list.iter_p (api_set_ref token) (Ref.Set.elements refs)
    >>= fun () ->
    (* NOTE: ideally we would also remove status, but the GitHub API doesn't
       support removing status so we just ignore *)
    let status = Elt.Set.status (Diff.update diff) in
    Lwt_list.iter_p (api_set_status token) (Status.Set.elements status)

  let add_webhooks token ~watch repos =
    Log.debug (fun l -> l "[add_webhooks] repos: %a" Repo.Set.pp repos);
    Lwt_list.iter_p (fun r ->
        Log.info (fun l -> l "API.add-webhook %a" Repo.pp r);
        if not (Capabilities.check token.c `Write `Webhook) then Lwt.return_unit
        else watch r
      ) (Repo.Set.elements repos)

  let import_webhook_events token ~events t =
    events () >>= function
    | []     -> Lwt.return t
    | events ->
      Log.debug (fun l ->
          l "[sync_webhook] events:@;%a" (Fmt.Dump.list Event.pp) events);
      (* Need to resynchronsize build status for new commits *)
      let commits = List.fold_left (fun acc -> function
          | Event.PR pr ->
            if PR.state pr <> `Open then acc
            else Commit.Set.add (PR.commit pr) acc
          | Event.Ref (`Removed _) -> acc
          | Event.Ref (`Created r
                      |`Updated r) -> Commit.Set.add (Ref.commit r) acc
          | Event.Repo _  | Event.Status _  | Event.Other _  -> acc
        ) Commit.Set.empty events
      in
      let new_commits = Commit.Set.diff commits (Snapshot.commits t) in
      status_of_commits token new_commits >|= fun new_status ->
      let events =
        (List.map Event.of_status @@ Status.Set.elements new_status)
        @ events
      in
      Snapshot.with_events events t

end
