open Asetmap
open Datakit_github

let refs_dir = Datakit_path.of_string_exn "ref"
let prs_dir = Datakit_path.of_string_exn "pr"
let commits_dir = Datakit_path.of_string_exn "commit"

let metadata_branch = "github-metadata"

open! Datakit_path.Infix

open CI_utils
open Result
open! Astring
open Lwt.Infix

type t = DK.t

type project_snapshot = {
  repo : Repo.t;
  root : DK.Tree.t;
}

let repo_root { Repo.user; repo } = Datakit_path.(empty / user / repo)

let read_file { repo; root } path =
  let path = repo_root repo /@ path in
  DK.Tree.read_file root path

let ensure_removed t path =
  DK.Transaction.remove t path >|= function
  | Ok () -> ()
  | Error (`Msg "No such file or directory") -> ()
  | Error (`Msg msg) -> failf "Error removing %a: %s" Datakit_path.pp path msg

let read_string snapshot path =
  read_file snapshot path >|= function
  | Ok x -> Ok (Cstruct.to_string x)
  | Error _ as e -> e

module Commit_state = struct
  type t = {
    snapshot : project_snapshot;
    path : Datakit_path.t; (* Relative to snapshot *)
  }

  let read t leaf fn =
    let path = t.path / leaf in
    read_file t.snapshot path >|= function
    | Ok data -> Some (fn (String.trim (Cstruct.to_string data)))
    | Error (`Msg "No such file or directory") -> None
    | Error (`Msg msg) -> failf "Reading %a: %s" Datakit_path.pp path msg

  let read_opt t leaf fn =
    let path = t.path / leaf in
    read_file t.snapshot path >|= function
    | Ok data -> fn (String.trim (Cstruct.to_string data))
    | Error (`Msg "No such file or directory") -> None
    | Error (`Msg msg) -> failf "Reading %a: %s" Datakit_path.pp path msg

  let status t = read_opt t "state" Status_state.of_string
  let descr t = read t "description" (fun x -> x)
  let target_url t = read t "target_url" Uri.of_string
end

module CI = struct
  type t = string list
  let circle_ci = ["ci"; "circleci"]
  let datakit_ci x = ["ci"; "datakit"; x]
end

module Commit = struct
  type t = {
    snapshot : project_snapshot;
    hash : string;
  }

  let repo t = t.snapshot.repo

  let hash t = t.hash

  let state ci t =
    let path =
      commits_dir / t.hash / "status" /@ Datakit_path.of_steps_exn ci
    in
    { Commit_state.snapshot = t.snapshot; path }

  let pp f t = Fmt.string f (String.with_range ~len:6 t.hash)

  let compare a b =
    compare a.hash b.hash
end

module PR = struct
  type t = {
    id : int;
    commit : Commit.t;
    title : string;
  }

  let id t = t.id
  let head t = t.commit
  let title t = t.title
  let repo t = Commit.repo t.commit

  let dump f t = Fmt.pf f "PR#%d (commit=%a;title=%s)" t.id Commit.pp t.commit t.title

  let compare a b =
    match compare a.id b.id with
    | 0 -> Commit.compare a.commit b.commit
    | r -> r

  module Index = Map.Make(struct
      type t = (Repo.t * int)
      let compare (a:t) (b:t) = Pervasives.compare a b
    end)
end

module Ref = struct
  type t = {
    name : string list;              (* Relative to refs_dir *)
    head : Commit.t;
  }

  let repo t = Commit.repo t.head
  let name t = t.name
  let head t = t.head
  let pp_name ppf t = Fmt.string ppf (String.concat ~sep:"/" t)
  let dump f t = Fmt.pf f "ref/%a (head=%a)" pp_name t.name Commit.pp t.head

  let compare a b =
    match Pervasives.compare a.name b.name with
    | 0 -> Commit.compare a.head b.head
    | r -> r

  module Index = Map.Make(struct
      type t = Repo.t * string list
      let compare = Pervasives.compare
    end)
end

let connect gh = gh

let set_state t ci ~status ~descr ?target_url ~message commit =
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.with_transaction metadata (fun t ->
      let snapshot = commit.Commit.snapshot in
      let dir =
        repo_root snapshot.repo
        /@ commits_dir / commit.Commit.hash / "status" /@ Datakit_path.of_steps_exn ci
      in
      DK.Transaction.make_dirs t dir >>*= fun () ->
      let update leaf data =
        DK.Transaction.create_or_replace_file t (dir / leaf) (Cstruct.of_string (data ^ "\n"))
        >>*= Lwt.return in
      update "state" (Fmt.to_to_string Status_state.pp status) >>= fun () ->
      update "description" descr >>= fun () ->
      begin match target_url with
        | None -> ensure_removed t (dir / "target_url")
        | Some url -> update "target_url" (Uri.to_string url)
      end >>= fun () ->
      Log.debug (fun f -> f "set_state: %s" message);
      DK.Transaction.commit t ~message
    ) >>*= Lwt.return

let pr snapshot id =
  let id_leaf = string_of_int id in
  read_string snapshot (prs_dir / id_leaf / "head") >>= function
  | Error _ -> Log.err (fun f -> f "Missing head for PR %d" id); Lwt.return None
  | Ok commit ->
    let commit = { Commit.hash = String.trim commit; snapshot } in
    read_string snapshot (prs_dir / id_leaf / "title") >|= fun title ->
    let title =
      match title with
      | Error (`Msg m) ->
        Log.err (fun f -> f "Bad title for PR %d: %s" id m);
        Fmt.strf "Bad title: %s" m
      | Ok t -> String.trim t in
    Some { PR.id; commit; title }

let read_opt_file snapshot path =
  read_file snapshot path >|= function
  | Ok data -> Some data
  | Error (`Msg "No such file or directory") -> None
  | Error e -> failf "Error reading %a: %a" Datakit_path.pp path DK.pp_error e

let read_opt_dir {repo; root} path =
  let path = repo_root repo /@ path in
  DK.Tree.read_dir root path >|= function
  | Ok items -> items
  | Error (`Msg "No such file or directory") -> []
  | Error e -> failf "Error reading %a: %a" Datakit_path.pp path DK.pp_error e

let prs snapshot =
  read_opt_dir snapshot prs_dir >>=
  Lwt_list.fold_left_s (fun acc id ->
      match String.to_int id with
      | None -> Log.warn (fun f -> f "Invalid PR ID %S" id); Lwt.return acc
      | Some id -> pr snapshot id >|= function
        | None -> acc
        | Some value -> PR.Index.add (snapshot.repo, id) value acc
    ) PR.Index.empty

let refs snapshot =
  let open! Datakit_path.Infix in
  let results = ref Ref.Index.empty in
  let rec scan ~context leaf =
    let context = context / leaf in
    read_opt_file snapshot (refs_dir /@ context / "head") >>= function
    | Some head ->
      let hash = String.trim (Cstruct.to_string head) in
      let head = { Commit.snapshot; hash } in
      let name = Datakit_path.unwrap context in
      results := Ref.Index.add (snapshot.repo, name) { Ref.head; name } !results;
      Lwt.return ()
    | None ->
      read_opt_dir snapshot (refs_dir /@ context) >>=
      Lwt_list.iter_s (scan ~context)
  in
  read_opt_dir snapshot refs_dir >>=
  Lwt_list.iter_s (scan ~context:Datakit_path.empty) >|= fun () ->
  !results

module Target = struct
  type t = [ `PR of PR.t | `Ref of Ref.t ]

  let dispatch p r = function
    | `PR x -> p x
    | `Ref x -> r x

  let head = dispatch PR.head Ref.head

  let dump f = function
    | `PR x -> PR.dump f x
    | `Ref x -> Ref.dump f x
end

module Snapshot = struct
  type t = {
    commit : DK.Commit.t;
    mutable projects:  (PR.t PR.Index.t * Ref.t Ref.Index.t) Lwt.t Repo.Map.t;
  }

  let repo t r =
    match Repo.Map.find r t.projects with
    | Some p -> p
    | None ->
      let p =
        let root = DK.Commit.tree t.commit in
        let p_snapshot = { repo = r; root } in
        prs p_snapshot >>= fun prs ->
        refs p_snapshot >>= fun refs ->
        Lwt.return (prs, refs)
      in
      t.projects <- Repo.Map.add r p t.projects;
      p

  let ( >|?= ) x f =
    match x with
    | None -> None
    | Some y -> Some (f y)

  let find id t =
    let r = CI_target.repo id in
    repo t r >|= fun (prs, refs) ->
    match id with
    | `PR pr -> PR.Index.find pr prs >|?= fun x -> `PR x
    | `Ref x ->
      match Ref.Index.find x refs with
      | Some x -> Some (`Ref x)
      | None   -> None
end

let snapshot t =
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.head metadata >|*= function
  | None -> failf "Metadata branch does not exist!"
  | Some commit -> { Snapshot.commit; projects = Repo.Map.empty }

let enable_monitoring t projects =
  DK.branch t metadata_branch >>*= fun metadata_branch ->
  DK.Branch.with_transaction metadata_branch (fun tr ->
      let changes = ref false in
      projects |> Lwt_list.iter_s (fun p ->
          let dir = repo_root p in
          let path = dir / ".monitor" in
          DK.Transaction.exists_file tr path >>*= function
          | true -> Lwt.return ()
          | false ->
            Log.info (fun f -> f "Adding monitor file for %a" Datakit_path.pp path);
            changes := true;
            DK.Transaction.make_dirs tr dir >>*= fun () ->
            DK.Transaction.create_file tr path (Cstruct.of_string "") >|*= fun () -> ()
        )
      >>= fun () ->
      if !changes then (
        DK.Transaction.commit tr ~message:"Add .monitor files"
      ) else (
        DK.Transaction.abort tr >|= fun () -> Ok ()
      )
    )
  >>*= fun () ->
  Lwt.return ()

let monitor t ?switch fn =
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.wait_for_head metadata ?switch (function
      | None -> ok `Again
      | Some commit ->
        let snapshot = { Snapshot.commit; projects = Repo.Map.empty } in
        fn snapshot >>= fun () -> ok `Again
    )
  >|*= function
  | `Abort -> `Abort
  | `Finish `Never -> assert false

let pr t ~repo id =
  DK.branch t metadata_branch >>*= fun metadata ->
  DK.Branch.head metadata >>*= function
  | None -> Lwt.return None
  | Some head ->
    let root = DK.Commit.tree head in
    pr { repo; root } id
