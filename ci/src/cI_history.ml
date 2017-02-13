open CI_utils.Infix
open Lwt.Infix
open Astring

module DK = CI_utils.DK

type commit = {
  parents : string list;
  jobs : string CI_output.t String.Map.t;
}

let empty = { parents = []; jobs = String.Map.empty }

type target = {
  branch_name : string;
  lock : Lwt_mutex.t;
  mutable commit : commit option;
}

module Saved_output = struct
  let to_cstruct t = Cstruct.of_string (Yojson.Basic.to_string t)

  let of_cstruct c =
    let json = Yojson.Basic.from_string (Cstruct.to_string c) in
    CI_output.of_json json
end

type t = {
  cache : (string, target) Hashtbl.t;   (* Branch name -> target *)
}

let create () =
  let cache = Hashtbl.create 100 in
  { cache }

let load commit =
  let tree = DK.Commit.tree commit in
  DK.Commit.parents commit >>*= fun parents ->
  let parents = List.map DK.Commit.id parents in
  DK.Tree.read_dir tree Datakit_path.empty >>*=
  Lwt_list.filter_map_p (fun job_name ->
      let path = Datakit_path.of_steps_exn [job_name; "output"] in
      DK.Tree.read_file tree path >|= function
      | Ok data -> Some (job_name, Saved_output.of_cstruct data)
      | Error `Does_not_exist -> None
      | Error x -> failwith (Fmt.to_to_string DK.pp_error x)
    )
  >>= fun jobs ->
  let jobs = String.Map.of_list jobs in
  Lwt.return { parents; jobs }

let lookup t dk target =
  let branch_name = CI_target.status_branch_v target in
  match Hashtbl.find t.cache branch_name with
  | t -> Lwt_mutex.with_lock t.lock (fun () -> Lwt.return t)    (* Ensures we've finished loading *)
  | exception Not_found ->
    let target = { lock = Lwt_mutex.create (); commit = None; branch_name } in
    Hashtbl.add t.cache branch_name target;
    (* No-one can interrupt us here, so we will certainly get the lock first. *)
    Lwt_mutex.with_lock target.lock (fun () ->
        DK.branch dk branch_name >>*= fun branch ->
        DK.Branch.head branch >>*= function
        | None -> Lwt.return ()
        | Some head ->
          load head >|= fun commit ->
          target.commit <- Some commit
      )
    >>= fun () ->
    Lwt.return target

let record t dk job input output =
  Lwt_mutex.with_lock t.lock @@ fun () ->
  let state = t.commit |> CI_utils.default empty in
  match String.Map.find job state.jobs with
  | Some prev when CI_output.equal prev output -> Lwt.return ()
  | _ ->
    let open! Datakit_path.Infix in
    DK.branch dk t.branch_name >>*= fun branch ->
    let dir = Datakit_path.of_steps_exn [job] in
    let metadata_commit = Cstruct.of_string (DK.Commit.id input) in
    let json = CI_output.json_of output in
    let data = Saved_output.to_cstruct json in
    let message = Fmt.strf "%s -> %s" job (CI_output.descr output) in
    DK.Branch.with_transaction branch (fun tr ->
        DK.Transaction.make_dirs tr dir >>*= fun () ->
        DK.Transaction.create_or_replace_file tr (dir / "metadata-commit") metadata_commit >>*= fun () ->
        DK.Transaction.create_or_replace_file tr (dir / "output") data >>*= fun () ->
        DK.Transaction.parents tr >>*= fun parents ->
        DK.Transaction.commit tr ~message >>*= fun  () ->
        let parents = List.map DK.Commit.id parents in
        Lwt.return (Ok parents)
      )
    >>*= fun parents ->
    let jobs = String.Map.add job output state.jobs in
    let state = { jobs; parents } in
    t.commit <- Some state;
    Lwt.return ()

let jobs c = c.jobs
let parents c = c.parents
let head t = t.commit
