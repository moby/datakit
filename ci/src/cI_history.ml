open CI_utils.Infix
open Lwt.Infix
open Astring

module DK = CI_utils.DK

let metadata_commit_path = Datakit_path.of_string_exn "metadata-commit"

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
  begin DK.Tree.read_dir tree (Datakit_path.of_steps_exn ["job"]) >>= function
    | Ok items ->
      items |> Lwt_list.filter_map_p (fun job_name ->
          let path = Datakit_path.of_steps_exn ["job"; job_name; "output"] in
          DK.Tree.read_file tree path >|= function
          | Ok data -> Some (job_name, Saved_output.of_cstruct data)
          | Error `Does_not_exist -> None
          | Error x -> failwith (Fmt.to_to_string DK.pp_error x)
        )
    | Error `Does_not_exist -> Lwt.return []
    | Error x -> failwith (Fmt.to_to_string DK.pp_error x)
  end
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

let diff _id prev next =
  match prev, next with
  | Some prev, Some next when CI_output.equal prev next -> None
  | _, Some next -> Some (`Write next)
  | Some _, None -> Some `Delete
  | None, None -> assert false

let record t dk input jobs =
  Lwt_mutex.with_lock t.lock @@ fun () ->
  let state = t.commit |> CI_utils.default empty in
  let patch = String.Map.merge diff state.jobs jobs in
  if String.Map.is_empty patch then Lwt.return ()
  else (
    let open! Datakit_path.Infix in
    let messages = ref [] in
    let add_msg fmt =
      fmt |> Fmt.kstrf @@ fun msg ->
      CI_utils.Log.info (fun f -> f "Record: %s" msg);
      messages := msg :: !messages
    in
    DK.branch dk t.branch_name >>*= fun branch ->
    let metadata_commit = Cstruct.of_string (DK.Commit.id input) in
    DK.Branch.with_transaction branch (fun tr ->
        DK.Transaction.create_or_replace_file tr metadata_commit_path metadata_commit >>*= fun () ->
        String.Map.bindings patch |> Lwt_list.iter_s (function
            | (job, `Delete) ->
              let dir = Datakit_path.of_steps_exn ["job"; job] in
              add_msg "Remove old job %s" job;
              DK.Transaction.remove tr dir >>*= Lwt.return
            | (job, `Write output) ->
              let dir = Datakit_path.of_steps_exn ["job"; job] in
              add_msg "%s -> %s" job (CI_output.descr output);
              let json = CI_output.json_of output in
              let data = Saved_output.to_cstruct json in
              DK.Transaction.make_dirs tr dir >>*= fun () ->
              DK.Transaction.create_or_replace_file tr (dir / "output") data >>*= Lwt.return
          )
        >>= fun () ->
        DK.Transaction.parents tr >>*= fun parents ->
        begin match !messages with
          | [message] ->
            DK.Transaction.commit tr ~message
          | ms ->
            let message =
              Fmt.strf "%d updates@.@.%a"
                (List.length ms)
                Fmt.(vbox (list ~sep:(const cut ()) string)) ms
            in
            DK.Transaction.commit tr ~message
        end >>*= fun () ->
        let parents = List.map DK.Commit.id parents in
        Lwt.return (Ok parents)
      )
    >>*= fun parents ->
    let state = { jobs; parents } in
    t.commit <- Some state;
    Lwt.return ()
  )

let jobs c = c.jobs
let parents c = c.parents
let head t = t.commit
