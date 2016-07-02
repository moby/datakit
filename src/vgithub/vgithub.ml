open Result
open Github
open Github_t
open Astring

let src = Logs.Src.create "vgithub" ~doc:"Virtual Github API"
module Log = (val Logs.src_log src : Logs.LOG)

let run x = Lwt_main.run (Github.Monad.run x)

let err_invalid_status s = Vfs.error "%S: invalid status" s

module PRSet = struct
  module X = struct
      type t = pull
      let compare p1 p2 =
        match compare p1.pull_number p2.pull_number with
        | i when i <> 0 -> i
        | _  ->
          match compare p1.pull_state p2.pull_state with
          | i when i <> 0 -> i
          | _ ->
            match compare p1.pull_head.branch_sha p2.pull_head.branch_sha with
            | i when i <> 0 -> i
            | _ ->
              compare p1.pull_updated_at p2.pull_updated_at
  end
  include Set.Make(X)
end

module API = struct

  type t = {
    token: Github.Token.t;
    user : string;
    repo : string;
  }

  let string_of_status_state = function
    | `Error   -> "error"
    | `Failure -> "failure"
    | `Pending -> "pending"
    | `Success -> "success"

  let pp_status_state =  Fmt.of_to_string string_of_status_state

  let status_state_of_string = function
    | "error"   -> Some `Error
    | "failure" -> Some `Failure
    | "pending" -> Some `Pending
    | "success" -> Some `Success
    | _         -> None

  let _pp_status ppf s =
    Fmt.pf ppf "%a:%a" pp_status_state
      s.status_state Fmt.(option string) s.status_description

  let user t =
    try
      User.info ~token:t.token ~user:t.user ()
      |> run
      |> fun x -> Some x
    with Message _ ->
      None

  let repo t =
    try
      Repo.info ~token:t.token ~user:t.user ~repo:t.repo ()
      |> run
      |> fun x -> Some x
    with Message _ ->
      None

  let list_repos t =
    User.repositories ~token:t.token ~user:t.user ()
    |> Stream.to_list
    |> run
    |> List.map (fun r -> r.repository_name)

  let status { token; user; repo } pr =
    let git_ref = pr.pull_head.branch_sha in
    Status.for_ref ~token ~user ~repo ~git_ref ()
    |> Stream.to_list
    |> run

  let set_status { token; user; repo } ?url ?description ~context pr status =
    let status = {
      new_status_target_url = url;
      new_status_description = description;
      new_status_state = status;
      new_status_context = Some context;
    } in
    Status.create ~token ~user ~repo ~sha:pr.pull_head.branch_sha ~status ()
    |> run
    |> ignore

  let prs t =
    Pull.for_repo ~token:t.token ~state:`Open ~user:t.user ~repo:t.repo ()
    |> Stream.to_list
    |> run

  let create ~token ~user ~repo = { token = Lazy.force token; user; repo }

  let pp_event ppf e = Fmt.string ppf @@ Github_j.string_of_event e

  let events t =
    let events = Event.for_repo ~token:t.token ~user:t.user ~repo:t.repo () in
    Github.Monad.run @@ Stream.to_list events

end

(* /github.com/${USER}/${REPO}/pr/${PR}/status/${CONTEXT} *)
let pr_status_dir t pr context ?(extra_dirs=fun () -> []) state =
  Logs.debug (fun l ->
      l "status_file %s/%s %d %a"
        t.API.user t.API.repo pr.pull_number API.pp_status_state state);
  let update_session = Vfs.File.Stream.session `Pending in
  let current_descr = ref None in
  let current_url = ref None in
  let current_state = ref state in
  let init = API.string_of_status_state state ^ "\n" in
  let set_status () =
    let state = !current_state in
    let description = !current_descr in
    let url = !current_url in
    API.set_status t ~context ?description ?url pr state;
  in
  let state = Vfs.File.command ~init (fun str ->
      match API.status_state_of_string str with
      | None   -> err_invalid_status str
      | Some s ->
        if s = !current_state then Vfs.ok (str ^ "\n")
        else (
          current_state := s;
          set_status ();
          Vfs.File.Stream.publish update_session s;
          Vfs.ok (API.string_of_status_state s ^ "\n");
        )
    ) in
  let descr = Vfs.File.command ~init:"" (fun str ->
      if Some str = !current_descr then Vfs.ok (str ^ "\n")
      else (
        current_descr := Some str;
        set_status ();
        Vfs.ok (str ^ "\n")
      )
    ) in
  let url = Vfs.File.command ~init:"" (fun str ->
      if Some str = !current_url then Vfs.ok (str ^ "\n")
      else (
        current_url := Some str;
        set_status ();
        Vfs.ok (str ^ "\n")
      )
    ) in

  let updates =
    let stream () =
      Vfs.File.Stream.create API.pp_status_state update_session
      |> Lwt.return
    in
    Vfs.File.of_stream stream
  in
  let dir = [
    Vfs.Inode.file "state"   state;
    Vfs.Inode.file "descr"   descr;
    Vfs.Inode.file "url"     url;
    Vfs.Inode.file "updates" updates;
  ] in
  Vfs.Dir.of_list (fun () -> dir @ extra_dirs ())

let context_of_string_opt = function
  | None   -> ["default"]
  | Some c -> String.cuts ~empty:false ~sep:"/" c

let context_of_status s = context_of_string_opt s.status_context
let context_of_status_event s = context_of_string_opt s.status_event_context

let rec compare_context x y =
  match x, y with
  | [], [] -> 0
  | [], _  -> -1
  | _ , [] -> 1
  | h1::t1, h2::t2 ->
    match String.compare h1 h2 with
    | 0 -> compare_context t1 t2
    | i -> i

let sort_by_hd childs =
  let childs = List.filter (fun (p, _) -> p <> []) childs in
  let compare_child (c1, _) (c2, _) = compare_context c1 c2 in
  let childs = List.sort compare_child childs in
  let rec aux (root, current, acc) = function
    | [] -> List.rev @@ (root, List.rev current) :: acc
    | ([]  , _)::_ -> assert false
    | (r::p, s)::t ->
      if r = root then
        let current = (p, s) :: current in
        aux (root, current, acc) t
      else
        let acc = (root, List.rev current) :: acc in
        let current = [ (p, s) ] in
        let root = r in
        aux (root, current, acc) t
  in
  match childs with
  | []           -> []
  | ([],_):: _   -> assert false
  | (r::p, s)::t -> aux (r, [ (p, s) ], []) t

let string_of_context context = String.concat ~sep:"/" context

(* /github.com/${USER}/${REPO}/pr/${PR}/status *)
let pr_status_root t pr =
  Log.debug (fun l ->
      l "status_dir %s/%s %d" t.API.user t.API.repo pr.pull_number);
  let rec inodes childs =
    let root_status =
      try Some (List.find (fun (p, _) -> p = []) childs |> snd)
      with Not_found -> None
    in
    let childs = sort_by_hd childs in
    let childs () =
      List.map (fun (n, childs) -> Vfs.Inode.dir n @@ inodes childs) childs
    in
    match root_status with
    | None   -> Vfs.Dir.of_list childs
    | Some s ->
      let context_str = string_of_context @@ context_of_status s in
      pr_status_dir t pr context_str ~extra_dirs:childs s.status_state
  in
  let ls () =
    API.status t pr
    |> List.map (fun s -> context_of_status s, s)
    |> sort_by_hd
    |> List.map (fun (name, childs) -> Vfs.Inode.dir name @@ inodes childs)
    |> Vfs.ok
  in
  let lookup name =
    try
      API.status t pr
      |> List.map (fun s -> context_of_status s, s)
      |> List.find_all (fun (c, _) -> List.hd c = name)
      |> List.map (fun (c, s) -> List.tl c, s)
      |> inodes
      |> Vfs.Inode.dir name
      |> Vfs.ok
    with Not_found ->
      Vfs.File.err_no_entry
  in
  let mkdir name =
    API.set_status t ~context:name pr `Pending;
    Vfs.ok @@ Vfs.Inode.dir name @@ pr_status_dir t pr name `Pending
  in
  let mkfile _ _ = Vfs.error "TODO" in
  let remove _ = Vfs.error "TODO" in
  let rename _ _ = Vfs.error "TODO" in
  Vfs.Dir.create ~ls ~lookup ~mkfile ~mkdir ~remove ~rename

(* /github.com/${USER}/${REPO}/pr/${PR}/head *)
let pr_head t pr =
  Logs.debug (fun l ->
      l "pr_dir %s/%s %d" t.API.user t.API.repo pr.pull_number);
  (* FIXME: this should be a stream, and update the relevant status
     files. *)
  Vfs.File.ro_of_string (pr.pull_head.branch_sha ^ "\n")

(* /github.com/${USER}/${REPO}/pr/${PR} *)
let pr_dir t pr =
  Logs.debug (fun l ->
      l "pr_dir %s/%s %d" t.API.user t.API.repo pr.pull_number);
  let dirs () = [
    Vfs.Inode.dir "status" @@ pr_status_root t pr;
    Vfs.Inode.file "head"  @@ pr_head t pr;
  ] in
  Vfs.Dir.of_list dirs

(* /github.com/${USER}/${REPO}/pr *)
let pr_root t =
  Logs.debug (fun l -> l "pr_root %s/%s" t.API.user t.API.repo);
  let prs () =
    let prs = API.prs t in
    List.map (fun pr ->
        Vfs.Inode.dir (string_of_int pr.pull_number) @@ pr_dir t pr
      ) prs
  in
  Vfs.Dir.of_list prs

(* /github.com/${USER}/${REPO}/events *)
let repo_events t =
  let open Lwt.Infix in
  Logs.debug (fun l -> l "repo_events %s/%s" t.API.user t.API.repo);
  let f () =
    let buf = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer buf in
    API.events t >|= fun events ->
    List.iter (Fmt.pf ppf "%a\n" API.pp_event) events;
    Buffer.contents buf
  in
  Vfs.File.status ~length:0 f

module Hack = struct

  (* NOTE(samoht): the proper way to handle this is to have a datakit
     client working at the VFS level. We don't have that yet, so we
     hook directly at the 9p level. This breaks the VFS abstraction
     but as we just have one backend I guess that's fine... *)

  let ( >>*= ) x f =
    let open Lwt.Infix in
    x >>= function
    | Ok x -> f x
    | Error _ as e -> Lwt.return e

  type 'a state = (module Datakit_S.CLIENT with type t = 'a) * 'a

  type t = E: 'a state -> t

  let state = ref None

  let init (s: unit -> (t, string) result Lwt.t) = state := Some s

  (* /github.com/${USER}/${REPO}/sync *)
  let repo_sync t =
    Log.debug (fun l -> l "repo_ctl %s/%s" t.API.user t.API.repo);
    let open Lwt.Infix in
    let (/) = Datakit_path.(/) in
    let (/@) = Datakit_path.(/@) in
    let ok = Lwt.return (Ok ()) in
    let root = Datakit_path.empty / t.API.user / t.API.repo in
    Vfs.File.command (fun branch ->
        Logs.debug (fun l -> l "syncing GitHub with branch %s" branch);
        API.events t >>= fun events ->
        let init () = match !state with
          | None   -> Lwt.fail_with "Vgithub Hack not initialized!"
          | Some x -> x () >>= function
            | Ok x    -> Lwt.return x
            | Error s -> Lwt.fail_with ("Vgithub Hack failed: " ^ s)
        in
        init () >>= fun (E ((module DK), dk)) ->
        let update_pr tr pr =
          let dir =
            root / "prs" / string_of_int pr.pull_request_event_number
          in
          Log.debug (fun l -> l "update_pr %s" @@ Datakit_path.to_hum dir);

          let pr = pr.pull_request_event_pull_request in
          match pr.pull_state with
          | `Closed ->
            DK.Transaction.exists tr dir >>*= fun exists ->
            if exists then DK.Transaction.remove tr dir else ok
          | `Open   ->
            DK.Transaction.make_dirs tr dir >>*= fun () ->
            let data = Cstruct.of_string pr.pull_head.branch_sha in
            DK.Transaction.create_or_replace_file tr ~dir "head" data
        in
        let update_status tr s =
          let dir =
            root / "commits" / s.status_event_sha /@
            Datakit_path.of_steps_exn (context_of_status_event s)
          in
          Log.debug (fun l -> l "update_status %s" @@ Datakit_path.to_hum dir);
          DK.Transaction.make_dirs tr dir >>*= fun () ->
          let some = function None -> "" | Some s -> s in
          let state = function
            | `Error -> "error" | `Failure -> "failure"
            | `Pending  -> "pending" | `Success -> "success"
          in
          let kvs = [
            "description", some s.status_event_description;
            "state"      , state s.status_event_state;
            "target_url" , some s.status_event_target_url;
          ] in
          List.fold_left (fun acc (k, v) ->
              acc >>*= fun () ->
              let v = Cstruct.of_string v in
              DK.Transaction.create_or_replace_file tr ~dir k v
            ) ok kvs
        in
        let sync () =
          DK.branch dk branch >>*= fun branch ->
          DK.Branch.with_transaction branch (fun tr ->
              Lwt_list.fold_left_s (fun acc e ->
                  match acc with
                  | Error e -> Lwt.return (Error e)
                  | Ok ()   ->
                    match e.event_payload with
                    | `PullRequest pr -> update_pr tr pr
                    | `Status s       -> update_status tr s
                    | _               -> ok
                ) (Ok ()) (List.rev events)
              >>*= fun () ->
              let message = Fmt.strf "Syncing %s/%s" t.API.user t.API.repo in
              DK.Transaction.commit tr ~message >>= fun result ->
              DK.disconnect dk >|= fun () ->
              result
            ) in
        sync () >>= function
        | Ok ()   -> Vfs.ok ""
        | Error e -> Vfs.error "conflict: %s" @@ Fmt.to_to_string DK.pp_error e
      )

end

(* /github.com/${USER}/${REPO} *)
let repo_dir t =
  Logs.debug (fun l -> l "repo_root %s/%s" t.API.user t.API.repo);
  match API.repo t with
  | None   -> None
  | Some _ ->
    let files = [
      Vfs.Inode.file "events" @@ repo_events t;
      Vfs.Inode.dir  "pr"     @@ pr_root t;
      Vfs.Inode.file "sync"   @@ Hack.repo_sync t;
    ] in
    let dir = Vfs.Dir.of_list (fun () -> files) in
    Some (Vfs.Inode.dir t.API.repo dir)

(* /github.com/${USER}/ *)
let user_dir ~token ~user =
  let t = API.create ~token ~user ~repo:"" in
  Logs.debug (fun l -> l "user_root %s/%s" t.API.user t.API.repo);
  match API.user t with
  | None   -> Vfs.Dir.err_no_entry
  | Some _ ->
    let ls () =
      API.list_repos t
      |> List.rev_map (fun repo -> repo_dir { t with API.repo })
      |> List.fold_left (fun acc -> function
          | None   -> acc
          | Some x -> x :: acc)
        []
      |> Vfs.ok
    in
    let remove _ = Vfs.Dir.err_read_only in
    let lookup name = match repo_dir { t with API.repo = name } with
      | None   -> Vfs.Dir.err_no_entry
      | Some x -> Vfs.ok x
    in
    let dir = Vfs.Dir.read_only ~ls ~remove ~lookup in
    Vfs.ok (Vfs.Inode.dir user dir)

(* /github.com/ *)
let create token =
  let token = lazy (token ()) in
  let ls () = Vfs.ok [] in
  let remove () = Vfs.Dir.err_read_only in
  let lookup name = user_dir ~token ~user:name in
  Vfs.Inode.dir "github.com" @@ Vfs.Dir.read_only ~ls ~remove ~lookup
