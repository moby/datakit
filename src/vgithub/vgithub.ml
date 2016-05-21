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
  let of_list = List.fold_left (fun s e -> add e s) empty
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

  let pp_pr ppf pr =
    Fmt.pf ppf "@[%d %s@]" pr.pull_number pr.pull_head.branch_sha

  let create ~token ~user ~repo = { token = Lazy.force token; user; repo }

  let prs_diff pr1 pr2 =
    let pr1s = PRSet.of_list pr1 in
    let pr2s = PRSet.of_list pr2 in
    PRSet.diff pr1s pr2s

  let on_new_issues t f =
    let open Github.Monad in
    let listen s () =
      Logs.debug (fun l ->
          l "listening for issue events on %s/%s" t.user t.repo);
      let rec loop s old_prs =
        Log.debug (fun l -> l "%s/%s: poll ..." t.repo t.user);
        Log.debug (fun l -> l "old_prs=@[%a@]" Fmt.(list pp_pr) old_prs);
        Stream.poll s >>= fun so ->
        API.get_rate_remaining ~token:t.token () >>= fun remaining ->
        match so with
        | None   ->
          Logs.debug (fun l ->
              l "no new events on %s/%s (%d)" t.user t.repo remaining);
          loop s old_prs
        | Some s ->
          Logs.debug (fun l ->
              l "new events on %s/%s (%d)" t.user t.repo remaining);
          let new_prs = prs t in
          Log.debug (fun l -> l "new_prs=@[%a@]" Fmt.(list pp_pr) new_prs);
          PRSet.iter (fun pr ->
              Log.debug (fun l -> l "diff pr: %a" pp_pr pr);
              f pr
            ) (prs_diff new_prs old_prs);
          loop s new_prs
      in
      let prs = prs t in
      (* FIXME: quick hack to iterate on issue with no status. *)
      List.iter (fun pr ->
          match status t pr with
          | [] -> f pr
          | _  -> ()
        ) prs;
      Github.Monad.run @@ loop s prs
    in
    let init () =
      let events = Event.for_repo ~token:t.token ~user:t.user ~repo:t.repo () in
      Stream.next events >|= function
      | None        -> ()
      | Some (_, s) -> Lwt.async (listen s)
    in
    Github.Monad.run @@ init ()

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

let context_of_status s = match s.status_context with
  | None   -> ["default"]
  | Some c -> String.cuts ~empty:false ~sep:"/" c

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


(* /github.com/${USER}/${REPO}/updates *)
let pr_updates t =
  let open Lwt.Infix in
  Logs.debug (fun l -> l "pr_updates %s/%s" t.API.user t.API.repo);
  let session = Vfs.File.Stream.session None in
  let pp ppf = function
    | None    -> Fmt.string ppf ""
    | Some pr -> Fmt.pf ppf "%d\n" pr.pull_number
  in
  let stream () = Vfs.File.Stream.create pp session in
  Vfs.File.of_stream @@ fun () ->
  API.on_new_issues t (fun pr ->
      Vfs.File.Stream.publish session @@ Some pr
    ) >|= stream

(* /github.com/${USER}/${REPO}/pr *)
let pr_root t =
  Logs.debug (fun l -> l "pr_root %s/%s" t.API.user t.API.repo);
  let prs () =
    let prs = API.prs t in
    Vfs.Inode.file "updates" (pr_updates t) ::
    List.map (fun pr ->
        Vfs.Inode.dir (string_of_int pr.pull_number) @@ pr_dir t pr
      ) prs
  in
  Vfs.Dir.of_list prs

(* /github.com/${USER}/${REPO} *)
let repo_dir t =
  Logs.debug (fun l -> l "repo_root %s/%s" t.API.user t.API.repo);
  match API.repo t with
  | None   -> None
  | Some _ ->
    let files = [
      Vfs.Inode.dir "pr" @@ pr_root t
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
