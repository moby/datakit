open Astring
open Datakit_github

let src = Logs.Src.create "pont9.vfs" ~doc:"Github to VFS"
module Log = (val Logs.src_log src : Logs.LOG)

let err_invalid_status s = Vfs.error "%S: invalid status" s

module Make (API: API) = struct

  open Lwt.Infix

  type t = {
    token: API.token;
    user: string;
    repo: string;
  }

  (* /github.com/${USER}/${REPO}/commit/${SHA1}/status/${S} *)
  let commit_status_dir t ?(extra_dirs=fun () -> []) s =
    Logs.debug (fun l ->
        l "commit_status_file %s/%s %a" t.user t.repo Status.pp s
      );
    let current_descr = ref None in
    let current_url = ref None in
    let current_state = ref s.Status.state in
    let init = Status_state.to_string s.Status.state ^ "\n" in
    let set_status () =
      let state = !current_state in
      let description = !current_descr in
      let url = !current_url in
      let new_status = { s with Status.description; url; state } in
      API.set_status t.token new_status;
    in
    let state = Vfs.File.command ~init (fun str ->
        match Status_state.of_string str with
        | None   -> err_invalid_status str
        | Some s ->
          if s = !current_state then Vfs.ok (str ^ "\n")
          else (
            current_state := s;
            set_status () >>= fun () ->
            Vfs.ok (Status_state.to_string s ^ "\n");
          )
      ) in
    let descr = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_descr then Vfs.ok (str ^ "\n")
        else (
          current_descr := Some str;
          set_status () >>= fun () ->
          Vfs.ok (str ^ "\n")
        )
      ) in
    let url = Vfs.File.command ~init:"" (fun str ->
        if Some str = !current_url then Vfs.ok (str ^ "\n")
        else (
          current_url := Some str;
          set_status () >>= fun () ->
          Vfs.ok (str ^ "\n")
        )
      ) in
    let dir = [
      Vfs.Inode.file "state"  state;
      Vfs.Inode.file "descr"  descr;
      Vfs.Inode.file "url"    url;
    ] in
    Vfs.Dir.of_list (fun () -> Vfs.ok @@ dir @ extra_dirs ())

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

  (* /github.com/${USER}/${REPO}/commit/${SHA1}/status *)
  let commit_status_root t commit =
    Log.debug (fun l -> l "commit_status_root %s/%s %s" t.user t.repo commit);
    let status =
      ref @@ lazy (API.status t.token ~user:t.user ~repo:t.repo ~commit)
    in
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
      | None   -> Vfs.Dir.of_list (fun () -> Vfs.ok @@ childs ())
      | Some s -> commit_status_dir t ~extra_dirs:childs s
    in
    let ls () =
      Lazy.force !status >>= fun s -> s
      |> List.map (fun s -> Datakit_path.unwrap (Status.path s), s)
      |> sort_by_hd
      |> List.map (fun (name, childs) -> Vfs.Inode.dir name @@ inodes childs)
      |> Vfs.ok
    in
    let lookup name =
      Log.debug (fun l -> l "lookup %s" name);
      try
        Lazy.force !status >>= fun s -> s
        |> List.map (fun s -> Datakit_path.unwrap (Status.path s), s)
        |> sort_by_hd
        |> List.assoc name
        |> inodes
        |> Vfs.Inode.dir name
        |> Vfs.ok
      with Not_found ->
        Vfs.File.err_no_entry
    in
    let mkdir name =
      Log.debug (fun l -> l "mkdir %s" name);
      let new_status = {
        user = t.user; repo = t.repo; commit;
        Status.context = [name];
        url = None;
        description = None;
        state = `Pending;
      } in
      API.set_status t.token new_status >>= fun () ->
      status := lazy (API.status t.token ~user:t.user ~repo:t.repo ~commit);
      Vfs.ok @@ Vfs.Inode.dir name @@ commit_status_dir t new_status
    in
    let mkfile _ _ = Vfs.error "TODO" in
    let remove _ = Vfs.error "TODO" in
    let rename _ _ = Vfs.error "TODO" in
    Vfs.Dir.create ~ls ~lookup ~mkfile ~mkdir ~remove ~rename

  let commit_root t =
    Logs.debug (fun l -> l "commit_root %s%s" t.user t.repo);
    let ls () = Vfs.ok [] in
    let lookup commit =
      let status = Vfs.Inode.dir "status" @@ commit_status_root t commit in
      Vfs.Inode.dir commit @@ Vfs.Dir.of_list (fun () -> Vfs.ok [status])
      |> Vfs.ok
    in
    let mkdir commit = (* TODO *) lookup commit in
    let remove () = Vfs.error "Cannot remove commits" in
    let rename _ _ = Vfs.error "Cannot rename commits" in
    Vfs.Dir.dir_only ~ls ~lookup ~mkdir ~remove ~rename

  (* /github.com/${USER}/${REPO}/pr/${PR}/head *)
  let pr_head t pr =
    Logs.debug (fun l ->
        l "pr_dir %s/%s %d" t.user t.repo pr.PR.number);
    let file, _ = Vfs.File.rw_of_string (pr.PR.head ^ "\n") in
    file

  (* /github.com/${USER}/${REPO}/pr/${PR} *)
  let pr_dir t pr =
    Logs.debug (fun l ->
        l "pr_dir %s/%s %d" t.user t.repo pr.PR.number);
    let dirs () = Vfs.ok [
      Vfs.Inode.file "head"  @@ pr_head t pr;
    ] in
    Vfs.Dir.of_list dirs

  (* /github.com/${USER}/${REPO}/pr *)
  let pr_root t =
    Logs.debug (fun l -> l "pr_root %s/%s" t.user t.repo);
    let prs () =
      API.prs t.token ~user:t.user ~repo:t.repo >>= fun prs ->
      List.map (fun pr ->
          Vfs.Inode.dir (string_of_int pr.PR.number) @@ pr_dir t pr
        ) prs
      |> Vfs.ok
    in
    Vfs.Dir.of_list prs

  (* /github.com/${USER}/${REPO}/events *)
  let repo_events t =
    let open Lwt.Infix in
    Logs.debug (fun l -> l "repo_events %s/%s" t.user t.repo);
    let data () =
      let buf = Buffer.create 1024 in
      let ppf = Format.formatter_of_buffer buf in
      API.events t.token ~user:t.user ~repo:t.repo >|= fun events ->
      List.iter (Fmt.pf ppf "%a\n" Event.pp) events;
      Buffer.contents buf
    in
    let length () = Lwt.return 0 in
    Vfs.File.status ~length data

  (* /github.com/${USER}/${REPO} *)
  let repo_dir t =
    Logs.debug (fun l -> l "repo_root %s/%s" t.user t.repo);
    API.repo_exists t.token ~user:t.user ~repo:t.repo >|= fun repo_exists ->
    if not repo_exists then None
    else
      let files = Vfs.ok [
        Vfs.Inode.file "events" @@ repo_events t;
        Vfs.Inode.dir  "pr"     @@ pr_root t;
        Vfs.Inode.dir  "commit" @@ commit_root t;
      ] in
      let dir = Vfs.Dir.of_list (fun () -> files) in
      Some (Vfs.Inode.dir t.repo dir)

  (* /github.com/${USER}/ *)
  let user_dir ~token ~user =
    Logs.debug (fun l -> l "user_root %s/" user);
    API.user_exists token ~user >>= fun exists_user ->
    if not exists_user then Vfs.Dir.err_no_entry
    else
      let ls () =
        API.repos token ~user >>= fun r ->
        Lwt_list.rev_map_p (fun repo -> repo_dir { token; user; repo }) r
        >>= fun r ->
        List.fold_left (fun acc -> function
            | None   -> acc
            | Some x -> x :: acc)
          [] r
        |> Vfs.ok
      in
      let remove _ = Vfs.Dir.err_read_only in
      let lookup repo =
        repo_dir { token; user; repo } >>= function
        | None   -> Vfs.Dir.err_no_entry
        | Some x -> Vfs.ok x
      in
      let dir = Vfs.Dir.read_only ~ls ~remove ~lookup in
      Vfs.ok (Vfs.Inode.dir user dir)

  (* /github.com/ *)
  let create token =
    let ls () = Vfs.ok [] in
    let remove () = Vfs.Dir.err_read_only in
    let lookup name = user_dir ~token ~user:name in
    Vfs.Inode.dir "github.com" @@ Vfs.Dir.read_only ~ls ~remove ~lookup

end
