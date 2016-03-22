open Github
open Github_t
open Astring

let run x = Lwt_main.run (Github.Monad.run x)

let err_invalid_status s = Vfs.error "%S: invalid status" s

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

  let pp_status ppf s =
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

  let status { token; user; repo } git_ref =
    Status.for_ref ~token ~user ~repo ~git_ref ()
    |> Stream.to_list
    |> run

  let set_status { token; user; repo } ?description ~context pr status =
    let status = {
      new_status_target_url = None;
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
    |> List.map (fun pr -> pr, status t pr.pull_head.branch_sha)

  let _pp_pr ppf (pr, status) =
    Fmt.pf ppf "%d %s @[%a@]@."
      pr.pull_number pr.pull_head.branch_sha Fmt.(list pp_status) status

  let create ~token ~user ~repo = { token = Lazy.force token; user; repo }

end

let status_file t pr context status =
  let current_status = ref status in
  let init = API.string_of_status_state status ^ "\n" in
  Vfs.File.command ~init (fun str ->
      match API.status_state_of_string str with
      | None   -> err_invalid_status str
      | Some s ->
        if s = !current_status then Vfs.ok (str ^ "\n")
        else (
          current_status := s;
          API.set_status t ~context pr s;
          Vfs.ok (API.string_of_status_state s ^ "\n");
        )
    )

let status_files t pr status =
  List.fold_left (fun acc status ->
      let path = match status.status_context with
        | None   -> ["default"]
        | Some c -> String.cuts ~sep:"/" c
      in
      let context = String.concat ~sep:"/" path in
      if List.mem_assoc path acc then acc
      else
        let rec aux = function
          | []     -> assert false
          | [name] ->
            let file = status_file t pr context status.status_state in
            Vfs.Inode.file name file
          | name :: rest ->
            Vfs.Inode.dir name @@ Vfs.Dir.of_list (fun () -> [aux rest])
        in
        (path, aux path) :: acc
    ) [] status
  |> List.map snd

let pr_root t =
  let prs () =
    API.prs t
    |> List.map (fun (pr, status) ->
        Vfs.Inode.dir (string_of_int pr.pull_number) @@
        Vfs.Dir.of_list (fun () -> status_files t pr status)
      )
  in
  Vfs.Dir.of_list prs

(* repo sub-directory. Can just see the pr sub-folder. *)
let repo_root t =
  match API.repo t with
  | None   -> None
  | Some _ ->
    let files () = [
      Vfs.Inode.dir "pr" @@ pr_root t
    ] in
    let dir = Vfs.Dir.of_list files in
    Some (Vfs.Inode.dir t.API.repo dir)

(* user sub-directory, can see all the user's repos. *)
let user_root ~token ~user =
  let t = API.create ~token ~user ~repo:"" in
  match API.user t with
  | None   -> Vfs.Dir.err_enoent
  | Some _ ->
    let ls () =
      API.list_repos t
      |> List.rev_map (fun repo -> repo_root { t with API.repo })
      |> List.fold_left (fun acc -> function
          | None   -> acc
          | Some x -> x :: acc)
        []
      |> Vfs.ok
    in
    let remove _ = Vfs.Dir.err_read_only in
    let lookup name = match repo_root { t with API.repo = name } with
      | None   -> Vfs.Dir.err_enoent
      | Some x -> Vfs.ok x
    in
    let dir = Vfs.Dir.read_only ~ls ~remove ~lookup in
    Vfs.ok (Vfs.Inode.dir user dir)

(* top-level directory, can read all the users sub-directories but
   [ls] returns nothing. *)
let create token =
  let token = lazy (token ()) in
  let ls () = Vfs.ok [] in
  let remove () = Vfs.Dir.err_read_only in
  let lookup name = user_root ~token ~user:name in
  Vfs.Inode.dir "github.com" @@ Vfs.Dir.read_only ~ls ~remove ~lookup
