open Datakit_github
open! Astring
open! Asetmap
open Datakit_client

type t = [ `PR of PR.id | `Ref of Ref.id ]

let pp f = function
  | `PR (r, x) -> Fmt.pf f "%a/prs/%d" Repo.pp r x
  | `Ref (r, x) -> Fmt.pf f "%a/%a" Repo.pp r Ref.pp_name x

let repo = function `PR (r, _) | `Ref (r, _) -> r

let id = function `PR (_, x) -> `PR x | `Ref (_, x) -> `Ref x

let compare a b =
  match (a, b) with
  | `PR a, `PR b -> PR.compare_id a b
  | `Ref a, `Ref b -> Ref.compare_id a b
  | `PR _, _ -> 1
  | _ -> -1

let equal a b = compare a b = 0

module Key = struct
  type nonrec t = t

  let compare = compare
end

module Set = Set.Make (Key)
module Map = Map.Make (Key)

let parse s =
  let ( >>= ) x f = match x with `Error _ as e -> e | `Ok x -> f x in
  let slash name s =
    match String.cut ~sep:"/" s with
    | None -> `Error (Fmt.strf "Missing %s/ at start of %S" name s)
    | Some (a, b) -> `Ok (a, b)
  in
  slash "user" s >>= fun (user, s) ->
  slash "project" s >>= fun (repo, s) ->
  let user = User.v user in
  let repo = Repo.v ~user ~repo in
  let parse_target = function
    | (("heads" | "tags") as ref_type), ref -> (
        match Path.of_string ref with
        | Ok path -> `Ok (`Ref (repo, ref_type :: Path.unwrap path))
        | Error msg -> `Error msg )
    | "prs", id -> (
        match String.to_int id with
        | Some id -> `Ok (`PR (repo, id))
        | None -> `Error (Fmt.strf "Invalid PR number %S" id) )
    | ty, _ ->
        `Error (Fmt.strf "Bad target type %S (should be heads/tags/prs)" ty)
  in
  slash "ref_type" s >>= fun (ref_type, ref) ->
  parse_target (ref_type, ref)

let arg = (parse, pp)

let map_of_list xs =
  let map = ref Repo.Map.empty in
  List.iter
    (fun target ->
      let p = repo target in
      let old_targets = Repo.Map.find p !map |> CI_utils.default Set.empty in
      map := !map |> Repo.Map.add p (Set.add target old_targets))
    xs;
  !map

let pr_path { Repo.user; repo } pr =
  Printf.sprintf "/%s/%s/pr/%d" (User.name user) repo pr

let unescape_ref s = String.cuts ~sep:"/" s |> List.map Uri.pct_decode

let escape_ref path =
  List.map (fun x -> Uri.pct_encode ~scheme:"http" x) path
  |> String.concat ~sep:"/"

let ref_path { Repo.user; repo } r =
  Fmt.strf "/%s/%s/ref/%s" (User.name user) repo (escape_ref r)

let path ?test target =
  let path =
    match target with
    | `PR (repo, pr) -> pr_path repo pr
    | `Ref (repo, r) -> ref_path repo r
  in
  let query =
    match test with None -> [] | Some test -> [ ("test", [ test ]) ]
  in
  Uri.make ~path ~query ()

type v = [ `PR of PR.t | `Ref of Ref.t ]

let pp_v f = function
  | `PR pr -> Fmt.pf f "prs/%a" PR.pp pr
  | `Ref x -> Fmt.pf f "refs/%a" Ref.pp x

let head = function `PR x -> PR.commit x | `Ref x -> Ref.commit x

let compare_v (x : v) (y : v) = Elt.compare (x :> Elt.t) (y :> Elt.t)

let path_v = function
  | `PR pr -> path (`PR (PR.id pr))
  | `Ref r -> path (`Ref (Ref.id r))

let repo_v = function `PR pr -> PR.repo pr | `Ref r -> Ref.repo r

module Branch_escape = struct
  (* We need to escape all invalid characters, plus '_' (which is our escape character)
     and '-' (which is our field separator). *)
  let re_needs_replace = Str.regexp "[^a-zA-Z0-9.]"

  let re_escape = Str.regexp "_.."

  let escape s =
    let replace s =
      let c = (Str.matched_string s).[0] in
      Printf.sprintf "_%02x" (Char.to_int c)
    in
    Str.global_substitute re_needs_replace replace s

  let unescape s =
    let replace s =
      let s = Str.matched_string s in
      let c = int_of_string ("0x" ^ String.with_range ~first:1 s) in
      String.of_char (Char.of_byte c)
    in
    Str.global_substitute re_escape replace s

  let pp_repo f { Repo.user; repo } =
    Fmt.pf f "%s-%s" (escape @@ User.name user) (escape repo)

  let parse_repo ~user ~repo =
    let user = User.v (unescape user) in
    Repo.v ~user ~repo:(unescape repo)

  let pp_sub f (v : t) =
    match v with
    | `PR (_, id) -> Fmt.pf f "pr-%d" id
    | `Ref (_, r) -> (
        (* We special case the first component to avoid ugly escaping. *)
        match r with
        | [] -> assert false
        | x :: xs ->
            let xs = String.concat ~sep:"/" xs in
            (* '/' isn't valid in branch components. *)
            Fmt.pf f "ref-%s-%s" (escape x) (escape xs) )

  let pp f t = Fmt.pf f "%a-%a" pp_repo (repo t) pp_sub t

  let parse_sub ~repo = function
    | [ "pr"; id ] -> Some (`PR (repo, int_of_string id))
    | [ "ref"; x; xs ] ->
        let x = unescape x in
        let xs = String.cuts ~sep:"/" (unescape xs) in
        Some (`Ref (repo, x :: xs))
    | _ -> None

  let parse = function
    | user :: repo :: sub ->
        let repo = parse_repo ~user ~repo in
        parse_sub ~repo sub
    | _ -> None

  let parse_sub ~repo x = parse_sub ~repo (String.cuts ~sep:"-" x)
end

let of_v = function `Ref r -> `Ref (Ref.id r) | `PR pr -> `PR (PR.id pr)

let status_branch t = Fmt.strf "status-%a" Branch_escape.pp t

let of_status_branch name =
  try
    match String.cuts ~sep:"-" name with
    | "status" :: rest -> (
        match Branch_escape.parse rest with
        | Some target -> target
        | None -> CI_utils.failf "Invalid target part" )
    | _ -> CI_utils.failf "Does not start 'status-'"
  with ex ->
    CI_utils.failf "Invalid status branch name %S: %a" name CI_utils.pp_exn ex
