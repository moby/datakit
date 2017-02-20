open Datakit_github
open Astring
open !Asetmap

type t = [ `PR of PR.id | `Ref of Ref.id ]

let pp f = function
  | `PR (r, x)  -> Fmt.pf f "%a/prs/%d" Repo.pp r x
  | `Ref (r, x) -> Fmt.pf f "%a/%a" Repo.pp r Ref.pp_name x

let repo = function `PR (r, _) | `Ref (r, _) -> r

let id = function
  | `PR (_, x)  -> `PR x
  | `Ref (_, x) -> `Ref x

let compare a b = match a, b with
  | `PR a, `PR b   -> PR.compare_id a b
  | `Ref a, `Ref b -> Ref.compare_id a b
  | `PR _, _ -> 1
  | _ -> -1

module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
  end)

let parse s =
  let ( >>= ) x f =
    match x with
    | `Error _ as e -> e
    | `Ok x -> f x
  in
  let slash name s =
    match String.cut ~sep:"/" s with
    | None -> `Error (Fmt.strf "Missing %s/ at start of %S" name s)
    | Some (a, b) -> `Ok (a, b)
  in
  slash "user" s >>= fun (user, s) ->
  slash "project" s >>= fun (repo, s) ->
  let repo = Repo.v ~user ~repo in
  let parse_target = function
    | ("heads" | "tags") as ref_type, ref ->
      let open! Datakit_path.Infix in
      begin match Datakit_path.of_string ref with
        | Ok path -> `Ok (`Ref (repo, ref_type :: Datakit_path.unwrap path))
        | Error msg -> `Error msg
      end
    | "prs", id ->
      begin match String.to_int id with
        | Some id -> `Ok (`PR (repo, id))
        | None -> `Error (Fmt.strf "Invalid PR number %S" id)
      end
    | ty, _ -> `Error (Fmt.strf "Bad target type %S (should be heads/tags/prs)" ty)
  in
  slash "ref_type" s >>= fun (ref_type, ref) ->
  parse_target (ref_type, ref)

let arg = parse, pp

let map_of_list xs =
  let map = ref Repo.Map.empty in
  List.iter (fun target ->
      let p = repo target in
      let old_targets = Repo.Map.find p !map |> CI_utils.default Set.empty in
      map := !map |> Repo.Map.add p (Set.add target old_targets)
    ) xs;
  !map

let pr_path { Repo.user; repo} pr =
  Printf.sprintf "/%s/%s/pr/%d" user repo pr

let unescape_ref s =
  String.cuts ~sep:"/" s
  |> List.map Uri.pct_decode

let escape_ref path =
  List.map (fun x -> Uri.pct_encode ~scheme:"http" x) path
  |> String.concat ~sep:"/"

let ref_path { Repo.user; repo} r =
    Fmt.strf "/%s/%s/ref/%s" user repo (escape_ref r)

let path = function
  | `PR (repo, pr) -> pr_path repo pr
  | `Ref (repo, r) -> ref_path repo r

type v = [ `PR of PR.t | `Ref of Ref.t ]

let pp_v f = function
  | `PR pr  -> Fmt.pf f "prs/%a" PR.pp pr
  | `Ref x -> Fmt.pf f "refs/%a" Ref.pp x

let head = function
  | `PR x  -> PR.commit x
  | `Ref x -> Ref.commit x

let compare_v (x:v) (y:v) = Elt.compare (x :> Elt.t) (y :> Elt.t)

let path_v = function
  | `PR pr -> path (`PR (PR.id pr))
  | `Ref r -> path (`Ref (Ref.id r))

let repo_v = function
  | `PR pr -> PR.repo pr
  | `Ref r -> Ref.repo r

module Branch_escape = struct
  (* We need to escape all invalid characters, plus '_' (which is our escape character)
     and '-' (which is our field separator). *)
  let re_needs_replace = Str.regexp "[^a-zA-Z0-9.]"

  let escape s =
    let replace s =
      let c = (Str.matched_string s).[0] in
      Printf.sprintf "_%02x" (Char.to_int c)
    in
    Str.global_substitute re_needs_replace replace s

  let pp_repo f { Repo.user; repo } =
    Fmt.pf f "%s-%s" (escape user) (escape repo)

  let pp_v f v =
    match v with
    | `PR pr -> Fmt.pf f "%a-pr-%d" pp_repo (PR.repo pr) (PR.number pr)
    | `Ref r ->
      (* We special case the first component to avoid ugly escaping. *)
      match Ref.name r with
      | [] -> assert false
      | x :: xs ->
        let xs = String.concat ~sep:"/" xs in   (* '/' isn't valid in branch components. *)
        Fmt.pf f "%a-ref-%s-%s" pp_repo (Ref.repo r) (escape x) (escape xs)
end

let status_branch_v v = Fmt.strf "status-%a" Branch_escape.pp_v v
