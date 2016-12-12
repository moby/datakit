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

type v = [ `PR of PR.t | `Ref of Ref.t ]

let head = function
  | `PR x  -> PR.commit x
  | `Ref x -> Ref.commit x
