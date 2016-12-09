open Datakit_github
open Astring
open !Asetmap

module ID = struct
  type t = [ `PR of int | `Ref of Datakit_path.t ]

  let pp f = function
    | `PR x -> Fmt.pf f "prs/%d" x
    | `Ref x -> Datakit_path.pp f x

  let compare a b =
    match a, b with
    | `PR a, `PR b -> compare a b
    | `Ref a, `Ref b -> Datakit_path.compare a b
    | _ -> compare a b
end

module ID_Set = Set.Make(ID)

module Full = struct
  type t = Repo.t * ID.t

  let repo = fst
  let id = snd

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
    let parse_target = function
      | ("heads" | "tags") as ref_type, ref ->
        let open! Datakit_path.Infix in
        let ref_type = Datakit_path.of_steps_exn [ref_type] in
        begin match Datakit_path.of_string ref with
          | Ok path -> `Ok (`Ref (ref_type /@ path))
          | Error msg -> `Error msg
        end
      | "prs", id ->
        begin match String.to_int id with
          | Some id -> `Ok (`PR id)
          | None -> `Error (Fmt.strf "Invalid PR number %S" id)
        end
      | ty, _ -> `Error (Fmt.strf "Bad target type %S (should be heads/tags/prs)" ty)
    in
    slash "user" s >>= fun (user, s) ->
    slash "project" s >>= fun (repo, s) ->
    let repo = Repo.v ~user ~repo in
    slash "ref_type" s >>= fun (ref_type, ref) ->
    parse_target (ref_type, ref) >>= fun target ->
    `Ok (repo, target)

  let pp f (project, target) =
    Fmt.pf f "%a/%a" Repo.pp project ID.pp target

  let arg = parse, pp

  let map_of_list xs =
    let map = ref Repo.Map.empty in
    List.iter (fun (p, target) ->
        let old_targets = Repo.Map.find p !map |> CI_utils.default ID_Set.empty in
        map := !map |> Repo.Map.add p (ID_Set.add target old_targets)
      ) xs;
    !map
end
