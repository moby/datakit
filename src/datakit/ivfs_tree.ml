(* FIXME: upstream that module *)

open Astring
open Lwt.Infix

type perm = [ `Normal | `Exec | `Link ]

module Path: Irmin.Path.S with type step = string = struct

  type step = string

  module Step = struct
    include Tc.String
    let to_hum s = s
    let of_hum = function
      | "" -> invalid_arg "Empty step!"
      | s  -> s
  end

  include Tc.List(Step)
  let to_json t = to_json (List.rev t)
  let of_json j = List.rev (of_json j)

  let empty = []
  let is_empty l = (l = [])
  let cons s t = t @ [s]
  let rcons t s = s :: t

  let rdecons = function
    | []   -> None
    | h::t -> Some (t, h)

  let decons l =
    match List.rev l with
    | []   -> None
    | h::t -> Some (h, List.rev t)

  let map l f = List.map f l
  let create x = x
  let to_hum t = String.concat ~sep:"/" (List.rev t)

  (* XXX: slow *)
  let of_hum s =
    List.filter ((<>)"") (String.cuts s ~sep:"/")
    |> List.map Step.of_hum
    |> List.rev

end

type path = Path.t
type step = Path.Step.t

module PathMap = struct
  include Map.Make(Path)
  let of_list l = List.fold_left (fun m (k, v) -> add k v m) empty l
end
module PathSet = Set.Make(Path)

module StepMap = Asetmap.Map.Make(Path.Step)

module type STORE = Irmin.S
  with type key = path
   and type value = string
   and type branch_id = string
   and module Key = Path
   and type Private.Node.Val.step = string
   and type Private.Node.Val.Metadata.t = perm

module type S = sig
  type repo
  type store
  module File: sig
    type t
    type hash
    val of_data: repo -> Ivfs_blob.t -> t
    val hash: t -> hash Lwt.t
    val content: t -> Ivfs_blob.t Lwt.t
    val size: t -> int64 Lwt.t
    val pp: t Fmt.t
    val pp_hash: Format.formatter -> hash -> unit
    val compare_hash: hash -> hash -> int
  end
  module Dir: sig
    type t
    type hash
    val empty: repo -> t
    val ty: t -> step -> [`File | `Directory | `None] Lwt.t
    val lookup:
      t -> step -> [`File of File.t * perm | `Directory of t | `None ] Lwt.t
    val lookup_path:
      t -> path -> [`File of File.t * perm | `Directory of t | `None ] Lwt.t
    val get: t -> path -> t option Lwt.t
    val map: t -> [`File of File.t * perm | `Directory of t] StepMap.t Lwt.t
    val ls: t -> ([`File | `Directory] * step) list Lwt.t
    val iter: t -> (path -> File.t * perm  -> unit Lwt.t) -> unit Lwt.t
    val of_hash: repo -> hash -> t
    val hash: t -> hash Lwt.t
    val repo: t -> repo
    val with_child:
      t -> step -> [`File of File.t * perm | `Directory of t] -> t Lwt.t
    val without_child: t -> step -> t Lwt.t
    val diff: t -> t -> (path * (File.t * perm) Irmin.diff) list Lwt.t
  end
  val snapshot: store -> Dir.t Lwt.t
end

module Make (Store: STORE) = struct
  type store = Store.t
  type repo = Store.Repo.t

  module File = struct

    type hash = Store.Private.Contents.Key.t

    (* For now, a value is either in memory or on disk. In future, we
       may want to support both at once for caching. *)
    type value =
      | Blob of Ivfs_blob.t
      | Hash of Store.Private.Contents.key

    type t = {
      repo: Store.Repo.t;
      mutable value: value;
    }

    let of_data repo value =
      { repo; value = Blob value }

    let of_hash repo hash =
      { repo; value = Hash hash }

    let hash f =
      match f.value with
      | Hash h -> Lwt.return h
      | Blob b ->
        let contents_t = Store.Private.Repo.contents_t f.repo in
        let data = Ivfs_blob.to_string b in
        Store.Private.Contents.add contents_t data >|= fun hash ->
        f.value <- Hash hash;
        hash

    let compare_hash a b =
      Store.Private.Contents.Key.compare a b

    let content f =
      match f.value with
      | Blob b -> Lwt.return b
      | Hash h ->
        let contents_t = Store.Private.Repo.contents_t f.repo in
        Store.Private.Contents.read_exn contents_t h >|= fun data ->
        Ivfs_blob.of_string data

    let size f =
      (* TODO: provide a more efficient API in Irmin to get sizes *)
      content f >|= Ivfs_blob.len

    let pp_hash fmt hash =
      Fmt.string fmt (Store.Private.Contents.Key.to_hum hash)

    let pp ppf t = match t.value with
      | Blob b -> Fmt.pf ppf "blob:%a" Ivfs_blob.pp b
      | Hash h -> Fmt.pf ppf "hash:%a" pp_hash h

  end

  module Dir = struct
    type hash = Store.Private.Node.Key.t
    type map = [`File of File.t * perm | `Directory of t] StepMap.t

    and value =
      | Hash of hash * map Lwt.t Lazy.t
      | Map_only of map

    and t = {
      repo: Store.Repo.t;
      mutable value: value;
    }

    let empty repo = {repo; value = Map_only StepMap.empty}

    let map dir =
      match dir.value with
      | Map_only m -> Lwt.return m
      | Hash (_, lazy m) -> m

    let rec of_hash repo hash =
      let map = lazy (
        let node_t = Store.Private.Repo.node_t repo in
        Store.Private.Node.read_exn node_t hash >|= fun node ->
        Store.Private.Node.Val.alist node
        |> List.fold_left (fun acc (name, item) ->
            acc |> StepMap.add (Path.Step.of_hum name) (
              match item with
              | `Contents (h, perm) -> `File (File.of_hash repo h, perm)
              | `Node h -> `Directory (of_hash repo h)
            )
          ) StepMap.empty
      ) in
      { repo; value = Hash (hash, map) }

    let ty dir step =
      map dir >|= fun m ->
      match StepMap.find step m with
      | None -> `None
      | Some (`File _) -> `File
      | Some (`Directory _) -> `Directory

    let lookup dir step =
      map dir >|= fun m ->
      match StepMap.find step m with
      | Some (`Directory _ | `File _ as r) -> r
      | None -> `None

    let rec lookup_path dir path = match Path.decons path with
      | None         -> Lwt.return (`Directory dir)
      | Some (p, ps) ->
        lookup dir p >>= function
        | `Directory d -> lookup_path d ps
        | `File _ as x when Path.is_empty ps -> Lwt.return x
        | `None | `File _ -> Lwt.return `None

    let get dir path =
      lookup_path dir path >|= function
      | `Directory d -> Some d
      | `None | `File _ -> None

    (* TODO: just return the full map, now that we load it anyway *)
    let ls dir =
      map dir >|= fun m ->
      StepMap.bindings m
      |> List.map (fun (name, value) ->
          match value with
          | `File _ -> `File, name
          | `Directory _ -> `Directory, name
        )

    let rec hash t =
      match t.value with
      | Hash (h, _) -> Lwt.return h
      | Map_only m ->
        StepMap.bindings m
        |> Lwt_list.fold_left_s (fun acc item ->
            match item with
            | name, `File (f, perm) ->
              File.hash f >|= fun h ->
              (Path.Step.to_hum name, `Contents (h, perm)) :: acc
            | name, `Directory d ->
              map d >>= fun map ->
              if StepMap.is_empty map then (
                (* Ignore empty directories *)
                Lwt.return acc
              ) else (
                hash d >|= fun h ->
                (Path.Step.to_hum name, `Node h) :: acc
              )
          ) []
        >|= Store.Private.Node.Val.create
        >>= Store.Private.Node.add (Store.Private.Repo.node_t t.repo)

    let repo a = a.repo

    let with_child t step child =
      map t >|= fun m ->
      let m = StepMap.add step child m in
      { repo = t.repo; value = Map_only m }

    let without_child t step =
      map t >|= fun m ->
      let m = StepMap.remove step m in
      { repo = t.repo; value = Map_only m }

    module KV = struct
        type t = Path.t * (File.hash * perm)
        let compare (p1, (h1, perm1)) (p2, (h2, perm2)) =
          match Path.compare p1 p2 with
          | 0 ->
            begin match File.compare_hash h1 h2 with
              | 0 -> Pervasives.compare perm1 perm2
              | i -> i
            end
          | i -> i
    end
    module KVSet = Set.Make(KV)

    let iter t fn =
      let rec aux = function
        | []            -> Lwt.return_unit
        | (t, path)::tl ->
          map t >>= fun childs ->
          let childs = StepMap.bindings childs in
          Lwt_list.fold_left_s (fun acc (k, v) ->
              match v with
              | `Directory  t -> Lwt.return ((t, Path.rcons path k) :: acc)
              | `File f       -> fn (Path.rcons path k) f >|= fun () -> acc
            ) [] childs
          >>= fun dirs ->
          let todo = dirs @ tl in
          aux todo
      in
      aux [t, Path.empty]

    let diff x y =
      let set t =
        let acc = ref KVSet.empty in
        iter t (fun k (v, p) ->
            File.hash v >>= fun v ->
            acc := KVSet.add (k, (v, p)) !acc;
            Lwt.return_unit
          ) >>= fun () ->
        Lwt.return !acc
      in
      let find path map =
        let f, p = PathMap.find path map in
        File.of_hash x.repo f, p
      in
      (* FIXME very dumb and slow *)
      set x >>= fun sx ->
      set y >>= fun sy ->
      let added     = KVSet.diff sy sx in
      let removed   = KVSet.diff sx sy in
      let added_l   = KVSet.elements added in
      let removed_l = KVSet.elements removed in
      let added_m   = PathMap.of_list added_l in
      let removed_m = PathMap.of_list removed_l in
      let added_p   = PathSet.of_list (List.map fst added_l) in
      let removed_p = PathSet.of_list (List.map fst removed_l) in
      let updated_p = PathSet.inter added_p removed_p in
      let added_p   = PathSet.diff added_p updated_p in
      let removed_p = PathSet.diff removed_p updated_p in
      let added =
        PathSet.fold (fun path acc ->
            (path, `Added (find path added_m)) :: acc
          ) added_p []
      in
      let removed =
        PathSet.fold (fun path acc ->
            (path, `Removed (find path removed_m)) :: acc
          ) removed_p []
      in
      let updated =
        PathSet.fold (fun path acc ->
            let x = find path removed_m in
            let y = find path added_m in
            (path, `Updated (x, y)) :: acc
          ) updated_p []
      in
      Lwt.return (added @ updated @ removed)

  end

  let snapshot store =
    let repo = Store.repo store in
    Store.Private.read_node store Path.empty >|= function
    | None -> Dir.empty repo
    | Some hash -> Dir.of_hash repo hash

end
