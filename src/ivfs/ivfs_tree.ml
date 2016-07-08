(* FIXME: upstream that module *)

open Astring
open Lwt.Infix

type step = string
type path = string list
type perm = [ `Normal | `Exec | `Link ]

module Path = Irmin.Path.String_list

module type STORE = Irmin.S
  with type key = string list
   and type value = string
   and type branch_id = string
   and module Key = Irmin.Path.String_list
   and type Private.Node.Val.step = string
   and type Private.Node.Val.Metadata.t = perm

module type S = sig
  type repo
  type store
  module File : sig
    type t
    type hash
    val of_data : repo -> Ivfs_blob.t -> t
    val hash : t -> hash Lwt.t
    val content : t -> Ivfs_blob.t Lwt.t
    val equal : t -> t -> bool
    val pp_hash : Format.formatter -> hash -> unit
    val size : t -> int64 Lwt.t
  end
  module Dir : sig
    type t
    type hash
    val empty : repo -> t
    val ty : t -> step -> [`File | `Directory | `None] Lwt.t
    val lookup : t -> step -> [`File of File.t * perm | `Directory of t | `None ] Lwt.t
    val lookup_path : t -> path -> [`File of File.t * perm | `Directory of t | `None ] Lwt.t
    val get : t -> path -> t option Lwt.t
    val map : t -> [`File of File.t * perm | `Directory of t] String.Map.t Lwt.t
    val ls : t -> ([`File | `Directory] * step) list Lwt.t
    val of_hash : repo -> hash -> t
    val hash : t -> hash Lwt.t
    val repo : t -> repo
    val with_child : t -> step -> [`File of File.t * perm | `Directory of t] -> t Lwt.t
    val without_child : t -> step -> t Lwt.t
  end
  val snapshot : store -> Dir.t Lwt.t
end

module Make (Store : STORE) = struct
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
      repo : Store.Repo.t;
      mutable value : value;
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

    let digest_blob b = Store.Private.Contents.Key.digest (Ivfs_blob.to_ro_cstruct b)

    let equal a b =
      match a.value, b.value with
      | Hash a, Hash b -> Store.Private.Contents.Key.equal a b
      | Blob a, Blob b -> a = b
      | Hash a, Blob b -> Store.Private.Contents.Key.equal a (digest_blob b)
      | Blob a, Hash b -> Store.Private.Contents.Key.equal b (digest_blob a)

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
  end

  module Dir = struct
    type hash = Store.Private.Node.Key.t

    type map = [`File of File.t * perm | `Directory of t] String.Map.t

    and value =
      | Hash of hash * map Lwt.t Lazy.t
      | Map_only of map

    and t = {
      repo : Store.Repo.t;
      mutable value : value;
    }

    let empty repo = {repo; value = Map_only String.Map.empty}

    let map dir =
      match dir.value with
      | Map_only m -> Lwt.return m
      | Hash (_, lazy m) -> m

    let rec of_hash repo hash =
      let map = lazy (
        let node_t = Store.Private.Repo.node_t repo in
        Store.Private.Node.read_exn node_t hash >|= fun node ->
        Store.Private.Node.Val.alist node |> List.fold_left (fun acc (name, item) ->
          acc |> String.Map.add name (
            match item with
            | `Contents (h, perm) -> `File (File.of_hash repo h, perm)
            | `Node h -> `Directory (of_hash repo h)
          )
        ) String.Map.empty
      ) in
      { repo; value = Hash (hash, map) }

    let ty dir step =
      map dir >|= fun m ->
      match String.Map.find step m with
      | None -> `None
      | Some (`File _) -> `File
      | Some (`Directory _) -> `Directory

    let lookup dir step =
      map dir >|= fun m ->
      match String.Map.find step m with
      | Some (`Directory _ | `File _ as r) -> r
      | None -> `None

    let rec lookup_path dir = function
      | [] -> Lwt.return (`Directory dir)
      | p::ps ->
          lookup dir p >>= function
          | `Directory d -> lookup_path d ps
          | `File _ as x when ps = [] -> Lwt.return x
          | `None | `File _ -> Lwt.return `None

    let get dir path =
      lookup_path dir path >|= function
      | `Directory d -> Some d
      | `None | `File _ -> None

    (* TODO: just return the full map, now that we load it anyway *)
    let ls dir =
      map dir >|= fun m ->
      String.Map.bindings m
      |> List.map (fun (name, value) ->
        match value with
        | `File _ -> `File, name
        | `Directory _ -> `Directory, name
      )

    let rec hash t =
      match t.value with
      | Hash (h, _) -> Lwt.return h
      | Map_only m ->
          String.Map.bindings m
          |> Lwt_list.fold_left_s (fun acc item ->
            match item with
            | name, `File (f, perm) -> File.hash f >|= fun h -> (name, `Contents (h, perm)) :: acc
            | name, `Directory d ->
                map d >>= fun map ->
                if String.Map.is_empty map then (
                  (* Ignore empty directories *)
                  Lwt.return acc
                ) else (
                  hash d >|= fun h ->
                  (name, `Node h) :: acc
                )
          ) []
          >|= Store.Private.Node.Val.create
          >>= Store.Private.Node.add (Store.Private.Repo.node_t t.repo)

(*
    let rec equal a b =
      match a.value, b.value with
      | Map_only a, Map_only b -> String.Map.equal eq_item a b
      | Hash (a, _), Hash (b, _) -> Store.Private.Node.Key.equal a b
      | Map_only m, Hash (a, _)
      | Hash (a, _), Map_only m ->
          let b = export m |> Store.Private.Node.Key.digest in
          Store.Private.Node.Key.equal a b
    and eq_item a b =
      match a, b with
      | `File a, `File b -> File.equal a b
      | `Directory a, `Directory b -> equal a b
      | `File _, `Directory _
      | `Directory _, `File _ -> false
*)

    let repo a = a.repo

    let with_child t step child =
      map t >|= fun m ->
      let m = String.Map.add step child m in
      { repo = t.repo; value = Map_only m }

    let without_child t step =
      map t >|= fun m ->
      let m = String.Map.remove step m in
      { repo = t.repo; value = Map_only m }
  end

  let snapshot store =
    let repo = Store.repo store in
    Store.Private.read_node store [] >|= function
    | None -> Dir.empty repo
    | Some hash -> Dir.of_hash repo hash

end
