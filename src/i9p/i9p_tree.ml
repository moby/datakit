(* FIXME: upstream that module *)

open Lwt.Infix

type leaf = string
type path = string list

module Path = Irmin.Path.String_list

module type STORE = Irmin.S
  with type key = string list
   and type value = string
   and type branch_id = string
   and module Key = Irmin.Path.String_list
   and type Private.Node.Val.step = string

module type S = sig
  type store
  module File : sig
    type t
    val content : t -> string Lwt.t
    val equal : t -> t -> bool
    val pp_hash : Format.formatter -> t -> unit
  end
  type repo
  module Dir : sig
    type t
    type hash
    val empty : repo -> t
    val ty : t -> leaf -> [`File | `Directory | `None] Lwt.t
    val node : t -> path -> [`File of File.t | `Directory of t | `None ] Lwt.t
    val get : t -> path -> t option Lwt.t
    val ls : t -> ([`File | `Directory] * leaf) list Lwt.t
    val equal : t -> t -> bool
    val of_hash : repo -> hash option -> t
    val hash : t -> hash option
    val repo : t -> repo
  end
  val snapshot : store -> Dir.t Lwt.t
end

module Make (Store : STORE) = struct
  type store = Store.t
  type repo = Store.Repo.t

  module Graph = Irmin.Private.Node.Graph(Store.Private.Contents)(Store.Private.Node)

  module File = struct
    type t = {
      repo : Store.Repo.t;
      hash : Store.Private.Node.Val.contents;
    }

    let equal a b =
      Store.Private.Contents.Key.equal a.hash b.hash

    let content f =
      let contents_t = Store.Private.Repo.contents_t f.repo in
      Store.Private.Contents.read_exn contents_t f.hash

    let pp_hash fmt f =
      Fmt.string fmt (Store.Private.Contents.Key.to_hum f.hash)
  end

  module Dir = struct
    type hash = Store.Private.Node.Key.t

    type t = {
      repo : Store.Repo.t;
      node : Store.Private.Node.key option;
    }

    let graph dir =
      (Store.Private.Repo.contents_t dir.repo, Store.Private.Repo.node_t dir.repo)

    let empty repo = {repo; node = None}

    let read_node dir =
      match dir.node with
      | None -> Lwt.return Store.Private.Node.Val.empty
      | Some key ->
        let node_t = Store.Private.Repo.node_t dir.repo in
        Store.Private.Node.read_exn node_t key

    let ty dir step =
      read_node dir >|= fun node ->
      match Store.Private.Node.Val.succ node step with
      | Some _ -> `Directory
      | None ->
        match Store.Private.Node.Val.contents node step with
        | Some _ -> `File
        | None -> `None

    let lookup dir step =
      read_node dir >|= fun node ->
      match Store.Private.Node.Val.succ node step with
      | Some node -> `Directory {dir with node = Some node}
      | None ->
        match Store.Private.Node.Val.contents node step with
        | Some hash -> `File {File.repo = dir.repo; hash}
        | None -> `None

    let get dir path =
      match dir.node with
      | None -> Lwt.return None
      | Some node ->
        Graph.read_node (graph dir) node path >|= function
        | None -> None
        | Some _ as node -> Some {dir with node}

    let node dir path =
      match dir.node with
      | None -> Lwt.return `None
      | Some node ->
        match Path.rdecons path with
        | None -> Lwt.return (`Directory dir)
        | Some (path, leaf) ->
          Graph.read_node (graph dir) node path >>= function
          | None -> Lwt.return `None
          | Some node -> lookup {dir with node = Some node} leaf

    let ls dir =
      read_node dir >|= fun node ->
      Store.Private.Node.Val.alist node |> List.map (function
          | leaf, `Contents _ -> `File, (leaf : string)
          | leaf, `Node _ -> `Directory, leaf
        )

    let equal a b =
      match a.node, b.node with
      | None, None -> true
      | Some a, Some b -> Store.Private.Node.Key.equal a b
      | _ -> false

    let hash a = a.node
    let repo a = a.repo
    let of_hash repo node = {repo; node}
  end

  let snapshot store =
    let repo = Store.repo store in
    Store.Private.read_node store [] >|= fun node ->
    {Dir.repo; node}

end
