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
  type file
  type repo
  type dir
  val empty : repo -> dir
  val snapshot: store -> dir Lwt.t
  val ty: dir -> leaf -> [`File | `Directory | `None] Lwt.t
  val node: dir -> path -> [`File of file | `Directory of dir | `None ] Lwt.t
  val get_dir: dir -> path -> dir option Lwt.t
  val ls: dir -> ([`File | `Directory] * leaf) list Lwt.t
  val equal: dir -> dir -> bool
  val repo: dir -> repo
  type hash
  val of_dir_hash: repo -> hash option -> dir
  val hash: dir -> hash option
end

module Make (Store : STORE) = struct
  type store = Store.t
  type file = Store.Private.Node.Val.contents
  type repo = Store.Repo.t
  type hash = Store.Private.Node.Key.t

  module Graph = Irmin.Private.Node.Graph(Store.Private.Contents)(Store.Private.Node)

  type dir = {
    repo : Store.Repo.t;
    node : Store.Private.Node.key option;
  }

  let graph dir =
    (Store.Private.Repo.contents_t dir.repo, Store.Private.Repo.node_t dir.repo)

  (* Hack until https://github.com/mirage/irmin/pull/327 is available *)
  let string_of_leaf l =
    match Path.cons l [] |> Store.Private.Node.Path.decons with
    | None -> assert false
    | Some (l, _) -> l

  let empty repo = {repo; node = None}

  let read_node dir =
    match dir.node with
    | None -> Lwt.return Store.Private.Node.Val.empty
    | Some key ->
      let node_t = Store.Private.Repo.node_t dir.repo in
      Store.Private.Node.read_exn node_t key

  let ty dir leaf =
    read_node dir >|= fun node ->
    let step = string_of_leaf leaf in
    match Store.Private.Node.Val.succ node step with
    | Some _ -> `Directory
    | None ->
      match Store.Private.Node.Val.contents node step with
      | Some _ -> `File
      | None -> `None

  let lookup dir leaf =
    let step = string_of_leaf leaf in
    read_node dir >|= fun node ->
    match Store.Private.Node.Val.succ node step with
    | Some node -> `Directory {dir with node = Some node}
    | None ->
      match Store.Private.Node.Val.contents node step with
      | Some f -> `File f
      | None -> `None

  let get_dir dir path =
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

  let snapshot store =
    let repo = Store.repo store in
    Store.Private.read_node store [] >|= fun node ->
    {repo; node}

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
  let of_dir_hash repo node = {repo; node}
end
