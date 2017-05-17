type stat = {
  kind: [`File | `Dir | `Link | `Exec];
  size: int64;
}

type status_state =
  [ `Pending
  | `Success
  | `Error
  | `Failure ]

type 'a diff = [ `Added of 'a | `Removed of 'a | `Updated of 'a ]
type value = [`File of Cstruct.t | `Dir of string list | `Link of string]

module Path = struct

  open Result

  type t = string list

  let empty = []

  let validate = function
    | "" | "." | ".." as x -> Error (Fmt.strf "Invalid path component %S" x)
    | x when String.contains x '/' -> Error (Fmt.strf "'/' in path step %S" x)
    | _ -> Ok ()

  let of_steps steps =
    let rec aux = function
      | [] -> Ok steps
      | x :: xs ->
        match validate x with
        | Ok () -> aux xs
        | Error _ as e -> e in
    aux steps

  let of_string path =
    of_steps (Astring.String.cuts ~sep:"/" path)

  let of_string_exn path =
    match of_string path with
    | Ok x -> x
    | Error msg -> raise (Invalid_argument msg)

  let pp = Fmt.(list ~sep:(const string "/") string)

  let of_steps_exn steps =
    match of_steps steps with
    | Ok x -> x
    | Error msg ->
      raise (Invalid_argument (Fmt.strf "Bad path %a: %s" pp steps msg))

  let unwrap x = x

  let to_hum = Fmt.to_to_string pp

  let compare = compare

  let dirname t = match List.rev t with
    | []   -> []
    | _::t -> List.rev t

  let basename t = match List.rev t with
    | []   -> None
    | h::_ -> Some h

  let pop = function
    | [] -> None
    | x::xs ->
      let rec aux dir this = function
        | [] -> Some (List.rev dir, this)
        | x::xs -> aux (this :: dir) x xs
      in
      aux [] x xs

  module Set = Set.Make(struct type t = string list let compare = compare end)
  module Map = Map.Make(struct type t = string list let compare = compare end)

  module Infix = struct

    let ( / ) path s =
      match validate s with
      | Ok () -> path @ [s]
      | Error msg -> raise (Invalid_argument msg)

    let ( /@ ) = ( @ )

  end

end

module type READABLE_TREE = sig
  type t
  type +'a result
  val read: t -> Path.t -> value result
  val stat: t -> Path.t -> stat option result
  val exists: t -> Path.t -> bool result
  val exists_file: t -> Path.t -> bool result
  val exists_dir: t -> Path.t -> bool result
  val read_file: t -> Path.t -> Cstruct.t result
  val read_dir: t -> Path.t -> string list result
  val read_link: t -> Path.t -> string result
end

module type S = sig
  type t
  type error = private
    [>`Already_exists
    | `Does_not_exist
    | `Is_dir
    | `Not_dir
    | `Not_file
    | `Not_symlink]

  val pp_error: error Fmt.t
  type +'a result = ('a, error) Result.result Lwt.t
  module Infix: sig
    val (>>=): 'a result -> ('a -> 'b result) -> 'b result
    val (>|=): 'a result -> ('a -> 'b) -> 'b result
  end
  module Tree: READABLE_TREE with type 'a result := 'a result
  module Commit: sig
    type t
    val pp: t Fmt.t
    val compare: t -> t -> int
    val id: t -> string
    val tree: t -> Tree.t result
    val message: t -> string result
    val parents: t -> t list result
    val diff: t -> t -> Path.t diff list result
  end

  module Transaction: sig
    include READABLE_TREE with type 'a result := 'a result
    val create_dir: t -> Path.t -> unit result
    val create_file: t -> Path.t -> ?executable:bool ->
      Cstruct.t -> unit result
    val create_symlink: t -> Path.t -> string -> unit result
    val replace_file: t -> Path.t -> Cstruct.t -> unit result
    val create_or_replace_file: t -> Path.t -> Cstruct.t -> unit result
    val set_executable: t -> Path.t -> bool -> unit result
    val remove: t -> Path.t -> unit result
    val truncate: t -> Path.t -> int64 -> unit result
    val make_dirs: t -> Path.t -> unit result
    val commit: t -> message:string -> unit result
    val abort: t -> unit result
    type merge_inputs = {
      ours: Tree.t;
      theirs: Tree.t;
      base: Tree.t;
    }
    val merge: t -> Commit.t -> (merge_inputs * Path.t list) result
    val parents: t -> Commit.t list result
    val set_parents: t -> Commit.t list -> unit result
    val conflicts: t -> Path.t list result
    val diff: t -> Commit.t -> Path.t diff list result
    val closed: t -> bool
  end

  module Branch: sig
    type t
    val name: t -> string
    val remove: t -> unit result
    val head: t -> Commit.t option result
    val wait_for_head: t -> ?switch:Lwt_switch.t ->
      (Commit.t option -> [`Finish of 'a | `Again | `Abort] result) ->
      [`Abort | `Finish of 'a] result
    val wait_for_path: t -> ?switch:Lwt_switch.t -> Path.t ->
      ([`File of Cstruct.t | `Dir of Tree.t
       | `Link of string | `Exec of Cstruct.t] option ->
       [`Finish of 'a | `Again | `Abort] result) ->
      [`Abort | `Finish of 'a] result
    val fast_forward: t -> Commit.t -> unit result
    val with_transaction: t -> (Transaction.t -> 'a result) -> 'a result
    val transaction: t -> Transaction.t result
  end
  val branches: t -> string list result
  val remove_branch: t -> string -> unit result
  val branch: t -> string -> Branch.t result
  val commit: t -> string -> Commit.t result
  val tree: t -> string -> Tree.t result
  val fetch: t -> url:string -> branch:string -> Commit.t result
  val disconnect: t -> unit result
end
