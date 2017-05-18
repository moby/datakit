open Result
open Astring
open Lwt.Infix
open Datakit_client

let src = Logs.Src.create "datakit.9p" ~doc:"DataKit client 9p bindings"
module Log = (val Logs.src_log src: Logs.LOG)

let () = Random.self_init ()

let rwx = [`Read; `Write; `Execute]
let rw = [`Read; `Write]
let rx = [`Read; `Execute]
let r = [`Read]

let rwxr_xr_x =
  Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx ()

let rw_r__r__ = Protocol_9p.Types.FileMode.make ~owner:rw ~group:r ~other:r ()

let symlink =
  Protocol_9p.Types.FileMode.make
    ~owner:rwx ~group:rx ~other:rx ~is_symlink:true ()

let ( / ) dir leaf = dir @ [leaf]
let ( /@ ) dir user_path = dir @ Path.unwrap user_path
let pp_path = Fmt.Dump.list String.dump

let rec last = function
  | [] -> None
  | [x] -> Some x
  | _::xs -> last xs

let rec rdecons = function
  | [] -> None
  | [x] -> Some ([], x)
  | x::xs ->
    match rdecons xs with
    | None -> None
    | Some (xs, leaf) -> Some (x::xs, leaf)

let ok x = Lwt.return (Ok x)

let ( >>*= ) x f =
  x >>= function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

let ( >|*= ) x f =
  x >|= function
  | Ok x -> Ok (f x)
  | Error _ as e -> e

let lines str =
  let rec aux = function
    | [] -> Log.err (fun f -> f "Missing final end-of-line in %S" str); []
    | [""] -> []
    | x :: xs -> x :: aux xs
  in
  aux (String.cuts ~sep:"\n" str)

let abort_if_off switch fn =
  match switch with
  | None -> fn ()
  | Some sw when Lwt_switch.is_on sw -> fn ()
  | Some _ -> ok `Abort

exception Err of string

module Make(P9p : Protocol_9p.Client.S) = struct

  module Infix = struct
    let (>>=) = (>>*=)
    let (>|=) = (>|*=)
  end

  type error = [
    | `Does_not_exist
    | `Already_exists
    | `Is_dir
    | `Not_symlink
    | `Not_dir
    | `Not_file
    | `Internal of string               (* A bug in this library or the server *)
    | `IO of Protocol_9p.Error.error
  ]

  let pp_error f : error -> unit = function
    | `Does_not_exist -> Fmt.string f "No such file or directory"
    | `Already_exists -> Fmt.string f "Already exists"
    | `Is_dir -> Fmt.string f "Is a directory"
    | `Not_symlink -> Fmt.string f "Not a symlink"
    | `Not_dir -> Fmt.string f "Not a directory"
    | `Not_file -> Fmt.string f "Not a file"
    | `Internal e -> Fmt.pf f "client-9p internal error: %s" e
    | `IO (`Msg e) -> Fmt.pf f "9p error: %s" e

  type +'a result = ('a, error) Result.result Lwt.t

  let bug fmt = Printf.ksprintf (fun str -> Lwt.return (Error (`Internal str))) fmt

  let wrap_9p = function
    | Ok _ as x -> x
    | Error (`Msg "No such file or directory") -> Error `Does_not_exist
    | Error (`Msg "Already exists") -> Error `Already_exists
    | Error (`Msg "Is a directory") -> Error `Is_dir
    | Error (`Msg "Can't walk from a file") -> Error `Not_dir
    | Error e -> Error (`IO e)

  module Line_reader : sig
    type t
    (* A buffering reader that splits a raw byte stream into lines.
       `Lwt_io` can do this too, but depends on Unix and uses exceptions. *)

    val create : (unit -> Cstruct.t result) -> t
    val read_line : t -> [`Line of string | `Eof] result
  end = struct
    type t = {
      read : unit -> Cstruct.t result;
      mutable buffer : string;
      mutable eof : bool;
    }

    let create read = { buffer = ""; read; eof = false }

    let rec read_line t =
      match String.cut ~sep:"\n" t.buffer with
      | None when t.eof && t.buffer = "" -> ok `Eof
      | None when t.eof ->
        let data = t.buffer in
        Log.warn (fun f -> f "End-of-file while expecting newline: %S" data);
        t.buffer <- "";
        ok (`Line data)
      | None ->
        let accept data =
          t.buffer <- t.buffer ^ Cstruct.to_string data;
          read_line t in
        t.read () >>*= fun data ->
        if Cstruct.len data = 0 then (
          t.read () >>*= fun data ->
          if Cstruct.len data = 0 then (
            t.eof <- true;
            read_line t
          ) else accept data
        ) else accept data
      | Some (line, rest) ->
        t.buffer <- rest;
        ok (`Line line)
  end

  let diff_of_lines lines =
    try
      List.fold_left (fun acc line ->
          let err e =
            let s = Fmt.strf "invalid diff line: %s %s" line e in
            raise (Err s)
          in
          match String.cut ~sep:" " line with
          | None            -> err "missing space"
          | Some (op, path) ->
            match Path.of_string path with
            | Error e -> err e
            | Ok path -> match op with
              | "+" -> (`Added path  ) :: acc
              | "-" -> (`Removed path) :: acc
              | "*" -> (`Updated path) :: acc
              | e   -> err e
        ) [] lines
      |> ok
    with Err e -> bug "%s" e

  module FS = struct
    (* Low-level wrappers for 9p. *)

    type t = {
      conn : P9p.t;
    }

    let with_file_full t path fn =
      P9p.with_fid t.conn (fun newfid ->
          P9p.walk_from_root t.conn newfid path >|= wrap_9p >>*=
          fn newfid >|= fun x -> Ok x
        )
      >|= function
      | Ok x         -> x                       (* Error or success from [fn] *)
      | Error _ as e -> wrap_9p e               (* Error from [with_fid] itself
                                                   or [walk_from_root]. *)

    let with_file t path fn =
      with_file_full t path (fun fid _resp -> fn fid)

    let create_dir t ~dir leaf =
      Log.debug (fun f -> f "create_dir %a" pp_path (dir / leaf));
      P9p.mkdir t.conn dir leaf rwxr_xr_x >|= wrap_9p

    let write_to_fid t fid ~offset data =
      let maximum_payload =
        Int32.to_int (min 0x100000l (P9p.LowLevel.maximum_write_payload t.conn))
      in
      let rec loop ~offset remaining =
        let len = Cstruct.len remaining in
        if len = 0 then ok ()
        else (
          let to_request = min len maximum_payload in
          P9p.LowLevel.write t.conn fid offset (Cstruct.sub remaining 0 to_request)
          >>*= fun { Protocol_9p.Response.Write.count } ->
          let count = Int32.to_int count in
          let remaining = Cstruct.shift remaining count in
          loop ~offset:Int64.(add offset (of_int count)) remaining
        ) in
      loop ~offset data

    let create_file ~executable t ~dir leaf data =
      Log.debug (fun f -> f "create_file %a" pp_path (dir / leaf));
      with_file t dir (fun fid ->
          let perm = if executable then rwxr_xr_x else rw_r__r__ in
          P9p.LowLevel.create t.conn fid leaf perm Protocol_9p.Types.OpenMode.write_only
          >|= wrap_9p
          >>*= fun _open ->
          write_to_fid t fid ~offset:0L data
          >|= wrap_9p
          >>*= fun _resp ->
          ok ()
        )

    let create_symlink t ~dir leaf target =
      Log.debug (fun f -> f "create_symlink %a -> %s" pp_path (dir / leaf) target);
      with_file t dir (fun fid ->
          P9p.LowLevel.create ~extension:target
            t.conn fid leaf symlink Protocol_9p.Types.OpenMode.write_only
          >|= wrap_9p
          >>*= fun _resp ->
          ok ()
        )

    let replace_file t path leaf data =
      Log.debug
        (fun f -> f "replace_file %a -> %S"
            pp_path (path / leaf) (Cstruct.to_string data));
      with_file t (path / leaf) (fun fid ->
          P9p.LowLevel.update ~length:0L t.conn fid
          >|= wrap_9p
          >>*= fun () ->
          P9p.LowLevel.openfid t.conn fid Protocol_9p.Types.OpenMode.write_only
          >|= wrap_9p
          >>*= fun _open ->
          write_to_fid t fid ~offset:0L data
          >|= wrap_9p
          >>*= fun _resp ->
          ok ()
        )

    let write_stream t path data =
      Log.debug
        (fun f -> f "write %S to %a" (Cstruct.to_string data) pp_path path);
      with_file t path (fun fid ->
          P9p.LowLevel.openfid t.conn fid Protocol_9p.Types.OpenMode.write_only
          >|= wrap_9p
          >>*= fun _open ->
          write_to_fid t fid ~offset:0L data
          >|= wrap_9p
          >>*= fun _resp ->
          ok ()
        )

    (* TODO: limited to 2 GB files *)
    let read_all t path =
      Log.debug (fun f -> f "read_all %s" (String.concat ~sep:"/" path));
      P9p.read t.conn path 0L Int32.max_int >|= wrap_9p >>*= fun data ->
      let data = Cstruct.concat data in
      ok data

    let remove t path =
      Log.debug (fun f -> f "remove %a" pp_path path);
      P9p.remove t.conn path >|= wrap_9p

    let truncate t path new_length =
      Log.debug (fun f -> f "truncate %a to %Ld" pp_path path new_length);
      with_file t path (fun fid ->
        P9p.LowLevel.update t.conn ~length:new_length fid
        >|= wrap_9p
      )

    let read_node_aux ~link ~file ~dir t path =
      let open Protocol_9p.Types in
      with_file_full t path (fun _fid { Protocol_9p.Response.Walk.wqids } ->
          (* Note: would be more efficient to use [_fid] here... *)
          match last wqids with
          | Some qid when List.mem Qid.Symlink qid.Qid.flags -> link t path
          | Some qid when not (List.mem Qid.Directory qid.Qid.flags) -> file t path
          | _ -> dir t path
        )

    let read_link_aux t path =
      read_all t path >>*= fun data ->
      ok (`Link (Cstruct.to_string data))

    let read_file_aux t path =
      read_all t path >>*= fun data ->
      ok (`File data)

    let read_dir_aux t path =
      P9p.readdir t.conn path >|= wrap_9p >>*= fun items ->
      let items =
        List.map (fun item -> item.Protocol_9p.Types.Stat.name) items
      in
      ok (`Dir items)

    let read_node =
      read_node_aux ~link:read_link_aux ~file:read_file_aux ~dir:read_dir_aux

    let read_link t path =
      let err _ _ = Lwt.return (Error `Not_symlink) in
      read_node_aux ~link:read_link_aux ~file:err ~dir:err t path
      >|*= fun (`Link l) -> l

    let read_file t path =
      let err _ _ = Lwt.return (Error `Not_file) in
      read_node_aux ~link:err ~file:read_file_aux ~dir:err t path
      >|*= fun (`File l) -> l

    let read_dir t path =
      let err _ _ = Lwt.return (Error `Not_dir) in
      read_node_aux ~link:err ~file:err ~dir:read_dir_aux t path
      >|*= fun (`Dir l) -> l

    let stat t path =
      P9p.stat t.conn path >|= wrap_9p >>= function
      | Error `Does_not_exist -> ok None
      | Error _ as e -> Lwt.return e
      | Ok info ->
      let open Protocol_9p.Types in
      let mode = info.Stat.mode in
      let kind =
        if mode.FileMode.is_directory then `Dir
        else if mode.FileMode.is_symlink then `Link
        else if List.mem `Execute mode.FileMode.owner then `Exec
        else `File in
      ok (Some {
        kind;
        size = info.Stat.length;
      })

    let exists t path =
      stat t path >|*= function
      | None -> false
      | Some _ -> true

    let exists_dir t path =
      stat t path >|*= function
      | Some { kind = `Dir; _ } -> true
      | _ -> false

    let exists_file t path =
      stat t path >|*= function
      | None | Some { kind = `Dir; _ } -> false
      | _ -> true

    let set_executable t path exec =
      Log.debug (fun f -> f "set_executable %a to %b" pp_path path exec);
      let mode = if exec then rwxr_xr_x else rw_r__r__ in
      with_file t path (fun fid -> P9p.LowLevel.update t.conn ~mode fid >|= wrap_9p)

    let random_subdir t parent =
      let rec aux = function
        | 0 -> bug "Failed to create temporary directory!"
        | n ->
          let leaf = Int64.to_string (Random.int64 Int64.max_int) in
          create_dir t ~dir:parent leaf >>= function
          | Ok () -> ok leaf
          | Error `Already_exists -> aux (n - 1)
          | Error _ as e -> Lwt.return e
      in
      aux 3

    (* Read lines from [path], calling [fn line] for each one.
       Continues as long as [fn] returns [`Again] and the switch is still on. *)
    let wait_for t ?switch path fn =
      with_file t path @@ fun fid ->
      P9p.LowLevel.openfid t.conn fid Protocol_9p.Types.OpenMode.read_only
      >|= wrap_9p
      >>*= fun _resp ->
      let stream_offset = ref 0L in
      let read () =
        P9p.LowLevel.read t.conn fid !stream_offset 4096l >|= wrap_9p >>*= fun resp ->
        let data = resp.Protocol_9p.Response.Read.data in
        let len = Cstruct.len data in
        stream_offset := Int64.add !stream_offset (Int64.of_int len);
        ok data in
      let stream = Line_reader.create read in
      let next () = Line_reader.read_line stream in
      let th = ref (next ()) in
      Lwt_switch.add_hook_or_exec switch
        (fun () -> Lwt.cancel !th; Lwt.return ()) >>= fun () ->
      let rec loop () =
        abort_if_off switch @@ fun () ->
        !th >>*= function
        | `Eof -> bug "End-of-file from monitor stream!"
        | `Line value ->
          abort_if_off switch @@ fun () ->
          fn (String.trim value) >>*= function
          | `Finish _ | `Abort as r -> ok r
          | `Again -> th := next (); loop ()
      in
      Lwt.catch loop
        (function
          | Lwt.Canceled as ex ->
            abort_if_off switch @@ fun () ->
            Lwt.fail ex
          | ex -> Lwt.fail ex
        )

    (* Ensure that [base @ path] exists (assuming that [base] already exists). *)
    let make_dirs t ~base path =
      let path = Path.unwrap path in
      let rec aux user_path =
        Log.debug (fun f -> f "make_dirs.aux(%a)" (Fmt.Dump.list String.dump) user_path);
        match rdecons user_path with
        | None -> ok ()
        | Some (dir, leaf) ->
          create_dir t ~dir:(base @ dir) leaf >>= function
          | Ok () | Error `Already_exists -> ok ()
          | Error `Does_not_exist ->
            (* Parent is missing too *)
            aux dir >>*= fun () ->
            create_dir t ~dir:(base @ dir) leaf >>= begin function
            | Ok () | Error `Already_exists -> ok ()
            | Error _ as e -> Lwt.return e
            end
          | Error _ as e -> Lwt.return e
      in
      aux path

    let create_or_replace t ~dir leaf value =
      let path = dir / leaf in
      exists t path >>*= function
      | true -> replace_file t dir leaf value
      | false -> create_file t ~executable:false ~dir:dir leaf value
  end

  module Tree = struct

    type value = [ `Dir of string list | `File of Cstruct.t | `Link of string ]
    type 'a cache = ('a, error) Result.result Path.Map.t ref

    type t = {
      fs   : FS.t;
      path : string list;
      reads: value cache;
      stats: stat option cache;
    }

    let find_cache c p =
      try Some (Path.Map.find p !c) with Not_found -> None

    let empty () = ref Path.Map.empty
    let add_cache c p v = c := Path.Map.add p v !c
    let v fs path = { fs; reads = empty () ; stats = empty (); path }
    let of_id fs id = v fs ["trees"; id]

    let read t path =
      match find_cache t.reads path with
      | Some x -> Lwt.return x
      | None   ->
        FS.read_node t.fs (t.path /@ path) >|= fun v ->
        add_cache t.reads path v;
        v

    let stat t path =
      match find_cache t.stats path with
      | Some x -> Lwt.return x
      | None   ->
        FS.stat t.fs (t.path /@ path) >|= fun v ->
        add_cache t.stats path v;
        v

    let exists t path =
      match find_cache t.reads path with
      | Some _ -> Lwt.return (Ok true)
      | None   ->
        stat t path >|= function
        | Ok None      -> Ok false
        | Ok (Some _)  -> Ok true
        | Error _ as e -> e

    let exists_dir t path =
      stat t path >|= function
      | Ok (Some { kind = `Dir; _ }) -> Ok true
      | Ok Some _    -> Ok false
      | Ok None      -> Ok false
      | Error _ as e -> e

    let exists_file t path =
      stat t path >|= function
      | Ok (Some { kind = `File; _ }) -> Ok true
      | Ok Some _    -> Ok false
      | Ok None      -> Ok false
      | Error _ as e -> e

    let read_file t path =
      read t path >|= function
      | Ok (`File f) -> Ok f
      | Error _ as e -> e
      | Ok _         -> Error `Not_file

    let read_dir t path =
      read t path >|= function
      | Ok (`Dir d)  -> Ok d
      | Error _ as e -> e
      | Ok _         -> Error `Not_dir

    let read_link t path =
      read t path >|= function
      | Ok (`Link l) -> Ok l
      | Error _ as e -> e
      | Ok _         -> Error `Not_symlink
  end

  module Commit = struct
    type t = { fs : FS.t; id : string }
    let path t = ["snapshots"; t.id]
    let tree t = Lwt.return (Ok (Tree.v t.fs (path t / "ro")))
    let message t = FS.read_all t.fs (path t / "msg") >|*= Cstruct.to_string
    let id t = t.id
    let pp ppf t = Fmt.string ppf t.id
    let compare x y = String.compare x.id y.id

    let parents t =
      FS.read_all t.fs (path t / "parents") >|*= fun data ->
      lines (Cstruct.to_string data)
      |> List.map (fun hash -> {t with id = hash})

    let diff t c =
      FS.read_all t.fs (path t / "diff" / id c) >>*= fun data ->
      let lines = lines (Cstruct.to_string data) in
      diff_of_lines lines
  end

  module Transaction = struct

    type t = {
      fs : FS.t;
      path : string list;
      mutable closed : bool;
    }

    let closed t = t.closed

    type merge_inputs = {
      ours : Tree.t;
      theirs : Tree.t;
      base : Tree.t;
    }

    let create fs branch_path =
      let dir = branch_path / "transactions" in
      FS.random_subdir fs dir >>*= fun leaf ->
      ok { fs; path = dir / leaf; closed = false }

    let rw_path t path =
      if t.closed then raise (Invalid_argument "Transaction is closed");
      t.path / "rw" /@ path

    let split_for_create path =
      match Path.pop path with
      | Some x -> x
      | None -> raise (Invalid_argument "Can't create '/'!")

    let create_file t path ?(executable=false) data =
      let (dir, leaf) = split_for_create path in
      FS.create_file t.fs ~executable ~dir:(rw_path t dir) leaf data

    let create_symlink t path target =
      let (dir, leaf) = split_for_create path in
      FS.create_symlink t.fs ~dir:(rw_path t dir) leaf target

    let make_dirs t path =
      FS.make_dirs t.fs ~base:(t.path / "rw") path

    let create_dir t path =
      let (dir, leaf) = split_for_create path in
      FS.create_dir t.fs ~dir:(rw_path t dir) leaf

    let set_parents t parents =
      if t.closed then raise (Invalid_argument "Transaction is closed");
      List.map (fun f -> Cstruct.of_string (f.Commit.id ^ "\n")) parents
      |> Cstruct.concat
      |> FS.replace_file t.fs t.path "parents"

    let replace_file t path data =
      let (dir, leaf) = split_for_create path in
      FS.replace_file t.fs (rw_path t dir) leaf data

    let remove t path =
      FS.remove t.fs (rw_path t path)

    let truncate t path new_length =
      FS.truncate t.fs (rw_path t path) new_length

    let set_executable t path =
      FS.set_executable t.fs (rw_path t path)

    let conflicts t =
      if t.closed then raise (Invalid_argument "Transaction is closed");
      FS.read_all t.fs (t.path / "conflicts") >>*= fun data ->
      let paths = lines (Cstruct.to_string data) in
      let rec aux = function
        | [] -> Ok []
        | x :: xs ->
          match Path.of_string x with
          | Error e -> Error (`Internal (Fmt.strf "Invalid path in conflicts: %s" e))
          | Ok path ->
            match aux xs with
            | Error _ as e -> e
            | Ok paths -> Ok (path :: paths)
      in
      Lwt.return (aux paths)

    let merge t commit =
      if t.closed then raise (Invalid_argument "Transaction is closed");
      FS.write_stream t.fs
        (t.path / "merge") (Cstruct.of_string commit.Commit.id) >>*= fun () ->
      conflicts t >>*= fun confl ->
      let ours = Tree.v t.fs (t.path / "ours") in
      let theirs = Tree.v t.fs (t.path / "theirs") in
      let base = Tree.v (t.fs) (t.path / "base") in
      ok ({ ours; theirs; base }, confl)

    let commit t ~message =
      if t.closed then raise (Invalid_argument "Transaction is closed");
      FS.write_stream t.fs (t.path / "msg") (Cstruct.of_string message)
      >>*= fun () ->
      FS.write_stream t.fs (t.path / "ctl") (Cstruct.of_string "commit")
      >|= function
      | Ok () -> t.closed <- true; Ok ()
      | Error _ as e -> e

    let abort t =
      if t.closed then Lwt.return (Ok ())
      else (
        FS.write_stream t.fs (t.path / "ctl") (Cstruct.of_string "close")
        >>= function
        | Error e ->
          Log.err
            (fun f -> f "Error aborting transaction %a: %a" pp_path t.path pp_error e);
          t.closed <- true; (* Give up *)
          Lwt.return (Ok ())
        | Ok () ->
          t.closed <- true;
          Lwt.return (Ok ())
      )

    let read t path = FS.read_node t.fs (t.path / "rw" /@ path)
    let stat t path = FS.stat t.fs (t.path / "rw" /@ path)
    let exists t path = FS.exists t.fs (t.path / "rw" /@ path)
    let exists_file t path = FS.exists_file t.fs (t.path / "rw" /@ path)
    let exists_dir t path = FS.exists_dir t.fs (t.path / "rw" /@ path)

    let create_or_replace_file t path content =
      let (dir, leaf) = split_for_create path in
      FS.create_or_replace t.fs ~dir:(t.path / "rw" /@ dir) leaf content

    let read_file t path = FS.read_file t.fs (t.path / "rw" /@ path)
    let read_dir t path = FS.read_dir t.fs (t.path / "rw" /@ path)
    let read_link t path = FS.read_link t.fs (t.path / "rw" /@ path)

    let parents t =
      FS.read_all t.fs (t.path / "parents") >|*= fun data ->
      lines (Cstruct.to_string data)
      |> List.map (fun hash -> {Commit.fs = t.fs; id = hash})

    let diff t c =
      FS.read_all t.fs (t.path / "diff" / Commit.id c) >>*= fun data ->
      let lines = lines (Cstruct.to_string data) in
      diff_of_lines lines

  end

  module Branch = struct
    type t = {
      fs : FS.t;
      mutable name : string;
    }

    let name t = t.name

    let branch_dir t = ["branch"; t.name]

    let create fs name =
      (* Note: DataKit returns success if the branch already exists too,
         so no need to handle errors here. *)
      FS.create_dir fs ~dir:["branch"] name >|*= fun () ->
      { fs; name }

    let remove t =
      FS.remove t.fs (branch_dir t)

    let node_of_hash t = function
      | "" -> ok None
      | line ->
        let file f =
          (* TODO: delay loading this? *)
          FS.read_file t.fs ["trees"; line] >>*= fun contents ->
          ok (Some (f contents)) in
        match String.cut ~sep:"-" line with
        | None -> bug "Invalid tree watch line!"
        | Some ("D", _) -> ok (Some (`Dir (Tree.of_id t.fs line)))
        | Some ("F", _) -> file (fun c -> `File c)
        | Some ("X", _) -> file (fun c -> `Exec c)
        | Some ("L", _) -> file (fun c -> `Link (Cstruct.to_string c))
        | Some (_, _) -> bug "Invalid tree kind code"

    let commit_of_hash t = function
      | "" -> None
      | id -> Some { Commit.fs = t.fs; id }

    let head t =
      FS.read_all t.fs (branch_dir t / "head") >|*= fun data ->
      commit_of_hash t (String.trim (Cstruct.to_string data))

    let wait_for_head t ?switch fn =
      FS.wait_for t.fs ?switch (branch_dir t / "head.live")
        (fun hash -> fn (commit_of_hash t hash))

    let wait_for_path t ?switch path fn =
      let path = Path.unwrap path in
      let path = List.map (fun x -> x ^ ".node") path in
      FS.wait_for t.fs ?switch (branch_dir t / "watch" @ (path / "tree.live"))
        (fun hash -> node_of_hash t hash >>*= fn)

    let fast_forward t commit =
      FS.write_stream t.fs
        (branch_dir t / "fast-forward") (Cstruct.of_string commit.Commit.id)

    let transaction t = Transaction.create t.fs (branch_dir t)

    let with_transaction t fn =
      transaction t >>*= fun tr ->
      Lwt.finalize
        (fun () ->
           fn tr >>*= fun result ->
           if tr.Transaction.closed then ok result
           else (
             Transaction.abort tr >|= fun _ ->
             (* Make sure the user doesn't think their transaction succeeded *)
             failwith "Transaction returned Ok without committing or aborting \
                       (so forced abort)";
           )
        )
        (fun () ->
           if tr.Transaction.closed then Lwt.return ()
           else (
             (* Just log, so we don't hide the underlying error *)
             Log.info (fun f -> f "Transaction finished without committing or \
                                   aborting (will abort)");
             Transaction.abort tr >|= function
             | Ok ()   -> ()
             | Error e ->
               Fmt.kstrf failwith "error while aborting the transaction: %a"
                 pp_error e
           )
        )

  end

  let branch t name =
    Branch.create t name

  let branches t =
    P9p.readdir t.FS.conn ["branch"] >|= wrap_9p >|*=
    List.map (fun info -> info.Protocol_9p.Types.Stat.name)

  let remove_branch t name =
    Branch.remove { Branch.fs = t; name }

  let fetch t ~url ~branch =
    FS.random_subdir t ["remotes"] >>*= fun id ->
    let path = ["remotes"; id] in
    Lwt.finalize
      (fun () ->
         FS.write_stream t (path / "url") (Cstruct.of_string url) >>*= fun () ->
         FS.write_stream t (path / "fetch") (Cstruct.of_string branch) >>*= fun () ->
         FS.read_all t (path / "head") >>*= fun commit_id ->
         let id = String.trim (Cstruct.to_string commit_id) in
         ok { Commit.fs = t; id })
      (fun () ->
         FS.remove t path >|= function
         | Error e ->
           Log.err (fun f -> f "Error removing remote %S: %a" id pp_error e)
         | Ok () -> ())

  let commit t id: Commit.t result =
    FS.read_all t (["commits"] / id) >>*= fun _json ->
    Lwt.return (Ok { Commit.fs = t; id })

  let tree t id = Lwt.return (Ok (Tree.of_id t id))
  let connect conn = { FS.conn }
  let disconnect t = P9p.disconnect t.FS.conn >|= fun () -> Ok ()
  type t = FS.t
end
