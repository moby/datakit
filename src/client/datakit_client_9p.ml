open Result
open Astring
open Lwt.Infix

let src = Logs.Src.create "datakit.client" ~doc:"DataKit client bindings"
module Log = (val Logs.src_log src: Logs.LOG)

let () = Random.self_init ()

let rwx = [`Read; `Write; `Execute]
let rw = [`Read; `Write]
let rx = [`Read; `Execute]
let r = [`Read]
let rwxr_xr_x = Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx ()
let rw_r__r__ = Protocol_9p.Types.FileMode.make ~owner:rw ~group:r ~other:r ()
let symlink = Protocol_9p.Types.FileMode.make ~owner:rwx ~group:rx ~other:rx ~is_symlink:true ()

let ( / ) dir leaf = dir @ [leaf]
let ( /@ ) dir user_path = dir @ Datakit_path.unwrap user_path
let pp_path = Fmt.Dump.list String.dump

let github_path = ["github.com"]

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

module Make(P9p : Protocol_9p_client.S) = struct

  type error = Protocol_9p_error.error
  let pp_error ppf (`Msg e) = Fmt.string ppf e
  type 'a or_error = ('a, error) result
  let error fmt = Printf.ksprintf (fun str -> Lwt.return (Error (`Msg str))) fmt

  module Line_reader : sig
    type t
    (* A buffering reader that splits a raw byte stream into lines.
       `Lwt_io` can do this too, but depends on Unix and uses exceptions. *)

    val create : (unit -> Cstruct.t or_error Lwt.t) -> t
    val read_line : t -> [`Line of string | `Eof] or_error Lwt.t
  end = struct
    type t = {
      read : unit -> Cstruct.t or_error Lwt.t;
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

  module FS = struct
    (* Low-level wrappers for 9p. *)

    type t = {
      conn : P9p.t;
    }

    let with_file_full t path fn =
      P9p.with_fid t.conn (fun newfid ->
          P9p.walk_from_root t.conn newfid path >>*= fn newfid
        )

    let with_file t path fn =
      with_file_full t path (fun fid _resp -> fn fid)

    let create_dir t ~dir leaf =
      Log.debug (fun f -> f "create_dir %a" pp_path (dir / leaf));
      P9p.mkdir t.conn dir leaf rwxr_xr_x

    let write_to_fid t fid ~offset data =
      (* TODO: see https://github.com/mirage/ocaml-9p/pull/80 *)
      let maximum_payload = 8192 in
      let rec loop ~offset remaining =
        let len = Cstruct.len remaining in
        if len = 0 then ok ()
        else (
          let to_request = min len maximum_payload in
          P9p.LowLevel.write t.conn fid offset (Cstruct.sub remaining 0 to_request)
          >>*= fun { Protocol_9p_response.Write.count } ->
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
          >>*= fun _open ->
          write_to_fid t fid ~offset:0L data
          >>*= fun _resp ->
          ok ()
        )

    let create_symlink t ~dir leaf target =
      Log.debug (fun f -> f "create_symlink %a -> %s" pp_path (dir / leaf) target);
      with_file t dir (fun fid ->
          P9p.LowLevel.create ~extension:target
            t.conn fid leaf symlink Protocol_9p.Types.OpenMode.write_only
          >>*= fun _resp ->
          ok ()
        )

    let replace_file t path leaf data =
      Log.debug
        (fun f -> f "replace_file %a -> %S"
            pp_path (path / leaf) (Cstruct.to_string data));
      with_file t (path / leaf) (fun fid ->
          P9p.LowLevel.update ~length:0L t.conn fid
          >>*= fun () ->
          P9p.LowLevel.openfid t.conn fid Protocol_9p.Types.OpenMode.write_only
          >>*= fun _open ->
          write_to_fid t fid ~offset:0L data
          >>*= fun _resp ->
          ok ()
        )

    let write_stream t path data =
      Log.debug
        (fun f -> f "write %S to %a" (Cstruct.to_string data) pp_path path);
      with_file t path (fun fid ->
          P9p.LowLevel.openfid t.conn fid Protocol_9p.Types.OpenMode.write_only
          >>*= fun _open ->
          write_to_fid t fid ~offset:0L data
          >>*= fun _resp ->
          ok ()
        )

    (* TODO: limited to 2 GB files *)
    let read_all t path =
      P9p.read t.conn path 0L Int32.max_int >>*= fun data ->
      let data = Cstruct.concat data in
      Log.debug
        (fun f -> f "read_all %s -> %S" (String.concat ~sep:"/" path)
            (Cstruct.to_string data));
      ok data

    let remove t path =
      Log.debug (fun f -> f "remove %a" pp_path path);
      P9p.remove t.conn path

    let rename t path new_name =
      Log.debug (fun f -> f "rename %a to %s" pp_path path new_name);
      with_file t path (fun fid ->
        P9p.LowLevel.update t.conn ~name:new_name fid
      )

    let truncate t path new_length =
      Log.debug (fun f -> f "truncate %a to %Ld" pp_path path new_length);
      with_file t path (fun fid ->
        P9p.LowLevel.update t.conn ~length:new_length fid
      )

    let read_node t path =
      let open Protocol_9p_types in
      with_file_full t path @@ fun _fid { Protocol_9p_response.Walk.wqids } ->
      (* Note: would be more efficient to use [_fid] here... *)
      match last wqids with
      | Some qid when List.mem Qid.Symlink qid.Qid.flags ->
        (* Symlink *)
        read_all t path >>*= fun data ->
        ok (`Link (Cstruct.to_string data))
      | Some qid when not (List.mem Qid.Directory qid.Qid.flags) ->
        (* File *)
        read_all t path >>*= fun data ->
        ok (`File data)
      | _ ->
        (* Directory *)
        P9p.readdir t.conn path >>*= fun items ->
        let items = List.map (fun item -> item.Stat.name) items in
        ok (`Dir items)

    let read_file read =
      read >>*= function
      | `File x -> ok x
      | `Dir _
      | `Link _ -> error "Not a file"

    let read_dir read =
      read >>*= function
      | `Dir x -> ok x
      | `File _
      | `Link _ -> error "Not a directory"

    let read_link read =
      read >>*= function
      | `Link x -> ok x
      | `Dir _
      | `File _ -> error "Not a symlink"

    let stat t path =
      P9p.stat t.conn path >>= function
      | Error (`Msg "No such file or directory") -> ok None
      | Error _ as e -> Lwt.return e
      | Ok info ->
      let open Protocol_9p_types in
      let mode = info.Stat.mode in
      let kind =
        if mode.FileMode.is_directory then `Dir
        else if mode.FileMode.is_symlink then `Link
        else if List.mem `Execute mode.FileMode.owner then `Exec
        else `File in
      ok (Some {
        Datakit_S.kind;
        size = info.Stat.length;
      })

    let exists t path =
      stat t path >|*= function
      | None -> false
      | Some _ -> true

    let exists_dir t path =
      stat t path >|*= function
      | Some { Datakit_S.kind = `Dir; _ } -> true
      | _ -> false

    let exists_file t path =
      stat t path >|*= function
      | None | Some { Datakit_S.kind = `Dir; _ } -> false
      | _ -> true

    let set_executable t path exec =
      Log.debug (fun f -> f "set_executable %a to %b" pp_path path exec);
      let mode = if exec then rwxr_xr_x else rw_r__r__ in
      with_file t path (fun fid -> P9p.LowLevel.update t.conn ~mode fid)

    let random_subdir t parent =
      let rec aux = function
        | 0 -> error "Failed to create temporary directory!"
        | n ->
          let leaf = Int64.to_string (Random.int64 Int64.max_int) in
          create_dir t ~dir:parent leaf >>= function
          | Ok () -> ok leaf
          | Error (`Msg "Already exists") -> aux (n - 1)
          | Error _ as e -> Lwt.return e
      in
      aux 3

    (* Read lines from [path], calling [fn line] for each one.
       Continues as long as [fn] returns [`Again] and the switch is still on. *)
    let wait_for t ?switch path fn =
      with_file t path @@ fun fid ->
      P9p.LowLevel.openfid t.conn fid Protocol_9p.Types.OpenMode.read_only
      >>*= fun _resp ->
      let stream_offset = ref 0L in
      let read () =
        P9p.LowLevel.read t.conn fid !stream_offset 4096l >>*= fun resp ->
        let data = resp.Protocol_9p_response.Read.data in
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
        | `Eof -> error "End-of-file from monitor stream!"
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
      let path = Datakit_path.unwrap path in
      let rec aux user_path =
        Log.debug (fun f -> f "make_dirs.aux(%a)" (Fmt.Dump.list String.dump) user_path);
        match rdecons user_path with
        | None -> ok ()
        | Some (dir, leaf) ->
          create_dir t ~dir:(base @ dir) leaf >>= function
          | Ok () | Error (`Msg "Already exists") -> ok ()
          | Error (`Msg "No such file or directory") ->
            (* Parent is missing too *)
            aux dir >>*= fun () ->
            create_dir t ~dir:(base @ dir) leaf >>= begin function
            | Ok () | Error (`Msg "Already exists") -> ok ()
            | Error _ as e -> Lwt.return e
            end
          | Error _ as e -> Lwt.return e
      in
      aux path

    let remove_if_exists t path =
      exists t path >>*= function
      | true -> remove t path
      | false -> ok ()

    let create_or_replace t ~dir leaf value =
      let path = dir / leaf in
      exists t path >>*= function
      | true -> replace_file t dir leaf value
      | false -> create_file t ~executable:false ~dir:dir leaf value
  end

  module Tree = struct
    type t = { fs : FS.t; path : string list; }
    let of_id fs id = { fs; path = ["trees"; id] }
    let read t path = FS.read_node t.fs (t.path /@ path)
    let stat t path = FS.stat t.fs (t.path /@ path)
    let exists t path = FS.exists t.fs (t.path /@ path)
    let exists_dir t path = FS.exists_dir t.fs (t.path /@ path)
    let exists_file t path = FS.exists_file t.fs (t.path /@ path)
    let read_file t path = FS.read_file (read t path)
    let read_dir t path = FS.read_dir (read t path)
    let read_link t path = FS.read_link (read t path)
  end

  module Commit = struct
    type t = { fs : FS.t; id : string }
    let path t = ["snapshots"; t.id]
    let tree t = { Tree.fs = t.fs; path = path t / "ro" }
    let message t = FS.read_all t.fs (path t / "msg") >|*= Cstruct.to_string
    let id t = t.id

    let parents t =
      FS.read_all t.fs (path t / "parents") >|*= fun data ->
      lines (Cstruct.to_string data)
      |> List.map (fun hash -> {t with id = hash})

  end

  module Transaction = struct
    type t = {
      fs : FS.t;
      path : string list;
      mutable closed : bool;
    }

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

    let create_file t ~dir leaf ?(executable=false) data =
      FS.create_file t.fs ~executable ~dir:(rw_path t dir) leaf data

    let create_symlink t ~dir leaf target =
      FS.create_symlink t.fs ~dir:(rw_path t dir) leaf target

    let make_dirs t path =
      FS.make_dirs t.fs ~base:(t.path / "rw") path

    let create_dir t ~dir leaf =
      FS.create_dir t.fs ~dir:(rw_path t dir) leaf

    let set_parents t parents =
      if t.closed then raise (Invalid_argument "Transaction is closed");
      List.map (fun f -> Cstruct.of_string (f.Commit.id ^ "\n")) parents
      |> Cstruct.concat
      |> FS.replace_file t.fs t.path "parents"

    let replace_file t ~dir leaf data =
      FS.replace_file t.fs (rw_path t dir) leaf data

    let remove t path =
      FS.remove t.fs (rw_path t path)

    let rename t path new_name =
      FS.rename t.fs (rw_path t path) new_name

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
          match Datakit_path.of_string x with
          | Error e -> Error (`Msg e)
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
      let ours = { Tree.fs = t.fs; path = t.path / "ours" } in
      let theirs = { Tree.fs = t.fs; path = t.path / "theirs" } in
      let base = { Tree.fs = t.fs; path = t.path / "base" } in
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
      if t.closed then Lwt.return ()
      else (
        FS.write_stream t.fs (t.path / "ctl") (Cstruct.of_string "close")
        >>= function
        | Error (`Msg msg) ->
          Log.err
            (fun f -> f "Error aborting transaction %a: %s" pp_path t.path msg);
          t.closed <- true; (* Give up *)
          Lwt.return ()
        | Ok () ->
          t.closed <- true;
          Lwt.return ()
      )

    let read t path = FS.read_node t.fs (t.path / "rw" /@ path)
    let stat t path = FS.stat t.fs (t.path / "rw" /@ path)
    let exists t path = FS.exists t.fs (t.path / "rw" /@ path)
    let exists_file t path = FS.exists t.fs (t.path / "rw" /@ path)
    let exists_dir t path = FS.exists t.fs (t.path / "rw" /@ path)

    let create_or_replace_file t ~dir =
      FS.create_or_replace t.fs ~dir:(t.path / "rw" /@ dir)

    let read_file t path = FS.read_file (read t path)
    let read_dir t path = FS.read_dir (read t path)
    let read_link t path = FS.read_link (read t path)

    let parents t =
      FS.read_all t.fs (t.path / "parents") >|*= fun data ->
      lines (Cstruct.to_string data)
      |> List.map (fun hash -> {Commit.fs = t.fs; id = hash})
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

    let rename t new_name =
      FS.rename t.fs (branch_dir t) new_name >>*= fun () ->
      t.name <- new_name;
      ok ()

    let node_of_hash t = function
      | "" -> ok None
      | line ->
        let file f =
          (* TODO: delay loading this? *)
          FS.read_file (FS.read_node t.fs ["trees"; line]) >>*= fun contents ->
          ok (Some (f contents)) in
        match String.cut ~sep:"-" line with
        | None -> error "Invalid tree watch line!"
        | Some ("D", _) -> ok (Some (`Dir (Tree.of_id t.fs line)))
        | Some ("F", _) -> file (fun c -> `File c)
        | Some ("X", _) -> file (fun c -> `Exec c)
        | Some ("L", _) -> file (fun c -> `Link (Cstruct.to_string c))
        | Some (_, _) -> error "Invalid tree kind code"

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
      let path = Datakit_path.unwrap path in
      let path = List.map (fun x -> x ^ ".node") path in
      FS.wait_for t.fs ?switch (branch_dir t / "watch" @ (path / "tree.live"))
        (fun hash -> node_of_hash t hash >>*= fn)

    let fast_forward t commit =
      FS.write_stream t.fs
        (branch_dir t / "fast-forward") (Cstruct.of_string commit.Commit.id)

    let with_transaction t fn =
      Transaction.create t.fs (branch_dir t) >>*= fun tr ->
      Lwt.finalize
        (fun () ->
           fn tr >>*= fun result ->
           if tr.Transaction.closed then ok result
           else (
             Transaction.abort tr >|= fun () ->
             (* Make sure the user doesn't think their transaction succeeded *)
             failwith "Transaction returned Ok without committing or aborting (so forced abort)";
           )
        )
        (fun () ->
           if tr.Transaction.closed then Lwt.return ()
           else (
             (* Just log, so we don't hide the underlying error *)
             Log.info (fun f -> f "Transaction finished without committing or aborting (will abort)");
             Transaction.abort tr
           )
        )
  end

  module GitHub = struct
    type t = FS.t

    module Status = struct
      type t = {
        fs : FS.t;
        dir : string list;
      }

      let string_of_state = function
        | `Pending -> "pending"
        | `Success -> "success"
        | `Failure -> "failure"
        | `Error -> "error"

      let state_of_string = function
        | "pending" -> Ok `Pending
        | "success" -> Ok `Success
        | "failure" -> Ok `Failure
        | "error"   -> Ok `Error
        | s -> Error (`Msg ("Invalid GitHub state: " ^ s))

      let url_of_string s =
        try Ok (Uri.of_string s)
        with ex -> Error (`Msg (Printexc.to_string ex))

      let update t leaf fn = function
        | None -> FS.remove_if_exists t.fs (t.dir / leaf)
        | Some v -> FS.create_or_replace t.fs ~dir:t.dir leaf (Cstruct.of_string (fn v))

      let set_descr t = update t "descr" (fun x -> x)
      let set_state t = update t "state" string_of_state
      let set_url   t = update t "url"   Uri.to_string

      let read_file t leaf f =
        FS.read_file (FS.read_node t.fs (t.dir / leaf)) >|= function
        | Error (`Msg "No such file or directory") -> Ok None
        | Error _ as e -> e
        | Ok x ->
          match f (String.trim (Cstruct.to_string x)) with
          | Ok x -> Ok (Some x)
          | Error _ as e -> e

      let descr t = read_file t "descr" (fun x -> Ok x)
      let state t = read_file t "state" state_of_string
      let url t   = read_file t "url"   url_of_string
    end

    module PR = struct
      type t = {
        fs : FS.t;
        prs_dir : string list;
        id : string;
      }

      let id t = t.id

      let pr_dir t = t.prs_dir @ [t.id]

      let status t path =
        FS.make_dirs t.fs ~base:(pr_dir t / "status") path >>*= fun () ->
        ok { Status.fs = t.fs; dir = pr_dir t / "status" /@ path }
    end

    let pr_path ~user ~project = github_path / user / project / "pr"

    let prs t ~user ~project =
      let prs_dir = pr_path ~user ~project in
      FS.read_dir (FS.read_node t prs_dir) >|*=
      List.map (fun id -> { PR.fs = t; prs_dir; id })

    let pr t ~user ~project id =
      let prs_dir = pr_path ~user ~project in
      FS.exists t (prs_dir / id) >>*= function
      | false -> error "PR %S not found" id
      | true -> ok { PR.fs = t; prs_dir; id }
  end

  let branch t name =
    Branch.create t name

  let branches t =
    P9p.readdir t.FS.conn ["branch"] >|*=
    List.map (fun info -> info.Protocol_9p_types.Stat.name)

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
         ok { Commit.fs = t; id = Cstruct.to_string commit_id })
      (fun () ->
         FS.remove t path >|= function
         | Error (`Msg msg) ->
           Log.err (fun f -> f "Error removing remote %S: %s" id msg)
         | Ok () -> ())

  let commit t id =
    { Commit.fs = t; id }

  let tree t id =
    Tree.of_id t id

  let github t =
    FS.exists t github_path >|*= function
    | true -> Some t
    | false -> None

  let connect conn = { FS.conn }

  let disconnect t =
    P9p.disconnect t.FS.conn

  type t = FS.t

end
