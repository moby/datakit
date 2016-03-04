open Lwt.Infix
open Result
module P = Protocol_9p
open Fs9p_misc

let max_chunk_size = Int32.of_int (100 * 1024)

module type S = sig
  type flow
  type inode
  module Inode: Fs9p_inode.S with type t = inode
  val accept: root:Inode.dir -> flow -> unit or_error Lwt.t
end

module Make (Log: Protocol_9p.S.LOG) (Flow: V1_LWT.FLOW) = struct
  type flow = Flow.flow

  (** An object (file or directory) in the filesystem. *)
  module Inode = struct
    type t = {
      qid : Protocol_9p.Types.Qid.t;  (* Unique id (similar to inode number) *)
      mutable name : string;          (* Statting a 9p file returns the name. *)
      obj : obj;
    }
    and obj =
      | File of Fs9p_file.t
      | Dir of dir
    and open_dir =                    (* All you can do with an open dir is list it *)
      { offset : int64;
        unread : t list }
    and dir = <
      ls : t list or_err Lwt.t;
      create : string -> t or_err Lwt.t;
      mkdir : string -> t or_err Lwt.t;
      lookup : string -> t or_err Lwt.t;
      remove : unit or_err Lwt.t;
      rename : t -> string -> unit or_err Lwt.t;
    >

    type open_obj =
      [ `OpenFile of Fs9p_file.open_file
      | `OpenDir of open_dir ]

    let mint_qid =
      let last = ref 0L in
      fun () ->
        let next = Int64.succ !last in
        last := next;
        next

    let of_dir name dir =
      let qid = P.Types.Qid.dir ~id:(mint_qid ()) ~version:0l () in
      { qid; name; obj = Dir dir }

    let basename t = t.name
    let qid t = t.qid

    let of_file name obj =
      let qid = P.Types.Qid.file ~id:(mint_qid ()) ~version:0l () in
      { qid; name; obj = File obj }
  end

  module Inode_ops = struct
    open Inode

    let rwx = [`Read; `Write; `Execute]
    let rw = [`Read; `Write]
    let rx = [`Read; `Execute]
    let r = [`Read]

    let stat ~info inode =
      let u =
        if info.P.Info.version = P.Types.Version.unix then
          Some (P.Types.Stat.make_extension ()) (* or mortdeus will crash *)
        else
          None in
      begin match inode.obj with
        | Dir _ ->
          Lwt.return (Ok (0L, P.Types.FileMode.make
                            ~owner:rwx ~group:rwx ~other:rx ~is_directory:true
                            ()))
        | File f -> Fs9p_file.size f >>*= fun length ->
          Lwt.return (Ok (length, P.Types.FileMode.make
                            ~owner:rw ~group:rw ~other:r ()))
      end >>*= fun (length, mode) ->
      Lwt.return (Ok (P.Types.Stat.make
                        ~qid:inode.qid ~mode ~length ~name:inode.name ?u ()))

    let rename dir inode new_name =
      match dir.obj with
      | File _ -> assert false
      | Dir d ->
          d#rename inode new_name >>*= fun () ->
          inode.name <- new_name;
          Lwt.return (Ok ())

    let truncate inode length =
      match inode.obj with
      | Dir _ when length = 0L -> Lwt.return (Ok ())
      | Dir _ -> Lwt.return (error "Can't set length of a directory")
      | File f -> Fs9p_file.truncate f length

    let read inode =
      match inode.obj with
      | File file ->
        Fs9p_file.open_ file >>*= fun o ->
        Lwt.return (Ok (`OpenFile o))
      | Dir dir ->
        dir#ls >>*= fun items ->
        Lwt.return (Ok (`OpenDir { offset = 0L; unread = items } ))

    let read_dir ~info ~offset ~count state =
      if offset <> state.offset then (
        Lwt.return (error "Can't seek in a directory");    (* TODO: allow 0 to restart *)
      ) else (
        let buffer = Cstruct.create count in
        let rec aux buf = function
          | [] -> Lwt.return (Ok (buf, []))   (* Done *)
          | x :: xs ->
            stat ~info x >>*= fun x_info ->
            match P.Types.Stat.write x_info buf with
            | Ok buf -> aux buf xs
            | Error _ -> Lwt.return (Ok (buf, xs)) in    (* No more room *)
        aux buffer state.unread >>*= fun (unused, remaining) ->
        let data = Cstruct.sub buffer 0 (count - Cstruct.len unused) in
        let len = Cstruct.len data in
        if len = 0 && remaining <> [] then Lwt.return (error "Buffer too small")
        else (
          let new_state = { offset = state.offset ++ Int64.of_int len; unread = remaining } in
          Lwt.return (Ok (new_state, data))
        )
      )

    let create ~parent ~perm name =
      match parent.obj with
      | Dir d ->
        let inode =
          if perm.P.Types.FileMode.is_directory then d#mkdir name
          else d#create name
        in
        inode >>*= fun inode ->
        read inode >>*= fun open_file ->
        Lwt.return (Ok (inode, open_file))
      | File _ -> Lwt.return (error "%S is not a directory" parent.name)

    let remove inode =
      match inode.obj with
      | File f -> Fs9p_file.remove f
      | Dir d  -> d#remove
  end

  (** Handle incoming requests from the client. *)
  module Dispatcher = struct
    type fd = {
      inode : Inode.t;
      parents : Inode.t list;   (* closest first *)
      mutable state :
        [ `Ready
        | Inode.open_obj ]
    }

    type t = Inode.dir  (* The root directory *)

    type connection = {
      root : t;
      info : Protocol_9p.Info.t;
      mutable fds : fd P.Types.Fid.Map.t;
    }

    let connect root info =
      let fds = P.Types.Fid.Map.empty in
      { root; info; fds }

    let fmt_fid () x = P.Types.Fid.sexp_of_t x |> Sexplib.Conv.string_of_sexp

    let lookup connection fid =
      try Ok (P.Types.Fid.Map.find fid connection.fds)
      with Not_found -> error "Unknown fid %a" fmt_fid fid

    let alloc_fid ?may_reuse connection newfid fd =
      let alloc () =
        connection.fds <- connection.fds |> P.Types.Fid.Map.add newfid fd; Ok () in
      match may_reuse with
      | Some old when old = newfid -> alloc ()
      | Some _ | None ->
        if P.Types.Fid.Map.mem newfid connection.fds then
          error "Fid %a already in use" fmt_fid newfid
        else alloc ()

    (* Returns the final inode, the path that led to it, and the new parents. *)
    let rec do_walk ~parents ~wqids inode = function
      | [] -> Lwt.return (Ok (inode, List.rev wqids, parents))
      | x :: xs ->
        match inode.Inode.obj with
        | Inode.File _ -> Lwt.return (error "Can't walk from a file")
        | Inode.Dir dir ->
            begin match x with
            | "." -> Lwt.return (error "'.' is not valid in 9p")
            | ".." ->
                begin match parents with
                | [] -> Lwt.return (Ok (inode, parents))     (* /.. = / *)
                | p::ps -> Lwt.return (Ok (p, ps)) end
            | x ->
                dir#lookup x >>*= fun x_inode ->
                Lwt.return (Ok (x_inode, inode :: parents))
            end >>*= fun (inode, parents) ->
            let wqids = Inode.qid inode :: wqids in
            do_walk ~parents ~wqids inode xs

    let walk connection ~cancel:_ { P.Request.Walk.fid; newfid; wnames } =
      Lwt.return (lookup connection fid) >>*= fun fd ->
      do_walk ~parents:fd.parents ~wqids:[] fd.inode wnames >>*= fun (inode, wqids, parents) ->
      Lwt.return (alloc_fid ~may_reuse:fid connection newfid { inode; parents; state = `Ready })
      >>*= fun () ->
      Lwt.return (Ok { P.Response.Walk.wqids })

    let attach connection ~cancel:_ { P.Request.Attach.fid; _ } =
      let fd = {
        inode = Inode.of_dir "/" connection.root;
        parents = [];
        state = `Ready
      } in
      Lwt.return (
        alloc_fid connection fid fd >>!= fun () ->
        Ok { P.Response.Attach.qid = fd.inode.Inode.qid }
      )

    let clunk_fid connection fid =
      connection.fds <- connection.fds |> P.Types.Fid.Map.remove fid

    let clunk connection ~cancel:_ { P.Request.Clunk.fid } =
      Lwt.return (
        let old = connection.fds in
        clunk_fid connection fid;
        if connection.fds == old then error "Unknown fid %a" fmt_fid fid else
          Ok ()
      )

    let stat connection ~cancel:_ { P.Request.Stat.fid } =
      Lwt.return (lookup connection fid) >>*= fun fd ->
      Inode_ops.stat ~info:connection.info fd.inode >>*= fun stat ->
      Lwt.return (Ok { P.Response.Stat.stat })

    let read connection ~cancel:_ { P.Request.Read.fid; offset; count } =
      let count = Int32.to_int (min count max_chunk_size) in
      Lwt.return (lookup connection fid) >>*= fun fd ->
      match fd.state with
      | `Ready -> Lwt.return (error "Can't read from unopened fid")
      | `OpenFile file ->
        Fs9p_file.read file ~offset ~count >>*= fun data ->
        Lwt.return (Ok { P.Response.Read.data } )
      | `OpenDir d ->
        Inode_ops.read_dir ~info:connection.info ~offset ~count d >>*= fun (new_state, data) ->
        fd.state <- `OpenDir new_state;
        Lwt.return (Ok { P.Response.Read.data } )

    let open_ connection ~cancel:_ { P.Request.Open.fid; mode } =
      ignore mode;
      Lwt.return (lookup connection fid) >>*= fun fd ->
      match fd.state with
      | `OpenDir _ | `OpenFile _ -> Lwt.return (error "Already open")
      | `Ready ->
        Inode_ops.read fd.inode >>*= fun state ->
        fd.state <- state;
        Lwt.return (Ok { P.Response.Open.qid = fd.inode.Inode.qid; iounit = 0l })

    let create connection ~cancel:_ { P.Request.Create.fid; name; perm; mode; extension } =
      ignore (name, mode, extension);
      Lwt.return (lookup connection fid) >>*= fun fd ->
      if fd.state <> `Ready then
        Lwt.return (error "Can't create in an opened fid")
      else (
        Inode_ops.create ~parent:fd.inode ~perm name >>*= fun (inode, open_file) ->
        let fd = { inode; parents = fd.inode :: fd.parents; state = open_file } in
        connection.fds <- connection.fds |> P.Types.Fid.Map.add fid fd;
        Lwt.return (Ok { P.Response.Create.qid = inode.Inode.qid; iounit = 0l })
      )

    let write connection ~cancel:_ { P.Request.Write.fid; offset; data } =
      Lwt.return (lookup connection fid) >>*= fun fd ->
      match fd.state with
      | `Ready -> Lwt.return (error "Can't write to unopened fid")
      | `OpenDir _ -> Lwt.return (error "Can't write to directories")
      | `OpenFile file ->
        Fs9p_file.write file ~offset data >>*= fun () ->
        let count = Int32.of_int (Cstruct.len data) in
        Lwt.return (Ok { P.Response.Write.count } )

    let remove connection ~cancel:_ { P.Request.Remove.fid } =
      Lwt.return (lookup connection fid) >>*= fun fd ->
      Inode_ops.remove fd.inode >|= fun err ->
      clunk_fid connection fid;
      err

    let rename fd name =
      match fd.parents with
      | [] -> Lwt.return (error "Can't rename /")
      | p::_ -> Inode_ops.rename p fd.inode name

    let wstat connection ~cancel:_ { P.Request.Wstat.fid; stat } =
      Lwt.return (lookup connection fid) >>*= fun fd ->
      let { P.Types.Stat.name; length; mode; mtime; gid;
            (* It's illegal to set these, but checking if we're setting
               to the current value is tedious, so ignore: *)
            ty = _; dev = _; qid = _; atime = _; uid = _; muid = _; u = _} = stat in
      ignore mtime;             (* Linux needs to set mtime *)
      ignore gid;               (* We don't care about permissions *)
      ignore mode;              (* TODO: reject changing dir bit? *)
      let name = if name = "" then None else Some name in
      let length = if P.Types.Int64.is_any length then None else Some length in
      match name, length with
      | Some name, None -> rename fd name
      | None, Some length -> Inode_ops.truncate fd.inode length
      | None, None -> Lwt.return (Ok ())
      | Some _, Some _ ->
          (* Hard to support atomically, and unlikely to be useful. *)
          Lwt.return (error "Can't rename and truncate at the same time")
  end

  module Server = P.Server.Make(Log)(Flow)(Dispatcher)

  type inode = Inode.t

  let accept ~root flow =
    Server.connect root flow () >>= function
    | Error _ as e -> Flow.close flow >|= fun () -> e
    | Ok t ->
      ignore t;   (* XXX: When to close flow? *)
      Lwt.return (Ok ())
end
