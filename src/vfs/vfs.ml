open Rresult
open Lwt.Infix

module Error = struct

  type err = { errno: int32 option; descr: string }

  type t =
    | Noent
    | Isdir
    | Read_only_file
    | Perm
    | Other of err

  let noent = Error Noent
  let isdir = Error Isdir
  let read_only_file = Error Read_only_file
  let perm = Error Perm

  let other ?errno fmt =
    Printf.ksprintf (fun descr -> Error (Other { descr; errno })) fmt

  module Infix = struct

    open Lwt.Infix

    let (>>*=) x f =
      x >>= function
      | Ok x         -> f x
      | Error _ as e -> Lwt.return e

  end

end

open Error.Infix

let ok x = Lwt.return (Ok x)
let error fmt = Printf.ksprintf (fun s -> Lwt.return (Error.other "%s" s)) fmt

type 'a or_err = ('a, Error.t) Result.result Lwt.t

module File = struct

  let err_noent = Lwt.return Error.noent
  let err_read_only = Lwt.return Error.read_only_file
  let err_perm = Lwt.return Error.perm
  let err_negative_offset o = error "Negative offset %Ld" o
  let err_too_large_offset o l =
    error "Offset %Ld beyond end-of-file (len = %d)" o l
  let err_bad_write_offset off = error "Bad write offset %d" off
  let err_stream_seek = error "Attempt to seek in stream"
  let err_extend_cmd_file = error "Can't extend command file"

  let ok x = Lwt.return (Ok x)

  let check_offset ~offset len =
    if offset < 0L then err_negative_offset offset
    else if offset > Int64.of_int len then err_too_large_offset offset len
    else ok ()

  let empty = Cstruct.create 0

  module Stream = struct
    type t = {
      read : int -> Cstruct.t or_err;
      write: Cstruct.t -> unit or_err;
    }
    let create ~read ~write = { read; write }
    let read t = t.read
    let write t = t.write
  end

  module Fd = struct
    type t = {
      read : offset:int64 -> count:int -> Cstruct.t or_err;
      write: offset:int64 -> Cstruct.t -> unit or_err;
    }
    let create ~read ~write = { read; write }
    let read t = t.read
    let write t = t.write
    let static data =
      let read ~offset ~count =
        check_offset ~offset (Cstruct.len data) >>*= fun () ->
        let avail = Cstruct.shift data (Int64.to_int offset) in
        let count = min count (Cstruct.len avail) in
        ok (Cstruct.sub avail 0 count)
      in
      let write ~offset:_ _data = err_read_only in
      ok { read; write }

    let (++) = Int64.add

    let of_stream stream  =
      let current_offset = ref 0L in
      let need_flush = ref false in
      (* Linux requires a blocking read to return "" to indicate that it
         is blocking. Otherwise, it doesn't return the existing data to
         the application. To Linux, two "" in a row means end-of-file.
         Other systems will probably interpret a single "" as end-of-file.
         Oh well. *)
      let read ~offset ~count =
        if offset <> !current_offset then err_stream_seek
        else if !need_flush then (
          need_flush := false;
          ok empty
        ) else (
          Stream.read stream count >>*= fun result ->
          current_offset := !current_offset ++ Int64.of_int (Cstruct.len result);
          need_flush := true;
          ok result
        ) in
      let write ~offset data =
        if offset <> !current_offset then err_stream_seek
        else (
          Stream.write stream data >>*= fun () ->
          current_offset := !current_offset ++ Int64.of_int (Cstruct.len data);
          ok ()
        ) in
      ok { read; write }

  end

  type fd = Fd.t

  let read = Fd.read
  let write = Fd.write

  type t = {
    size: unit -> int64 or_err;
    open_: unit -> fd or_err;
    remove: unit -> unit or_err;
    truncate: int64 -> unit or_err;
  }

  let create ~size ~open_ ~remove ~truncate = { size; open_; remove; truncate }
  let size t = t.size ()
  let open_ t = t.open_ ()
  let remove t = t.remove ()
  let truncate t = t.truncate

  let read_only =
    create ~remove:(fun _ -> err_read_only) ~truncate:(fun _ -> err_read_only)

  let ro_of_cstruct data =
    let len = Cstruct.len data |> Int64.of_int in
    let size () = ok len in
    let open_ () = Fd.static data in
    read_only ~size ~open_

  let ro_of_string text = ro_of_cstruct (Cstruct.of_string text)

  let of_stream stream =
    let size () = ok 0L in
    let open_ () = stream () >>= fun s -> Fd.of_stream s in
    read_only ~size ~open_

  let command handler =
    (* Value currently being returned to user. Note that this is
       attached to the file, not the client's FD. This is so a shell
       client can write and then read in a separate step, but does
       mean we can't support parallel commands for a single FS (so if
       this is used, you should create a fresh FS for each client
       connection at least). *)
    let data = ref (Cstruct.create 0) in
    let size () = ok 0L in
    let open_ () =
      let read count =
        let count = min count (Cstruct.len !data) in
        let result = Cstruct.sub !data 0 count in
        data := Cstruct.shift !data count;
        ok result
      in
      let write buf =
        handler (Cstruct.to_string buf |> String.trim) >>*= fun result ->
        data := Cstruct.of_string result;
        ok ()
      in
      let stream = Stream.create ~read ~write in
      Fd.of_stream stream
    in
    let remove () = err_perm in
    let truncate = function
      | 0L -> ok () (* For `echo cmd > file` *)
      | _  -> err_extend_cmd_file
    in
    create ~size ~open_ ~remove ~truncate

  let status fn =
    let size () = fn () >|= fun data -> (Ok (String.length data |> Int64.of_int)) in
    let open_ () =
      let data = fn () >|= fun result -> ref (Cstruct.of_string result) in
      let read count =
        data >>= fun data ->
        let count = min count (Cstruct.len !data) in
        let result = Cstruct.sub !data 0 count in
        data := Cstruct.shift !data count;
        ok result
      in
      let write _ = err_read_only in
      let stream = Stream.create ~read ~write in
      Fd.of_stream stream
    in
    read_only ~size ~open_

  (* [overwrite orig (new, offset)] is a buffer [start; padding; new;
      end] where [new] is at position [offset], [start] and [end] are
      from [orig] and [padding] is zeroes inserted as needed. *)
  let overwrite orig (data, offset) =
    let orig = match orig with
      | None -> empty
      | Some orig -> orig
    in
    let orig_len = Cstruct.len orig in
    let data_len = Cstruct.len data in
    if offset = 0 && data_len >= orig_len then data (* Common, fast case *)
    else (
      let padding = Cstruct.create (max 0 (offset - orig_len)) in
      Cstruct.memset padding 0;
      let tail =
        let data_end = offset + data_len in
        if orig_len > data_end then Cstruct.sub orig data_end (orig_len - data_end)
        else empty in
      Cstruct.concat [
        Cstruct.sub orig 0 (min offset (Cstruct.len orig));
        padding;
        data;
        tail
      ]
    )

  let of_kv ~read ~write =
    let size () = read () >>*= function
      | None          -> err_noent
      | Some contents -> ok @@ Int64.of_int (Cstruct.len contents)
    in
    let open_ () =
      let read ~offset ~count =
        read () >>*= function
        | None -> err_noent
        | Some contents ->
          check_offset ~offset (Cstruct.len contents) >>*= fun () ->
          let avail = Cstruct.shift contents (Int64.to_int offset) in
          let count = min count (Cstruct.len avail) in
          ok (Cstruct.sub avail 0 count)
      and write ~offset data =
        let offset = Int64.to_int offset in
        if offset < 0 then err_bad_write_offset offset
        else (
          read () >>*= fun old ->
          write (overwrite old (data, offset))
        )
      in
      ok @@ Fd.create ~read ~write
    in
    let truncate len =
      let len = Int64.to_int len in
      if len = 0 then write empty
      else (
        read () >>*= fun old ->
        let old = match old with
          | None -> empty
          | Some old -> old
        in
        let extra = len - Cstruct.len old in
        if extra = 0 then Lwt.return (Ok ())
        else if extra < 0 then write (Cstruct.sub old 0 len)
        else (
          let padding = Cstruct.create extra in
          Cstruct.memset padding 0;
          write (Cstruct.append old padding)
        )
      ) in
    create ~size ~open_ ~truncate

  let of_kvro ~read =
    let write _ = err_read_only in
    let remove () = err_read_only in
    of_kv ~read ~write ~remove

  let rw_of_string init =
    let data = ref (Cstruct.of_string init) in
    let read () = ok (Some !data) in
    let write v = data := v; ok () in
    let remove () = err_read_only in
    let file = of_kv ~read ~write ~remove in
    (file, fun () -> Cstruct.to_string !data)

end

module Dir = struct

  let err_read_only = error "Directory is read-only"
  let err_already_exists = error "Already exists"
  let err_dir_only = error "Can only contain directories"
  let err_enoent = Lwt.return Error.noent

  module StringMap  = Map.Make(String)

  type t = {
    ls: unit -> inode list or_err;
    mkfile: string -> inode or_err;
    lookup: string -> inode or_err;
    mkdir: string -> inode or_err;
    remove: unit -> unit or_err;
    rename: inode -> string -> unit or_err;
  }

  and kind = [`File of File.t | `Dir of t]

  and inode = {
    basename: string;
    kind: kind;
  }

  type inode_map = inode StringMap.t

  let ls t = t.ls ()
  let mkfile t = t.mkfile
  let lookup t = t.lookup
  let mkdir t = t.mkdir
  let remove t = t.remove ()
  let rename t = t.rename

  let create ~ls ~mkfile ~lookup ~mkdir ~remove ~rename =
    {ls; mkfile; mkdir; remove; lookup; rename }

  let read_only =
    let mkfile _ = err_read_only in
    let mkdir _ = err_read_only in
    let rename _ _ = err_read_only in
    create ~mkfile ~mkdir ~rename

  let of_list items =
    let ls () = ok items in
    let lookup name =
      let rec aux = function
        | [] -> err_enoent
        | x :: _ when x.basename = name -> ok x
        | _ :: xs -> aux xs in
      aux items
    in
    let remove () = err_read_only in
    read_only ~ls ~lookup ~remove

  let empty = of_list []

  let of_map_ref m =
    let ls () = ok (StringMap.bindings !m |> List.map snd) in
    let lookup name =
      try ok (StringMap.find name !m)
      with Not_found -> err_enoent
    in
    let remove () = err_read_only in
    read_only ~ls ~lookup ~remove

  let directories ~make ~init =
    let lock = Lwt_mutex.create () in
    let items: inode_map ref =
      List.fold_left
        (fun acc inode -> acc |> StringMap.add inode.basename inode)
        StringMap.empty init
      |> ref
    in
    let ls () = ok (StringMap.bindings !items |> List.map snd) in
    let lookup name =
      try ok (StringMap.find name !items)
      with Not_found -> err_enoent
    in
    let mkdir name =
      Lwt_mutex.with_lock lock (fun () ->
          if StringMap.mem name !items then err_already_exists
          else (
            let remover = lazy (
              Lwt_mutex.with_lock lock (fun () ->
                  items := !items |> StringMap.remove name;
                  Lwt.return_unit
                )
            ) in
            make ~remover name >>= function
            | Ok dir ->
              if Lazy.is_val remover then err_enoent else (
                let inode = { basename = name; kind = `Dir dir } in
                items := !items |> StringMap.add name inode;
                ok inode
              )
            | Error _ as e -> Lwt.return e
          ))
    in
    let mkfile _ = err_dir_only in
    let rename _ _ = err_read_only in   (* TODO *)
    let remove _ = err_read_only in
    create ~ls ~mkfile ~mkdir ~lookup ~remove ~rename

  let dir_only  =
    let mkfile _ = err_dir_only in
    create ~mkfile

end

module Inode = struct
  type t = Dir.inode
  type kind = Dir.kind
  let file basename file = { Dir.basename; kind = `File file }
  let dir basename dir = { Dir.basename; kind = `Dir dir }
  let basename t = t.Dir.basename
  let kind t = t.Dir.kind
end
