(* Helpers for creating files. *)

open Lwt.Infix
open Result
open Fs9p_misc

type open_file = <
  read : offset:int64 -> count:int -> Cstruct.t or_err Lwt.t;
  write: offset:int64 -> Cstruct.t -> unit or_err Lwt.t;
>

let read t = t#read
let write t = t#write

class type t = object
  method size     : int64 or_err Lwt.t
  method open_file: open_file or_err Lwt.t
  method remove   : unit or_err Lwt.t
  method truncate : int64 -> unit or_err Lwt.t
end

let err_enoent = Lwt.return Fs9p_error.enoent

let empty = Cstruct.create 0

let size t = t#size
let open_ t = t#open_file
let remove t = t#remove
let truncate t len = t#truncate len

type stream =
  (int -> Cstruct.t or_err Lwt.t) * (Cstruct.t -> unit or_err Lwt.t)

let check_offset ~offset len =
  if offset < 0L then
    Lwt.return (error "Negative offset %Ld" offset)
  else if offset > Int64.of_int len then
    Lwt.return (error "Offset %Ld beyond end-of-file (len = %d)" offset len)
  else
    Lwt.return (Ok ())

class virtual read_only =
  object (_ : #t)
    method remove = Lwt.return (error "Read-only file")
    method truncate _ = Lwt.return (error "Read-only file")
  end

let static data =
  object (_ : t)
    inherit read_only

    method size = Lwt.return (Ok (Cstruct.len data |> Int64.of_int))

    method open_file =
      let o =
        object (_ : open_file)
          method read ~offset ~count =
            check_offset ~offset (Cstruct.len data) >>*= fun () ->
            let avail = Cstruct.shift data (Int64.to_int offset) in
            let count = min count (Cstruct.len avail) in
            Lwt.return (Ok (Cstruct.sub avail 0 count))
          method write ~offset:_ _data = Lwt.return (error "Read-only file")
        end in
      Lwt.return (Ok o)
  end

let static_string text = static (Cstruct.of_string text)

let open_file_of_stream (read, write) =
  object
    val mutable current_offset = 0L

    val mutable need_flush = false
    (* Linux requires a blocking read to return "" to indicate that it
       is blocking. Otherwise, it doesn't return the existing data to the
       application. To Linux, two "" in a row means end-of-file.
       Other systems will probably interpret a single "" as end-of-file.
       Oh well. *)

    method read ~offset ~count =
      if offset <> current_offset then
        Lwt.return (error "Attempt to seek in stream")
      else (
        if need_flush then (
          need_flush <- false;
          Lwt.return (Ok empty)
        ) else (
          read count >>*= fun result ->
          current_offset <- current_offset ++ Int64.of_int (Cstruct.len result);
          need_flush <- true;
          Lwt.return (Ok result)
        )
      )
    method write ~offset data =
      if offset <> current_offset then
        Lwt.return (error "Attempt to seek in stream")
      else (
        write data >>*= fun () ->
        current_offset <- current_offset ++ Int64.of_int (Cstruct.len data);
        Lwt.return (Ok ())
      )
  end

let of_stream stream: t =
  object
    inherit read_only

    method size = Lwt.return (Ok 0L)
    method open_file = stream () >|= fun s -> Ok (open_file_of_stream s)
  end

let command handler =
  object (_ : t)
    (* Value currently being returned to user. Note that this is
       attached to the file, not the client's FD. This is so a shell
       client can write and then read in a separate step, but does
       mean we can't support parallel commands for a single FS (so if
       this is used, you should create a fresh FS for each client
       connection at least). *)
    val mutable data = Cstruct.create 0

    method size = Lwt.return (Ok 0L)

    method open_file =
      let read count =
        let count = min count (Cstruct.len data) in
        let result = Cstruct.sub data 0 count in
        data <- Cstruct.shift data count;
        Lwt.return (Ok result)
      in
      let write buf =
        handler (Cstruct.to_string buf |> String.trim) >>*= fun result ->
        data <- Cstruct.of_string result;
        Lwt.return (Ok ())
      in
      Lwt.return (Ok (open_file_of_stream (read, write)))

    method remove = Lwt.return Fs9p_error.eperm

    method truncate = function
      | 0L -> Lwt.return (Ok ())        (* For `echo cmd > file` *)
      | _ -> Lwt.return (error "Can't extend command file")
  end

(** Calls a function each time it is opened and lets the caller read the value back. *)
let status fn =
  object (_ : t)
    inherit read_only

    method size =
      fn () >|= fun data -> (Ok (String.length data |> Int64.of_int))

    method open_file =
      let data = fn () >|= fun result -> ref (Cstruct.of_string result) in
      let read count =
        data >>= fun data ->
        let count = min count (Cstruct.len !data) in
        let result = Cstruct.sub !data 0 count in
        data := Cstruct.shift !data count;
        Lwt.return (Ok result)
      in
      let write _ = Lwt.return (error "Read-only file") in
      Lwt.return (Ok (open_file_of_stream (read, write)))
  end

(** [overwrite orig (new, offset)] is a buffer [start; padding; new; end]
    where [new] is at position [offset], [start] and [end] are from [orig]
    and [padding] is zeroes inserted as needed. *)
let overwrite orig (data, offset) =
  let orig =
    match orig with
    | None -> empty
    | Some orig -> orig in
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

let read_write ~read ~write ~remove =
  object
    method size =
      read () >>*= function
      | None -> err_enoent
      | Some contents -> Lwt.return (Ok (Int64.of_int (Cstruct.len contents)))

    method open_file =
      let o =
        object (_ : open_file)
          method read ~offset ~count =
            read () >>*= function
            | None -> err_enoent
            | Some contents ->
            check_offset ~offset (Cstruct.len contents) >>*= fun () ->
            let avail = Cstruct.shift contents (Int64.to_int offset) in
            let count = min count (Cstruct.len avail) in
            Lwt.return (Ok (Cstruct.sub avail 0 count))
          method write ~offset data =
            let offset = Int64.to_int offset in
            if offset < 0 then Lwt.return (error "Bad write offset %d" offset)
            else (
              read () >>*= fun old ->
              write (overwrite old (data, offset))
            )
        end in
      Lwt.return (Ok o)

    method remove = remove ()

    method truncate len =
      let len = Int64.to_int len in
      if len = 0 then write empty
      else (
        read () >>*= fun old ->
        let old =
          match old with
          | None -> empty
          | Some old -> old in
        let extra = len - Cstruct.len old in
        if extra = 0 then Lwt.return (Ok ())
        else if extra < 0 then write (Cstruct.sub old 0 len)
        else (
          let padding = Cstruct.create extra in
          Cstruct.memset padding 0;
          write (Cstruct.append old padding)
        )
      )
  end

let read_only ~read =
  let write _ = Lwt.return (error "Read-only file") in
  let remove () = Lwt.return (error "Read-only file") in
  read_write ~read ~write ~remove

let mutable_string init =
  let data = ref (Cstruct.of_string init) in
  let read () = Lwt.return (Ok (Some !data)) in
  let write v = data := v; Lwt.return (Ok ()) in
  let remove () = Lwt.return (error "Read-only file") in
  let file = read_write ~read ~write ~remove in
  (file, fun () -> Cstruct.to_string !data)
