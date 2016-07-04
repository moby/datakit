open Result

type t = Cstruct.t list ref (* (reversed) *)

let ( >>!= ) x f =
  match x with
  | Ok x -> f x
  | Error _ as e -> e

let len t = Cstruct.lenv !t

let empty_cs = Cstruct.create 0
let empty = ref []

let of_string s = ref [Cstruct.of_string s]

let of_ro_cstruct cs = ref [cs]

let to_ro_cstruct t =
  let cs = Cstruct.concat (List.rev !t) in
  t := [cs];
  cs

let to_string t =
  Cstruct.to_string (to_ro_cstruct t)

(* [overwrite orig (new, offset)] is a buffer [start; padding; new;
    end] where [new] is at position [offset], [start] and [end] are
    from [orig] and [padding] is zeroes inserted as needed. *)
let overwrite orig (data, offset) =
  let orig_len = Cstruct.len orig in
  let data_len = Cstruct.len data in
  if offset = 0 && data_len >= orig_len then data (* Common, fast case *)
  else (
    let padding = Cstruct.create (max 0 (offset - orig_len)) in
    let tail =
      let data_end = offset + data_len in
      if orig_len > data_end then Cstruct.sub orig data_end (orig_len - data_end)
      else empty_cs in
    Cstruct.concat [
      Cstruct.sub orig 0 (min offset (Cstruct.len orig));
      padding;
      data;
      tail
    ]
  )

let check_offset ~offset len =
  let len = Int64.of_int len in
  if offset < 0L then Vfs.Error.negative_offset offset
  else if offset > len then Vfs.Error.offset_too_large ~offset len
  else Ok ()
  
let read t ~offset ~count =
  let contents = to_ro_cstruct t in
  check_offset ~offset (Cstruct.len contents) >>!= fun () ->
  let avail = Cstruct.shift contents (Int64.to_int offset) in
  let count = min (max count 0) (Cstruct.len avail) in
  Ok (Cstruct.sub avail 0 count)

let write old ~offset data =
  if offset < 0L then Vfs.Error.negative_offset offset
  else (
    let offset = Int64.to_int offset in
    if offset = len old then Ok (ref (data :: !old))
    else Ok (of_ro_cstruct (overwrite (to_ro_cstruct old) (data, offset)))
  )

let truncate old = function
  | n when n < 0L -> Vfs.Error.negative_offset n
  | 0L -> Ok empty
  | new_len ->
    let new_len = Int64.to_int new_len in
    let extra = new_len - len old in
    if extra = 0 then Ok old
    else if extra < 0 then (
      Ok (of_ro_cstruct (Cstruct.sub (to_ro_cstruct old) 0 new_len))
    ) else (
      let padding = Cstruct.create extra in
      Ok (ref (padding :: !old))
    )

let len t = Int64.of_int (len t)
