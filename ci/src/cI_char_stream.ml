type t = string * int

type span = string * int * int

let of_string s = (s, 0)

let to_string (s, i) = String.sub s i (String.length s - i)

let skip (s, a) = (s, a + 1)

let skip_all (s, _) = (s, String.length s)

let string_of_span (s, a, b) = String.sub s a (b - a)

let ( -- ) (s, a) (_, b) =
  assert (b >= a);
  (s, a, b)

let find (base, off) c =
  try Some (base, String.index_from base off c) with Not_found -> None

let avail (base, off) = String.length base - off

let is_empty (base, off) = String.length base = off

let next (base, off) =
  if String.length base = off then None else Some (base.[off], (base, off + 1))

let equal (a : t) (b : t) = a = b
