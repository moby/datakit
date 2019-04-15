include Irmin.Branch.String

let is_valid s =
  let ok = ref true in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    ( match s.[!i] with
    | '/' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' -> ()
    | _ -> ok := false );
    incr i
  done;
  !ok
