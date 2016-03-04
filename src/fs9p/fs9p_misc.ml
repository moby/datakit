open Result

type leaf = string
type path = string list

let ( ++ ) = Int64.add

type 'a or_error = 'a Protocol_9p.Error.t
type 'a or_err = ('a, Protocol_9p.Response.Err.t) result

let ok x = Ok x
let error ?(errno=0l) fmt =
  Printf.ksprintf (fun ename ->
      Error {Protocol_9p.Response.Err.ename; errno = Some errno}
    ) fmt
let ( >>*= ) = Protocol_9p.Infix.( >>*= )
let ( >>!= ) = Protocol_9p.Error.(>>=)
