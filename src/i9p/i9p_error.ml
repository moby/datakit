(* Must match exactly what Linux is expecting *)

open I9p_misc

let enoent = error "No such file or directory"
let eisdir = error "Is a directory"
let ero = error "Read-only file"
let eperm = error "Operation not permitted"
