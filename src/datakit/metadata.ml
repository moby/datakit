module X = struct
  type t = [`Normal | `Exec | `Link]
  let t = Irmin.Type.enum "metadata" [
      ("normal", `Normal);
      ("exec"  , `Exec);
      ("link"  , `Link);
    ]
end
include X
let default = `Normal
let merge = Irmin.Merge.default X.t
