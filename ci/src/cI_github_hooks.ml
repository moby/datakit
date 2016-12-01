open Astring

module CI = struct
  type t = string list
  let circle_ci = ["ci"; "circleci"]
  let datakit_ci x = ["ci"; "datakit"; x]
end
