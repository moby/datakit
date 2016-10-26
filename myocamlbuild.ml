open Ocamlbuild_plugin

let crunch_command env _build =
  let out = env "ci/src/cI_static.ml" in
  Cmd(S[A "ocaml-crunch"; A "--mode=plain"; A "-o"; Px out; P "ci/static"])

let rec all_files dir =
  Ocamlbuild_plugin.Pathname.readdir dir
  |> Array.to_list
  |> List.map (fun name ->
      let path = Filename.concat dir name in
      if Sys.is_directory path then
        all_files path
      else
        [path]
    )
  |> List.concat

let () =
  dispatch (function
    | Before_rules ->
      rule "crunch"
        ~prod:"ci/src/cI_static.ml"
        ~deps:(all_files "ci/static")
        ~doc:"run ocaml-crunch on a directory"
        crunch_command
    | _ -> ())
