open Ocamlbuild_plugin

let src_vgithub = "src/vgithub"
let src_bin = "src/bin"
let main_pp = "file:src/bin/main_pp.ml"
let main = "file:src/bin/main.ml"

let github_dispatch =
  let pkg = "github" in
  let have_pkg =
    run_and_read ("opam config var " ^ pkg ^ ":installed")
    |> String.trim
    |> bool_of_string
  in
  function
  | After_rules  ->
    begin match have_pkg with
      | false -> ()
      | true  ->
        let flags = S [ A "-package"; A "github.unix" ] in
        Pathname.define_context src_bin [src_vgithub];
        flag [main_pp; "ocamldep"] flags;
        flag [main_pp; "ocaml"; "compile"] flags;
        let main = Filename.chop_suffix main ".ml" in
        flag [main ^ ".native"; "ocaml"; "link"] flags;
        flag [main ^ ".byte"; "ocaml"; "link"] flags;
    end;
    let pp = match have_pkg with
      | false -> S[A "-pp"; A ("cppo -U HAVE_" ^ String.uppercase pkg)]
      | true  -> S[A "-pp"; A ("cppo -D HAVE_" ^ String.uppercase pkg)]
    in
    flag [main_pp; "ocamldep"] pp;
    flag [main_pp; "ocaml"; "compile"] pp;
  | _ -> ()

let () = Ocamlbuild_plugin.dispatch github_dispatch
