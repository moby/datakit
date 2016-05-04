Ocamlbuild_plugin.mark_tag_used "tests";;

let env = BaseEnvLight.load () (* setup.data *)

let src_bin = "src/bin"
let main_pp = "file:src/bin/main_pp.ml"
let main = "file:src/bin/main.ml"

let pkg_dispatch ~pkg ~src ~findlib =
  let have_pkg = bool_of_string (BaseEnvLight.var_get pkg env) in
  function
  | After_rules  ->
    begin match have_pkg with
      | false -> Ocamlbuild_plugin.mark_tag_used ("pkg_" ^ pkg)
      | true  ->
        let flags = S [ A "-package"; A findlib ] in
        Pathname.define_context src_bin [src];
        flag [main_pp; "ocamldep"] flags;
        flag [main_pp; "ocaml"; "compile"] flags;
        let main = Filename.chop_suffix main ".ml" in
        flag [main ^ ".native"; "ocaml"; "link"] flags;
        flag [main ^ ".byte"; "ocaml"; "link"] flags;
    end;
    let var_name = String.uppercase pkg in
    let pp = match have_pkg with
      | false -> S[A "-pp"; A ("cppo -U HAVE_" ^ var_name)]
      | true  -> S[A "-pp"; A ("cppo -D HAVE_" ^ var_name)]
    in
    flag [main_pp; "ocamldep"] pp;
    flag [main_pp; "ocaml"; "compile"] pp;
  | _ -> ()

let github_dispatch =
  pkg_dispatch ~pkg:"github" ~src:"src/vgithub" ~findlib:"github.unix"

let namedpipe_dispatch =
  pkg_dispatch ~pkg:"named_pipe" ~src:"src/bin" ~findlib:"named-pipe.lwt"

let () =
  Ocamlbuild_plugin.dispatch
    (MyOCamlbuildBase.dispatch_combine [
        dispatch_default;
        github_dispatch;
      ])
