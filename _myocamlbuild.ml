Ocamlbuild_plugin.mark_tag_used "tests";;

let env = BaseEnvLight.load () (* setup.data *)

let cppo_dispatch pkg tags =
  let have_pkg = bool_of_string (BaseEnvLight.var_get pkg env) in
  let pp_pkg = "pp_" ^ pkg in
  function
  | After_rules ->
    let pp = match have_pkg with
      | false -> S[A "-pp"; A ("cppo -U HAVE_" ^ String.capitalize pkg)]
      | true  -> S[A "-pp"; A ("cppo -D HAVE_" ^ String.capitalize pkg)]
    in
    flag [pp_pkg; "ocamldep"] pp;
    flag [pp_pkg; "ocaml"; "compile"] pp;
    begin match have_pkg with
      | false -> Ocamlbuild_plugin.mark_tag_used ("pkg_" ^ pkg)
      | true  ->
        let tags = Tags.of_list tags in
        let flags = S [T tags] in
        flag [pp_pkg; "ocamldep"] flags;
        flag [pp_pkg; "ocaml"; "compile"] flags;
        flag [pp_pkg; "ocaml"; "link"] flags;
    end
  | _ -> ()

let github_distpatch =
  cppo_dispatch "github" ["package(github.unix)";"use_vgithub"]

let () =
  Ocamlbuild_plugin.dispatch
    (MyOCamlbuildBase.dispatch_combine [ dispatch_default; github_distpatch])
