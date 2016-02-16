(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open OpamTypes

module OT = OpamState.Types
module OF = OpamFilename
module OSC = OpamStateConfig
module OGraph = OpamSolver.ActionGraph
module OS = OpamSystem

let (/) = Filename.concat

type t = { root: string; switch: Switch.t; }

let pp_t ppf t = Fmt.pf ppf "root:%s switch:%a" t.root Switch.pp t.switch

type plan = {
  g: OGraph.t;
  h: Host.t;
  s: Switch.t;
}

let debug fmt = Gol.debug ~section:"opam" fmt
let fail fmt = Printf.ksprintf failwith ("Ciso.Opam: " ^^ fmt)

let package_s p = OpamPackage.to_string p

let package_of_opam p =
  let name = OpamPackage.(Name.to_string @@ name p) in
  let version = OpamPackage.(Version.to_string @@ version p) in
  Package.create ~version name

let opam_of_package p =
  let name = OpamPackage.(Name.of_string @@ Package.name p) in
  let version = match Package.version p with
    | None   -> failwith "no version!"
    | Some v -> OpamPackage.Version.of_string v
  in
  OpamPackage.create name version

let parse_atom str =
  match fst OpamArg.atom str with `Ok a -> a | `Error s ->
    fail "Cannot parse %s: %s\n%!" str s

let atom_of_package p = parse_atom (Package.to_string p)
let opam_switch s = OpamSwitch.of_string (Switch.to_string s)

let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic);
    r
  with exn ->
    ignore (Unix.close_process_in ic);
    raise exn

let check t o =
  let assert_eq got expected =
    if got <> expected then (
      let red = Fmt.(styled `Red string) in
      let yellow = Fmt.(styled `Yellow string) in
      Fmt.(pf stderr) "%a got %a, but was expecting %a.\n%!"
        red "Error" yellow got yellow expected;
      assert false
    )
  in
  let check k expected =
    let cmd = Fmt.strf "opam config var %s" k in
    with_process_in cmd (fun ic ->
        let got = String.trim (input_line ic) in
        assert_eq got expected
      )
  in
  check "root"     t.root;
  check "prefix"   (t.root / Switch.to_string t.switch);
  check "compiler" (Switch.to_string t.switch);
  assert_eq t.root (OF.Dir.to_string OSC.(!r.root_dir));
  assert_eq
    (Switch.to_string t.switch) (OpamSwitch.to_string OSC.(!r.current_switch));
  match o with
  | None   -> ()
  | Some o ->
    assert_eq t.root (OF.Dir.to_string o.OT.root);
    assert_eq (Switch.to_string t.switch) (OpamSwitch.to_string o.OT.switch);
    assert_eq (Switch.to_string t.switch) (OpamCompiler.to_string o.OT.compiler)

let init_config t dbg =
  debug "init_config: %a %s" pp_t t dbg;
  Unix.putenv "OPAMROOT" t.root;
  Unix.putenv "OPAMSWITCH" (Switch.to_string t.switch);
  let current_switch = opam_switch t.switch in
  let root_dir = OF.Dir.of_string t.root in
  OpamClientConfig.opam_init
    ~root_dir ~current_switch ~strict:false
    ~skip_version_checks:true ~answer:None ~keep_build_dir:true
    ();
  check t None

(*
let repo t name url =
  let repo_name = OpamRepositoryName.of_string name in
  let repo_priority = 0 in
  let repo_address, repo_kind = OpamTypesBase.parse_url url in
  let repo_root =
    OpamRepositoryPath.create (OF.Dir.of_string t.root) repo_name
  in
  { repo_root; repo_name; repo_kind; repo_address; repo_priority }
*)

let load_state t dbg =
  init_config t dbg;
  debug "load_state %s" dbg;
  let switch = opam_switch t.switch in
  let o = OpamState.load_state ~save_cache:true ("ci-opam-" ^ dbg) switch in
  check t (Some o);
  o

let create ~root switch =
  let t = match switch with
    | Some s -> { root; switch = s}
    | None   ->
      let aliases = root / "aliases"  in
      if Sys.file_exists aliases then
        let aliases = OpamFile.Aliases.read (OF.of_string aliases) in
        match OpamSwitch.Map.bindings aliases with
        | []        -> { root; switch = Switch.system }
        | (s, _)::_ -> { root; switch = Switch.of_string (OpamSwitch.to_string s)}
      else
        { root; switch = Switch.system }
  in
  if not OF.(exists_dir Dir.(of_string (t.root / "system"))) then (
    (* FIXME: we don't want opam 1.3, so shell out `opam init...`*)
    (*   let repo = repo t "default" default_repo in
         let comp = OpamCompiler.system in
         let root = OF.of_string t.root in
         OpamClient.SafeAPI.init repo comp `bash root `no; *)
    (* FIXME: use Bos *)
    let cmd = Printf.sprintf "opam init --root=%s -n" t.root in
    match Sys.command cmd with
    | 0 -> ()
    | i -> Printf.ksprintf failwith "%s failed (exit %d)!" cmd i
  );
  init_config t "create";
  t

let get_var t v =
  let t = load_state t "get-var" in
  OpamVariable.Full.of_string v
  |> OpamState.contents_of_variable (lazy t)
  |> OpamVariable.string_of_variable_contents

(* FIXME: this doesn't work as OPAM is caching the env variables
   lazily. *)
let _set_env t s h =
  let app k v = Unix.putenv ("OPAMVAR_" ^ k) v in
  let tts = Fmt.to_to_string in
  List.iter (fun (k, v) -> app k v) [
    "os"          , tts Host.pp_os @@ Host.os h;
    "switch"      , Switch.to_string s;
    "os"          , Switch.to_string s;
    "preinstalled", "false";
  ];
  Unix.putenv "OPAMROOT" t.root

let eval_opam_config_env t =
  let env_s = load_state t "opam-eval-env" in
  let env = OpamState.get_opam_env ~force_path:true env_s in
  List.iter (fun (n, v, _) -> Unix.putenv n v) env

let resolve t atoms_s =
  let state = load_state t "resolve" in
  let atoms = List.rev_map parse_atom atoms_s in
  let install_set = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let action = Install install_set in
  let universe = OpamState.universe state action in
  let request = OpamSolver.request ~install:atoms () in
  let result =
    OpamSolver.resolve ~orphans:OpamPackage.Set.empty universe request
  in
  let solution = match result with
    | Success s -> Some s
    | Conflicts c ->
      let info = OpamCudf.string_of_conflict OpamFormula.string_of_atom c in
      let str = String.concat ";" atoms_s in
      debug "no solution for %s: %s" str info;
      None
  in
  match solution with
  | None   -> None
  | Some s ->
    let graph = OpamSolver.get_atomic_action_graph s in
    let oc = open_out "solver_log" in
    OGraph.Dot.output_graph oc graph; close_out oc;
    Some graph

let resolve_packages t pkgs = resolve t (List.map Package.to_string pkgs)

let rev_deps t pkgs =
  let o = load_state t "rev_deps" in
  let pkgs = List.map opam_of_package pkgs in
  let pkgs = OpamPackage.Set.of_list pkgs in
  let universe = OpamState.universe o Depends in
  let pkgs =
    OpamSolver.reverse_dependencies
      ~build:true ~depopts:true ~installed:false ~unavailable:false
      universe pkgs
  in
  List.map package_of_opam pkgs

let plans t task f =
  (* FIXME: we don't really need to resolve the packages on the
     current switch. However it is currently not possibe to tweak the
     solver API to parametrize the environment in which the variables
     appearing in the opam file are resolved. *)
  let resolve_switch switch pkgs =
    eval_opam_config_env t;
    let i = Sys.command (Fmt.strf "opam switch %a" Switch.pp switch) in
    if i <> 0 then failwith "error while switching";
    resolve_packages { t with switch } pkgs
  in
  let resolve pkgs =
    let h = Host.detect () in
    if not (List.mem h (Task.hosts task)) then (
      debug "Task:%a do not need to be resolved on host %a."
        Id.pp (Task.id task) Host.pp h;
      ()
    ) else
      let switches = Task.switches task in
      List.iter (fun s ->
          match resolve_switch s pkgs with
          | None   ->
            (* FIXME: log the conflict properly *)
            ()
          | Some g -> f { g; h; s }
        ) switches
  in
  let rev_deps pkgs =
    match Task.rev_deps task with
    | `None          -> []
    | `All           -> List.map (fun d -> d :: pkgs) (rev_deps t pkgs)
    | `Packages deps -> List.map (fun d -> d :: pkgs) deps
  in
  let pkgs = Task.packages task in
  List.iter resolve (pkgs :: rev_deps pkgs)

module IdSet = struct
  include Set.Make(struct
      type t = Job.id
      let compare = Id.compare
    end)
end

let package_meta o pkg =
  debug "package_meta %a" Package.pp pkg;
  let nv = opam_of_package pkg in
  let mk name f x =
    let file = Filename.temp_file name "tmp" in
    f (OF.of_string file) x;
    let fd = Unix.openfile file [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o644 in
    let cstruct = Unix_cstruct.of_fd fd in
    Unix.close fd;
    cstruct
  in
  let mk_o name f = function None -> None | Some x -> Some (mk name f x) in
  let mk_file prefix file =
    let fd = Unix.openfile file [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o644 in
    let cstruct = Unix_cstruct.of_fd fd in
    Unix.close fd;
    OpamStd.String.remove_prefix ~prefix file, cstruct
  in
  let mk_files = function
    | None   -> None
    | Some d ->
      let prefix = OF.Dir.to_string d in
      let files = OS.rec_files prefix in
      Some (List.map (mk_file prefix) files)
  in
  let opam  = OpamState.opam o nv  |> mk "opam" OpamFile.OPAM.write in
  let descr = OpamState.descr o nv |> mk "descr" OpamFile.Descr.write in
  let url   = OpamState.url o nv   |> mk_o "url" OpamFile.URL.write in
  let files = OpamState.files o nv |> mk_files in
  Package.meta ~opam ~descr ?url ?files pkg

let package_of_action (a:OGraph.vertex) =
  let o = match a with
    | `Install target -> target
    | `Change (_, o, t) -> fail "change %s -> %s" (package_s o) (package_s t)
    | `Remove p | `Reinstall p | `Build p ->
      fail "Not expect delete/recompile %s" (package_s p)
  in
  package_of_opam o

module PMap = Map.Make(Package)

let atomic_jobs_of_plan t plan =
  let process_queue = Queue.create () in
  let add_stack = Stack.create () in
  OGraph.iter_vertex (fun v ->
      if OGraph.out_degree plan.g v = 0 then Queue.add v process_queue
    ) plan.g;
  while not (Queue.is_empty process_queue) do
    let v = Queue.pop process_queue in
    OGraph.iter_pred (fun pred -> Queue.add pred process_queue) plan.g v;
    Stack.push v add_stack;
  done;
  let id_map = ref PMap.empty in
  let j_lst = ref [] in
  while not (Stack.is_empty add_stack) do
    let v = Stack.pop add_stack in
    let p = package_of_action v in
    let inputs = OGraph.fold_pred (fun pred i ->
        let pred_pkg  = package_of_action pred in
        let pred_id   = PMap.find pred_pkg !id_map in
        pred_id :: i
      ) plan.g v []
    in
    let meta = package_meta t p in
    let job = Job.create ~inputs plan.h plan.s [meta] in
    id_map  := PMap.add p (Job.id job) !id_map;
    j_lst := job :: !j_lst
  done;
  !j_lst

let jobs_of_plan t plan =
  let actions = OGraph.fold_vertex (fun e l -> e :: l) plan.g [] in
  let pkgs = List.map (fun a -> package_meta t (package_of_action a)) actions in
  Job.create plan.h plan.s pkgs

let (@@++) x f =
  let open OpamProcess.Job.Op in
  x @@+ function
  | Some err -> raise err
  | None     -> f ()

let install t pkgs =
  let state = load_state t "install" in
  if List.length pkgs = 0 then ()
  else match pkgs with
    | [pkg] ->
      let nv = opam_of_package pkg in
      let job =
        let open OpamProcess.Job.Op in
        OpamAction.download_package state nv @@+ function
        | `Error err         -> failwith err
        | `Successful source ->
          OpamAction.build_package state source nv @@++ fun () ->
          OpamAction.install_package state nv @@++ fun () ->
          let { OT.installed; installed_roots; reinstall; _ } = state in
          let installed = OpamPackage.Set.add nv installed in
          OpamAction.update_switch_state
            state ~installed_roots ~reinstall ~installed
          |> fun _ -> exit 0
      in
      OpamProcess.Job.run job
    | pkgs ->
      let atoms = List.map atom_of_package pkgs in
      let add_to_root = None in
      let deps_only = false in
      OpamClient.SafeAPI.install
        atoms add_to_root ~deps_only ~upgrade:false

let remove t = function
  | []   -> ()
  | pkgs ->
    init_config t "remove";
    let atoms = List.map atom_of_package pkgs in
    OpamClient.SafeAPI.remove ~autoremove:true ~force:true atoms

(*

let remove_switch c =
  init ();
  let switch = OpamSwitch.of_string c in
  OpamSwitchCommand.switch ~quiet:false ~warning:false (OpamSwitch.of_string "system");
  OpamSwitchCommand.remove switch;
  Lwt.return_unit

*)

let switch_install t =
  init_config t "install_switch";
  let root = OSC.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string (Switch.to_string t.switch) in
  if OpamSwitch.Map.mem switch aliases then ()
  else
    let compiler = OpamCompiler.of_string (OpamSwitch.to_string switch) in
    OpamSwitchCommand.install ~quiet:false ~update_config:true switch compiler

let repo_clean t =
  let t = load_state t "repo_clean" in
  let repos = OpamState.sorted_repositories t in
  List.iter (fun r -> OpamRepositoryCommand.remove r.repo_name) repos

let repo_add t repos =
  init_config t "repo_add";
  let add_one_repo (name, address) =
    let address = Uri.to_string address in
    debug "repository: add %s %s" name address;
    let name = OpamRepositoryName.of_string name in
    let url = OpamUrl.of_string address in
    OpamRepositoryCommand.add name url ~priority:None
  in
  List.iter add_one_repo repos

let pin_clean t =
  let t = load_state t "pin_clean" in
  let pins = OpamState.pinned_packages t in
  let pkgs = OpamPackage.Set.elements pins in
  let names = List.map OpamPackage.name pkgs in
  let _ = OpamPinCommand.unpin ~state:t names in
  ()

let pin_add t pin =
  init_config t "pin_add";
  let add_one (pkg, target) =
    let name = OpamPackage.Name.of_string pkg in
    match target with
    | None -> (* --dev *)
      OpamClient.SafeAPI.PIN.pin ~edit:false ~action:false name None
    | Some target ->
      let target = Uri.to_string target in
      let pin_option = OpamTypesBase.pin_option_of_string ?kind:None target in
      let kind = OpamTypesBase.kind_of_pin_option pin_option in
      let () = assert (kind <> `rsync) in
      OpamClient.SafeAPI.PIN.pin
        ~edit:false ~action:false name (Some pin_option)
  in
  List.iter add_one pin

let update t =
  init_config t "update";
  OpamClient.SafeAPI.update ~repos_only:false ~dev_only:false []

let read_installed t =
  let t = load_state t "read_installed" in
  t.OT.installed
  |> OpamPackage.Set.elements
  |> List.map OpamPackage.to_string
  |> List.map Package.of_string

let write_installed t installed =
  let t = load_state t "write-installed" in
  let installed =
    installed
    |> List.map Package.to_string
    |> List.map OpamPackage.of_string
    |> OpamPackage.Set.of_list
  in
  let file = OpamPath.Switch.state t.OT.root t.OT.switch in
  let state = OpamFile.State.read file in
  let state = { state with OpamFile.State.installed } in
  OpamFile.State.write file state

let write_pinned t pinned =
  let t = load_state t "write-pinned" in
  let pinned =
    List.fold_left (fun acc (n, t) ->
        let t = match t with None -> failwith "TODO" | Some t -> t in
        OpamPackage.Name.Map.add
          (OpamPackage.Name.of_string n)
          (OpamTypesBase.pin_option_of_string (Uri.to_string t))
          acc
      ) OpamPackage.Name.Map.empty pinned
  in
  let file = OpamPath.Switch.state t.OT.root t.OT.switch in
  let state = OpamFile.State.read file in
  let state = { state with OpamFile.State.pinned } in
  OpamFile.State.write file state

let atomic_jobs t task f =
  let o = load_state t "atomic_jobs" in
  plans t task (fun plan ->
      let jobs =  atomic_jobs_of_plan o plan in
      List.iter f jobs
    )

let jobs t task f =
  let o = load_state t "jobs" in
  plans t task (fun plan ->
      let job = jobs_of_plan o plan in
      f job
    )
