module Context = struct
  (* The context in which a term is evaluated. We create a fresh context each time
     the term is evaluated. *)
  type t = {
    github : CI_github_hooks.snapshot;
    target : [`PR of CI_github_hooks.PR.t | `Ref of CI_github_hooks.Ref.t];
    mutable recalc : unit -> unit;              (* Call this to schedule a recalculation. *)
    dk : unit -> CI_utils.DK.t Lwt.t;
  }

  let head   t = t.target
  let dk     t = t.dk
  let github t = t.github

  let disable t =
    t.recalc <- (fun () -> CI_utils.Log.debug (fun f -> f "recalculate called, but term is finished"))

  let watch t ready =
    (* When [ready] is done, call the then-current [recalc] function. *)
    Lwt.on_termination ready (fun () -> t.recalc ())
end

include CI_eval.Make(Context)

open Infix

let target = value Context.head
let dk     = value Context.dk
let github = value Context.github

let pp_target f = function
  | `PR pr -> CI_github_hooks.PR.dump f pr
  | `Ref r -> CI_github_hooks.Ref.dump f r

let head =
  target >|= function
  | `PR pr -> CI_github_hooks.PR.head pr
  | `Ref r -> CI_github_hooks.Ref.head r

let ref_head project_id ref_name =
  match Datakit_path.of_string ref_name with
  | Error msg -> fail "Invalid ref name %S: %s" ref_name msg
  | Ok ref_path ->
  github >>= fun snapshot ->
  of_lwt_quick (CI_github_hooks.project snapshot project_id) >>= fun (_, refs) ->
  match List.find (fun r -> Datakit_path.compare (CI_github_hooks.Ref.name r) ref_path = 0) refs with
  | exception Not_found -> fail "Ref %a/%a does not exist" CI_projectID.pp project_id Datakit_path.pp ref_path
  | r -> return (CI_github_hooks.Ref.head r )

let branch_head project_id branch =
  ref_head project_id ("heads/" ^ branch)

let tag project_id tag =
  ref_head project_id ("tags/" ^ tag)

let ci_state fn ci =
  head >>= fun commit ->
  let gh_ci = CI_github_hooks.CI.of_string ci in
  let state = CI_github_hooks.Commit.state gh_ci commit in
  of_lwt_quick (fn state)

let ci_status     = ci_state CI_github_hooks.Commit_state.status
let ci_descr      = ci_state CI_github_hooks.Commit_state.descr
let ci_target_url = ci_state CI_github_hooks.Commit_state.target_url

let pp_opt_descr f = function
  | None -> ()
  | Some descr -> Fmt.pf f " (%s)" descr

let ci_success_target_url ci =
  ci_status ci >>= function
  | None -> pending "Waiting for %s status to appear" ci
  | Some `Pending -> ci_descr ci >>= pending "Waiting for %s to complete%a" ci pp_opt_descr
  | Some `Failure -> ci_descr ci >>= fail "%s failed%a" ci pp_opt_descr
  | Some `Error   -> ci_descr ci >>= fail "%s errored%a" ci pp_opt_descr
  | Some `Success ->
    ci_state CI_github_hooks.Commit_state.target_url ci >>= function
    | None -> fail "%s succeeded, but has no URL!" ci
    | Some url -> return url

let run ~snapshot ~target ~recalc ~dk term =
  let ctx = { Context.target; recalc; dk; github = snapshot } in
  (run ctx term, fun () -> Context.disable ctx)
