module Metrics = struct
  let namespace = "DataKitCI"
  let subsystem = "term"

  let evals =
    let help = "Number of term evaluations" in
    CI_prometheus.Counter.v ~help ~namespace ~subsystem "evals_total"
end

module Context = struct
  (* The context in which a term is evaluated. We create a fresh context each time
     the term is evaluated. *)
  type t = {
    github : CI_github_hooks.Snapshot.t;
    job_id : CI_s.job_id;
    mutable recalc : unit -> unit;              (* Call this to schedule a recalculation. *)
    dk : unit -> CI_utils.DK.t Lwt.t;
  }

  let dk     t = t.dk
  let github t = t.github
  let job_id t = t.job_id

  let disable t =
    t.recalc <- (fun () -> CI_utils.Log.debug (fun f -> f "recalculate called, but term is finished"))

  let watch t ready =
    (* When [ready] is done, call the then-current [recalc] function. *)
    Lwt.on_termination ready (fun () -> t.recalc ())
end

include CI_eval.Make(Context)

open Infix

let dk     = value Context.dk
let github = value Context.github
let job_id = value Context.job_id

let pp_target f = function
  | `PR pr -> CI_github_hooks.PR.dump f pr
  | `Ref r -> CI_github_hooks.Ref.dump f r

let github_target id =
  github >>= fun gh ->
  of_lwt_quick (CI_github_hooks.Snapshot.find id gh) >>= function
  | None -> fail "Target %a does not exist" CI_target.Full.pp id
  | Some x -> return x

let head id =
  github_target id >|= CI_github_hooks.Target.head

let ref_head project_id ref_name =
  match Datakit_path.of_string ref_name with
  | Error msg -> fail "Invalid ref name %S: %s" ref_name msg
  | Ok ref_path -> head (project_id, `Ref ref_path)

let branch_head project_id branch =
  ref_head project_id ("heads/" ^ branch)

let tag project_id tag =
  ref_head project_id ("tags/" ^ tag)

let ci_state fn ci t =
  head t >>= fun commit ->
  let gh_ci = CI_github_hooks.CI.of_string ci in
  let state = CI_github_hooks.Commit.state gh_ci commit in
  of_lwt_quick (fn state)

let ci_status     = ci_state CI_github_hooks.Commit_state.status
let ci_descr      = ci_state CI_github_hooks.Commit_state.descr
let ci_target_url = ci_state CI_github_hooks.Commit_state.target_url

let pp_opt_descr f = function
  | None -> ()
  | Some descr -> Fmt.pf f " (%s)" descr

let ci_success_target_url ci target =
  ci_status ci target >>= function
  | None -> pending "Waiting for %s status to appear" ci
  | Some `Pending -> ci_descr ci target >>= pending "Waiting for %s to complete%a" ci pp_opt_descr
  | Some `Failure -> ci_descr ci target >>= fail "%s failed%a" ci pp_opt_descr
  | Some `Error   -> ci_descr ci target >>= fail "%s errored%a" ci pp_opt_descr
  | Some `Success ->
    ci_state CI_github_hooks.Commit_state.target_url ci target >>= function
    | None -> fail "%s succeeded, but has no URL!" ci
    | Some url -> return url

let run ~snapshot ~job_id ~recalc ~dk term =
  CI_prometheus.Counter.inc_one Metrics.evals;
  let ctx = { Context.recalc; job_id; dk; github = snapshot } in
  (run ctx term, fun () -> Context.disable ctx)
