open Datakit_github

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
  | `PR pr -> PR.pp f pr
  | `Ref r -> Ref.pp f r

let github_target id =
  github >>= fun gh ->
  of_lwt_quick (CI_github_hooks.Snapshot.target gh id) >>= function
  | None -> fail "Target %a does not exist" CI_target.pp id
  | Some x -> return x

let head id =
  github_target id >|= CI_target.head

let ref_head repo ref_name =
  match Datakit_path.of_string ref_name with
  | Error msg -> fail "Invalid ref name %S: %s" ref_name msg
  | Ok ref_path -> head @@ `Ref (repo, Datakit_path.unwrap ref_path)

let branch_head repo branch = ref_head repo ("heads/" ^ branch)

let tag repo tag = ref_head repo ("tags/" ^ tag)

let ci_state fn ci t =
  head t >>= fun c ->
  github >>= fun s ->
  of_lwt_quick (CI_github_hooks.Snapshot.status s (c, ci)) >|= function
  | Some s -> fn s
  | None   -> None

let ci_status     = ci_state (fun s -> Some (Status.state s))
let ci_descr      = ci_state Status.description
let ci_target_url = ci_state Status.url

let pp_opt_descr f = function
  | None -> ()
  | Some descr -> Fmt.pf f " (%s)" descr

let ci_success_target_url ci target =
  ci_status ci target >>= function
  | None -> pending "Waiting for %a status to appear" Ref.pp_name ci
  | Some `Pending -> ci_descr ci target >>= pending "Waiting for %a to complete%a" Ref.pp_name ci pp_opt_descr
  | Some `Failure -> ci_descr ci target >>= fail "%a failed%a" Ref.pp_name ci pp_opt_descr
  | Some `Error   -> ci_descr ci target >>= fail "%a errored%a" Ref.pp_name ci pp_opt_descr
  | Some `Success ->
    ci_state Status.url ci target >>= function
    | None -> fail "%a succeeded, but has no URL!" Ref.pp_name ci
    | Some url -> return url

let run ~snapshot ~job_id ~recalc ~dk term =
  CI_prometheus.Counter.inc_one Metrics.evals;
  let ctx = { Context.recalc; job_id; dk; github = snapshot } in
  (run ctx term, fun () -> Context.disable ctx)
