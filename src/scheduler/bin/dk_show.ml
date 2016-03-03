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



open Cmdliner
open Lwt.Infix
include Dk_common

let id_t: [`Id] Id.t Arg.converter =
  let parse x = `Ok (Id.of_string `Id x) in
  let print = Id.pp in
  parse, print

let id =
  let doc = "Object identifier" in
  Arg.(value & opt (some id_t) None & info ["id"] ~doc ~docv:"HASH")

let one ?(pad=0) pp pp_status ppf (id, v) =
  match v with
  | None   ->
    Fmt.(styled `Cyan Id.pp ppf) id;
    Fmt.(pf ppf " --\n")
  | Some (v, s) ->
    Fmt.box ~indent:2 pp ppf v;
    match s with
    | None   -> ()
    | Some s ->
      let pad = String.make pad ' ' in
      let underline = Fmt.(styled `Underline string) in
      Fmt.pf ppf "%a%s: %a\n" underline "status" pad pp_status s

let block ?pad title pp pp_status b =
  let bar ppf = Fmt.pf ppf "\n=== %s ===\n\n" in
  Fmt.(styled `Yellow bar stdout) title;
  match b with
  | [] -> Fmt.(string stdout) "None!\n\n"
  | _  ->
    Fmt.(list (one ?pad pp pp_status) stdout) b;
    Fmt.(cut stdout) ()

let find (get, status) t x =
  Lwt.catch
    (fun () ->
       get t x    >>= fun y ->
       status t x >|= fun s ->
       x, Some (y, s))
    (function Invalid_argument _ -> Lwt.return (x, None) | e -> Lwt.fail e)

let kind =
  let doc = "Select the kind of objects to show." in
  let choices = [
    "tasks"  , `Task;
    "workers", `Worker;
    "jobs"   , `Job;
  ] in
  Arg.(value & opt (enum choices) `Task & info ["k";"kind"] ~docv:"KIND" ~doc)

let cast k id = Id.of_string k (Id.to_string id)

let list_tasks store =
  Store.Task.list store >>= fun task_ids ->
  Lwt_list.map_p (find Store.Task.(get, status) store) task_ids
  >|= fun tasks ->
  block "Tasks" Task.pp Task.pp_status tasks ~pad:2

let list_jobs store =
  Store.Job.list store >>= fun job_ids ->
  Lwt_list.map_p (find Store.Job.(get, status) store) job_ids
  >|= fun jobs ->
  block "Jobs" Job.pp Job.pp_status jobs ~pad:2

let list_workers store =
  Store.Worker.list store >>= fun worker_ids ->
  Lwt_list.map_p (find Store.Worker.(get, status) store) worker_ids
  >|= fun workers ->
  block "Workers" Worker.pp Worker.pp_status workers

let task_id store id =
  find Store.Task.(get, status) store (cast `Task id) >|= function
  | _, None -> None
  | x       -> Some (fun () -> one Task.pp Task.pp_status Fmt.stdout x ~pad:2)

let job_id store id =
  find Store.Job.(get, status) store (cast `Job id) >|= function
  | _, None -> None
  | x       -> Some (fun () -> one Job.pp Job.pp_status Fmt.stdout x ~pad:2)

let worker_id store id =
  find Store.Worker.(get, status) store (cast `Worker id) >|= function
  | _, None -> None
  | x       -> Some (fun () -> one Worker.pp Worker.pp_status Fmt.stdout x)

let (>>) x y =
  x >>= function
  | Some f -> Lwt.return (f ())
  | None   -> y ()

let invalid_id id = err "invalid id: %a" Id.pp id; exit 1

let main =
  let list store kind id =
    Lwt_main.run begin
      store >>= fun store ->
      match id with
      | Some id ->
        task_id store id >> fun () ->
        job_id store id >> fun () ->
        worker_id store id >> fun () ->
        invalid_id id
      | None ->
        match kind with
        | `Task   -> list_tasks store
        | `Job    -> list_jobs store
        | `Worker -> list_workers store
    end
  in
  Term.(global list $ store $ kind $ id),
  term_info ~doc:"Show datakit objects" "dk-show"

let () =
  match Term.eval main with `Error _ -> exit 1 | _ -> exit 0
