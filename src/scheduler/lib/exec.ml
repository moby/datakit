(*
 * Copyright (c) 2016 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let (/) = Filename.concat
let debug fmt = Gol.debug ~section:"exec" fmt
let err fmt = Printf.ksprintf failwith fmt
let fail cmd fmt = Printf.ksprintf (fun e -> err "%s: %s" cmd e) fmt
let failv cmd fmt =
  Printf.ksprintf (fun e -> `Error (Printf.sprintf "%s: %s" cmd e)) fmt

let check_exit_status cmd (out, err) status =
  if out <> [] then debug "stdout:\n%s\n" (String.concat "\n" out);
  if err <> [] then debug "stderr:\n%s\n" (String.concat "\n" err);
  match status with
  | Unix.WEXITED 0   -> `Ok (out, err)
  | Unix.WEXITED i   -> failv cmd "exit %d" i
  | Unix.WSIGNALED i ->
    if i = Sys.sigkill then fail cmd "timeout" else fail cmd "signal %d" i
  | Unix.WSTOPPED i  -> fail cmd "stopped %d" i

let read_lines oc =
  let rec aux acc =
    let line =
      try Some (input_line oc)
      with End_of_file -> None
    in
    match line with
    | Some l -> aux (l :: acc)
    | None   -> List.rev acc
  in
  aux []

let spawn ?env cmd =
  let env = match env with None -> Unix.environment () | Some e -> e in
  debug "%s%!" cmd;
  let oc, ic, ec = Unix.open_process_full cmd env in
  let out = read_lines oc in
  let err = read_lines ec in
  let exit_status = Unix.close_process_full (oc, ic, ec) in
  check_exit_status cmd (out, err) exit_status

let read_outputs ?env fmt = Printf.ksprintf (spawn ?env) fmt

let exec ?env fmt =
  Printf.ksprintf (fun cmd ->
      match read_outputs ?env "%s" cmd with
      | `Ok _    -> ()
      | `Error e -> err "%s" e
    ) fmt

let read_stdout ?env fmt =
  Printf.ksprintf (fun cmd ->
      match read_outputs ?env "%s" cmd with
      | `Ok (out, _) -> out
      | `Error e     -> err "%s" e
    ) fmt

let exists s = Sys.command ("which " ^ s) = 0

let in_dir dir f =
  let pwd = Sys.getcwd () in
  let reset () = if pwd <> dir then Sys.chdir pwd in
  if pwd <> dir then Sys.chdir dir;
  try let r = f () in reset (); r
  with e -> reset (); raise e

let rmdir path = exec "rm -rf %s" path
let mkdir path = exec "mkdir -p %s" path
let touch file = exec "touch %s" file
let remove file = exec "rm -f %s" file
let unzip file =  exec "unzip %s" file

let docker fmt = Printf.ksprintf (read_stdout "docker %s") fmt

let docker_run ?(volumes=[]) fmt =
  Printf.ksprintf (fun cmd ->
      let volumes =
        List.map (fun (k, v) -> Printf.sprintf "-v %S:%S " k v) volumes
      in
      let volumes = String.concat " " volumes in
      docker  "run --rm %s%s" volumes cmd
    ) fmt

let docker_run_sh ?(volumes=[]) where lines =
  (* Note the lack of escaping here *)
  let lines =  String.concat ";" lines in
  docker_run ~volumes "%s sh -c '%s'" where lines

let write_file ~output file =
  let oc = open_out_bin file in
  output_string oc output;
  close_out oc

let exists_dir d = Sys.file_exists d && Sys.is_directory d

let real_path d =
  if not (Filename.is_relative d) then d
  else if exists_dir d then
    in_dir d Sys.getcwd
  else
    let dirname = Filename.dirname d in
    let basename = Filename.basename d in
    if exists_dir dirname then
      in_dir dirname (fun () -> Sys.getcwd () / basename)
    else
      failwith (d ^ " is not a valid local directory")

module Op = struct

  let (|=>) x f = match x with
    | `Error e -> `Error e
    | `Ok x    -> f x

  let (/) = Filename.concat

end
