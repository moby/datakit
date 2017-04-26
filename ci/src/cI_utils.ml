open Result
open Lwt.Infix

let src9p = Logs.Src.create "Client9p" ~doc:"9p client"
module Log9p = (val Logs.src_log src9p : Logs.LOG)
module Client9p = Client9p_unix.Make(Log9p)
module DK = Datakit_client_9p.Make(Client9p)

let src = Logs.Src.create "datakit-ci" ~doc:"DataKit-based CI system"
module Log = (val Logs.src_log src : Logs.LOG)

(* Hold this to prevent other threads changing the current directory. *)
let chdir_lock = Lwt_mutex.create ()

let ok x = Lwt.return (Ok x)

module Infix = struct
  (* Chain operations together, returning early if we get an error *)
  let ( >>*= ) x f =
    x >>= function
    | Ok x -> f x
    | Error e -> Lwt.fail (Failure (Fmt.to_to_string DK.pp_error e))

  let ( >|*= ) x f =
    x >>*= fun x -> Lwt.return (f x)
end

let return_error fmt =
  fmt |> Fmt.kstrf @@ fun msg -> Lwt.return (Error msg)

let failf fmt =
  Fmt.kstrf failwith fmt

let pp_exn f = function
  | Failure msg -> Fmt.string f msg
  | ex -> Fmt.string f (Printexc.to_string ex)

let pp_duration f d =
  if d < 120. then Fmt.pf f "%.2f seconds" d
  else Fmt.pf f "%.2f minutes" (d /. 60.)

let with_switch fn =
  let switch = Lwt_switch.create () in
  Lwt.finalize
    (fun () -> fn switch)
    (fun () -> Lwt_switch.turn_off switch)

let with_child_switch ?switch fn =
  match switch with
  | None -> with_switch fn
  | Some parent ->
    with_switch (fun child ->
        Lwt_switch.add_hook_or_exec (Some parent) (fun () -> Lwt_switch.turn_off child) >>= fun () ->
        fn child
      )

let with_timeout ?switch duration fn =
  with_child_switch ?switch @@ fun switch ->
  let timeout = Lwt_unix.sleep duration in
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt.cancel timeout; Lwt.return ());
  Lwt.on_success timeout (fun () ->
      Log.info (fun f -> f "Timeout (of %a) expired" pp_duration duration);
      Lwt.async (fun () -> Lwt_switch.turn_off switch)
    );
  Lwt.catch
    (fun () -> fn switch)
    (fun ex ->
       match Lwt.state timeout with
       | Lwt.Return () -> failf "Exceeded timeout of %a" pp_duration duration
       | _ -> Lwt.fail ex
    )
let default d = function
  | None -> d
  | Some x -> x

let abs_path x =
  if Filename.is_relative x then Filename.concat (Sys.getcwd ()) x else x

let ensure_dir ~mode path =
  let rec loop path =
    match Unix.stat path with
    | info ->
      if info.Unix.st_kind = Unix.S_DIR then ()
      else failf "Not a directory: %s" path
    | exception _ ->
      let parent = Filename.dirname path in
      assert (path <> parent);
      loop parent;
      Unix.mkdir path mode
  in
  loop path

let make_tmp_dir ?(prefix="tmp-") ?(mode=0o700) parent =
  let rec mktmp = function
    | 0 -> failf "Failed to generate temporary directroy name!"
    | n ->
      try
        let tmppath = Printf.sprintf "%s/%s%x" parent prefix (Random.int 0x3fffffff) in
        Unix.mkdir tmppath mode;
        tmppath
      with Unix.Unix_error (Unix.EEXIST, _, _) -> mktmp (n - 1) in
  mktmp 10

let rm_f_tree root =
  Log.debug (fun f -> f "rm -rf %S" root);
  let rec rmtree path =
    let info = Unix.lstat path in
    match info.Unix.st_kind with
    | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK | Unix.S_FIFO ->
      Unix.unlink path
    | Unix.S_DIR ->
      Unix.chmod path 0o700;
      Sys.readdir path |> Array.iter (fun leaf -> rmtree (Filename.concat path leaf));
      Unix.rmdir path
  in
  rmtree root

let with_tmpdir ?prefix ?mode fn =
  let tmpdir = make_tmp_dir ?prefix ?mode (Filename.get_temp_dir_name ()) in
  Lwt.finalize
    (fun () -> fn tmpdir)
    (fun () -> rm_f_tree tmpdir; Lwt.return ())

let ls path =
  let rec read_files acc fd =
    Lwt.try_bind
      (fun () -> Lwt_unix.readdir fd)
      (fun leaf -> read_files (leaf :: acc) fd)
      (function
        | End_of_file -> Lwt.return acc
        | ex -> Lwt.fail ex
      )
  in
  Lwt_unix.opendir path >>= fun fd ->
  Lwt.finalize
    (fun () -> read_files [] fd)
    (fun () -> Lwt_unix.closedir fd)

let cancel_when_off switch fn =
  let th = fn () in
  Lwt_switch.add_hook_or_exec
    (Some switch)
    (fun () -> Lwt.cancel th; Lwt.return ())
  >>= fun () ->
  th
