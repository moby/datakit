open Astring
open Lwt.Infix
open Result

let () = Printexc.record_backtrace true

let default d = function
  | None -> d
  | Some x -> x

let p = function
  | "" -> Datakit_client.Path.empty
  | path -> Datakit_client.Path.of_string_exn path

let ( / ) = Datakit_client.Path.Infix.( / )

let v = Cstruct.of_string

let ( ++ ) = Int64.add

let ok x = Lwt.return (Ok x)

let ( >>!= ) x f =
  match x with
  | Ok y -> f y
  | Error vfs_error ->
    Alcotest.fail ("Vfs.error: " ^ Fmt.to_to_string Vfs.Error.pp vfs_error)

let ( >>*= ) x f = x >>= function
  | Ok y -> f y
  | Error (`Msg msg) -> Alcotest.fail ("Msg: " ^ msg)


let () =
  let fd_stderr = Unix.descr_of_out_channel stderr in
  let real_stderr = Unix.dup fd_stderr in
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := (fun ex ->
      Unix.dup2 real_stderr fd_stderr;
      Printf.eprintf "\nasync_exception_hook:\n%!";
      old_hook ex
    )

module Test_flow = struct
  type error = {zero : 'a. 'a}
  let pp_error ppf _ = Fmt.string ppf "<0>"
  type write_error = Mirage_flow.write_error
  let pp_write_error = Mirage_flow.pp_write_error
  type buffer = Cstruct.t
  type 'a io = 'a Lwt.t
  let error_message e = e.zero

  type flow = {
    from_remote : Cstruct.t Lwt_mvar.t;
    to_remote : Cstruct.t Lwt_mvar.t;
  }

  let create () =
    let a = Lwt_mvar.create_empty () in
    let b = Lwt_mvar.create_empty () in
    let flow1 = { from_remote = a; to_remote = b } in
    let flow2 = { from_remote = b; to_remote = a } in
    (flow1, flow2)


  let ok x = Ok x
  let close _t = Lwt.return_unit
  let write1 t buf = Lwt_mvar.put t.to_remote buf
  let write t buf = write1 t buf >|= ok
  let writev t bufv = Lwt_list.iter_s (write1 t) bufv >|= ok
  let read t = Lwt_mvar.take t.from_remote >|= fun x -> Ok (`Data x)
end

let reporter () =
  let pad n x =
    if String.length x > n then x
    else x ^ String.v ~len:(n - String.length x) (fun _ -> ' ')
  in
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf ("%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string) (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt ->
    with_stamp header tags k fmt
  in
  { Logs.report = report }

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.(set_level (Some Debug));
  Logs.set_reporter (reporter ());
  ()

module Maker = Irmin_git.Mem.Make(Datakit_io.IO)(Datakit_io.Zlib)
module Store = Ivfs_tree.Make(Maker)
module RW = Ivfs_rw.Make(Store)
module Filesystem = Ivfs.Make(Store)

type history_node = {
  id : string;
  msg : string;
  parents : history_node list;
}

let compare_history_node a b =
  match compare a.msg b.msg with
  | 0 -> compare a.id b.id
  | x -> x

let quiet_9p src9p =
  Logs.Src.set_level src9p (Some Logs.Info);
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "fs9p" then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let quiet_git () =
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "git.value" || Logs.Src.name src = "git.memory"
      then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let quiet_irmin () =
  let srcs = Logs.Src.list () in
  List.iter (fun src ->
      if Logs.Src.name src = "irmin.bc"
      || Logs.Src.name src = "irmin.commit"
      || Logs.Src.name src = "irmin.node"
      then Logs.Src.set_level src (Some Logs.Info)
    ) srcs

let split path =
  match Irmin.Path.String_list.of_string path with
  | Error _ -> assert false
  | Ok path ->
    match Irmin.Path.String_list.rdecons path with
    | None -> assert false
    | Some (x, y) -> x, y

let config = Irmin_mem.config ()

let vfs_error = Alcotest.of_pp Vfs.Error.pp
let vfs_result ok = Alcotest.result ok vfs_error

let rec pp_history fmt {id; msg; parents} =
  Format.fprintf fmt "@[<v2>%s (%s)@\n%a@]"
    id msg (Format.pp_print_list pp_history) parents

let reject (type v) =
  let module T = struct
    type t = v
    let pp fmt _ = Fmt.string fmt "reject-all"
    let equal _ _ = false
  end in
  (module T : Alcotest.TESTABLE with type t = v)
