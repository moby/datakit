(* Server configuration *)
let server_protocol = "tcp"
let server_address = "127.0.0.1:5640"

open Result             (* For [Ok] and [Error] on older versions of OCaml *)
open Lwt.Infix          (* For [>>=] *)

let src = Logs.Src.create "Client9p" ~doc:"9p client"
module Log9p = (val Logs.src_log src : Logs.LOG)
module Client9p = Client9p_unix.Make(Log9p);;

module DK = Datakit_client_9p.Make(Client9p);;

(* Chain operations together, returning early if we get an error *)
let ( >>*= ) x f =
  x >>= function
  | Ok x -> f x
  | Error _ as e -> Lwt.return e

let p = Datakit_path.of_string_exn
let root = Datakit_path.empty

let main () =
  (* Connect to 9p server *)
  Client9p.connect server_protocol server_address () >>*= fun conn ->
  (* Wrap it with the DataKit client *)
  let dk = DK.connect conn in
  (* Make a test branch *)
  DK.branch dk "test" >>*= fun test_branch ->
  DK.Branch.with_transaction test_branch (fun t ->
      let contents = Cstruct.of_string "This is a test" in
      DK.Transaction.create_file t ~dir:root "README" contents >>*= fun () ->
      DK.Transaction.commit t ~message:"My first commit"
    )
  >>*= fun () ->
  (* See what branches we've got *)
  DK.branches dk >>*= fun branches ->
  Fmt.pr "Branches: %a@." Fmt.(Dump.list string) branches;
  (* Look at the head commit *)
  DK.Branch.head test_branch >>*= function
  | None -> failwith "Branch no longer exists!"
  | Some head ->
  DK.Commit.message head >>*= fun msg ->
  Fmt.pr "Head (commit %s) has message: %S@." (DK.Commit.id head) msg;
  let tree = DK.Commit.tree head in
  DK.Tree.read_dir tree root >>*= fun items ->
  Fmt.pr "Items in the root directory: %a@." Fmt.(Dump.list string) items;
  DK.Tree.read_file tree (p "README") >>*= fun data ->
  Fmt.pr "Contents of README: %S@." (Cstruct.to_string data);
  Lwt.return (Ok ())

let () =
  match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Fmt.epr "Test program failed: %s@." msg
