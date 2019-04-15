open Lwt.Infix
module Client = Datakit_client_git

let test_db = "_build/test-git"

let run fn =
  Lwt_main.run
    ( Git_unix.FS.create ~root:test_db () >>= fun db ->
      Git_unix.FS.reset db >>= fun () ->
      Git_unix.FS.clear ();
      Client.connect ~author:"datakit" test_db >>= fn )

(* FIXME(samoht): re-add server-side tests *)

module C = Test_client.Make (struct
  include Client

  let run = run
end)

let () = Alcotest.run "datakit-git" [ ("client", C.test_set) ]
