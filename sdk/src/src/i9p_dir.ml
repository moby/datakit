open I9p_misc
open Result
open Lwt.Infix

let err_ro = Lwt.return (error "Directory is read-only")
let err_already_exists = Lwt.return (error "Already exists")
let err_dir_only = Lwt.return (error "Can only contain directories")

let ok x = Lwt.return (Ok x)
let enoent = Lwt.return I9p_error.enoent

module InodeMap = Map.Make(String)

module Make(Inode : I9p_inode.S) = struct
  type t = Inode.dir

  let fixed items =
    object
      method create _ = err_ro
      method mkdir _ = err_ro
      method remove = err_ro
      method ls = ok items
      method lookup name =
        let rec aux = function
          | [] -> I9p_error.enoent
          | x :: _ when Inode.basename x = name -> Ok x
          | _ :: xs -> aux xs in
        Lwt.return (aux items)
      method rename _ _ = err_ro
    end

  let of_map_ref m =
    object
      method create _ = err_ro
      method mkdir _ = err_ro
      method remove = err_ro
      method ls = ok (InodeMap.bindings !m |> List.map snd)
      method lookup name =
        try ok (InodeMap.find name !m)
        with Not_found -> enoent
      method rename _ _ = err_ro
    end

  let directories make contents =
    let lock = Lwt_mutex.create () in
    object
      val mutable next = 0L

      val mutable items =
        List.fold_left
          (fun acc inode -> acc |> InodeMap.add (Inode.basename inode) inode)
          InodeMap.empty contents

      method ls = ok (InodeMap.bindings items |> List.map snd)

      method lookup name =
        try ok (InodeMap.find name items)
        with Not_found -> enoent

      method mkdir name =
        Lwt_mutex.with_lock lock (fun () ->
            if InodeMap.mem name items then err_already_exists
            else (
              let remover = lazy (
                Lwt_mutex.with_lock lock (fun () ->
                    items <- items |> InodeMap.remove name;
                    Lwt.return_unit
                  )
              ) in
              make ~remover name >|= function
              | Ok dir ->
                if Lazy.is_val remover then I9p_error.enoent else (
                  let inode = Inode.of_dir name dir in
                  items <- items |> InodeMap.add name inode;
                  Ok inode
                )
              | Error _ as e -> e
            ))

      method remove = err_ro
      method create _name = err_dir_only
      method rename _ _ = err_ro    (* TODO *)
    end

  let rename_ro _ _ = err_ro

  let read_only ~ls ~lookup ~remove ?(rename=rename_ro) () =
    object
      method ls = ls ()
      method create _ = err_ro
      method mkdir _ = err_ro
      method remove = remove ()
      method lookup = lookup
      method rename = rename
    end

  let dir_only ~ls ~mkdir ~lookup ~remove ?(rename=rename_ro) () =
    object
      method ls = ls ()
      method create _ = err_dir_only
      method mkdir = mkdir
      method remove = remove ()
      method lookup = lookup
      method rename = rename
    end

  let read_write ~ls ~create ~mkdir ~lookup ~remove =
    object
      method ls = ls ()
      method create = create
      method mkdir = mkdir
      method remove = remove ()
      method lookup = lookup
      method rename _ _ = err_ro    (* TODO *)
    end
end
