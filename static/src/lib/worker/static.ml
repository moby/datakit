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

type t = { cache: string }

let create ~cache = { cache }

module V = struct

  type t = Source of Source.t | Cmd of Cmd.t

  let compare x y = match x, y with
    | Source x, Source y -> Source.compare x y
    | Cmd x   , Cmd y    -> Cmd.compare x y
    | Source _, _        -> 1
    | Cmd _   , _        -> -1

  let equal x y = match x, y with
    | Source x, Source y -> Source.equal x y
    | Cmd    x, Cmd y    -> Cmd.equal x y
    | _ -> false

  let hash = function
    | Source s -> Source.hash s
    | Cmd c    -> Cmd.hash c

end

module G = Graph.Imperative.Digraph.Concrete(V)
module GT = Graph.Topological.Make_stable(G)
module J = Graph.Imperative.Digraph.Concrete(Job)

let jobs t task f =
  let flow = Task.flow task in
  let flow = Flow.fetch ~dst:t.cache flow in
  let size = List.length (Flow.deps flow) * 2 (* random guess *) in
  let g = G.create ~size () in
  let j = J.create ~size () in
  let jobs = Hashtbl.create size in
  let job v = try Some (Hashtbl.find jobs v) with Not_found -> None in
  let add_job v j = Hashtbl.add jobs v j in
  List.iter (fun (src, dst) ->
      let src = V.Source src and dst = V.Cmd dst in
      G.add_vertex g src;
      G.add_vertex g dst;
      G.add_edge g src dst;
    ) (Flow.inputs flow);
  List.iter (fun (src, dst) ->
      let src = V.Cmd src and dst = V.Cmd dst in
      G.add_vertex g src;
      G.add_vertex g dst;
      G.add_edge g src dst;
    ) (Flow.deps flow);
  GT.iter (fun cmd ->
      let inputs = try G.pred g cmd with Not_found -> [] in
      let preds =
          List.fold_left (fun acc i ->
              match job i with None -> acc | Some j -> j :: acc
            ) [] inputs
      in
      let inputs = List.map Job.id preds in
      let job = match cmd with
        | V.Source _ -> Job.create ~inputs Cmd.nope
        | V.Cmd x    -> Job.create ~inputs x
      in
      add_job cmd job;
      J.add_vertex j job;
      List.iter (fun i -> J.add_edge j i @@ job) preds;
    ) g;
  Hashtbl.iter (fun _ v -> f v) jobs
