open Forester_prelude
open Forester_core
open Forester_compiler

module Gph = Graph.Imperative.Digraph.Concrete (Addr)

include Gph

module Topo = Graph.Topological.Make (Gph)
let topo_fold = Topo.fold

let build (trees : Code.tree list) =
  let import_graph = Gph.create () in

  let rec analyse_tree roots (tree : Code.tree) =
    let roots = Option.fold ~none:roots ~some:(fun x -> x :: roots) tree.addr in
    begin
      let@ addr = Option.iter @~ tree.addr in
      Gph.add_vertex import_graph @@ Addr.user_addr addr
    end;

    let@ node = List.iter @~ tree.code in
    match Asai.Range.(node.value) with
    | Code.Import (_, dep) ->
      let@ addr = List.iter @~ roots in
      Gph.add_edge import_graph (Addr.user_addr dep) (Addr.user_addr addr)
    | Code.Subtree (addr, code) ->
      analyse_tree roots @@ Code.{tree with addr; code}
    | _ -> ()
  in

  trees |> List.iter (analyse_tree []);
  import_graph
