open Forester_core

module Gph =
struct
  module G = Graph.Imperative.Digraph.Concrete (Addr)
  include G
  include Graph.Oper.I (G)

  let safe_succ g x =
    if mem_vertex g x then succ g x else []

  let safe_pred g x =
    if mem_vertex g x then pred g x else []
end

module Topo = Graph.Topological.Make (Gph)

let build_import_graph (trees : Code.tree list) =
  let import_graph = Gph.create () in

  let rec analyse_tree roots (tree : Code.tree) =
    let roots = Option.fold ~none:roots ~some:(fun x -> x :: roots) tree.addr in
    begin
      tree.addr |> Option.iter @@ fun addr ->
      Gph.add_vertex import_graph @@ User_addr addr
    end;
    tree.code |> List.iter @@ fun node ->
    match Asai.Range.(node.value) with
    | Code.Import (_, dep) ->
      roots |> List.iter @@ fun addr -> Gph.add_edge import_graph (User_addr dep) (User_addr addr)
    | Code.Subtree (addr, code) ->
      analyse_tree roots @@ Code.{tree with addr; code}
    | _ -> ()
  in

  trees |> List.iter (analyse_tree []);
  import_graph
