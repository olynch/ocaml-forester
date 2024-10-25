open Forester_prelude
open Forester_core
open Forester_compiler

module Gph = Graph.Imperative.Digraph.Concrete(String)
include Gph

module Topo = Graph.Topological.Make(Gph)
let topo_fold = Topo.fold

let build (trees : Code.tree list) =
  let import_graph = Gph.create () in
  let rec analyse_tree roots addr_opt code =
    let roots =
      Option.fold
        ~none: roots
        ~some: (fun x -> x :: roots)
        addr_opt
    in
    analyse_code roots code;
    let@ addr = Option.iter @~ addr_opt in
    Gph.add_vertex import_graph addr
  and analyse_code roots (code : Code.t) =
    List.iter (analyse_node roots) code
  and analyse_node roots (node : Code.node Asai.Range.located) =
    match node.value with
    | Import (_, dep) ->
      let@ addr = List.iter @~ roots in
      Gph.add_edge import_graph dep addr
    | Subtree (addr, code) ->
      analyse_tree roots addr code
    | Scope code | Namespace (_, code) | Group (_, code) | Math (_, code) | Let (_, _, code) | Fun (_, code) | Def (_, _, code) ->
      analyse_code roots code
    | Object { methods; _ } | Patch { methods; _ } ->
      let@ _, code = List.iter @~ methods in
      analyse_code roots code
    | Dx_prop (rel, args) ->
      analyse_code roots rel;
      List.iter (analyse_code roots) args
    | Dx_sequent (concl, premises) ->
      analyse_code roots concl;
      List.iter (analyse_code roots) premises
    | Dx_query (_, positives, negatives) ->
      List.iter (analyse_code roots) positives;
      List.iter (analyse_code roots) negatives
    | Text _ | Hash_ident _ | Xml_ident _ | Verbatim _ | Ident _ | Open _ | Put _ | Default _ | Get _ | Decl_xmlns _ | Call _ | Alloc _ | Dx_var _ | Dx_const_content _ | Dx_const_iri _ -> ()
  in
  begin
    let@ tree = List.iter @~ trees in
    analyse_tree [] tree.addr tree.code
  end;
  import_graph
