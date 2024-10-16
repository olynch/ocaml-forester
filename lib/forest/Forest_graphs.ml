open Forester_prelude
open Forester_core

module Dl = Datalog_engine

module type S = sig

  val dl_db : Dl.db

  val register_iri : Iri.t -> unit
  val get_all_vertices : unit -> Vertex_set.t
  val get_rel : Query.mode -> Query.rel -> Forest_graph.t
  val add_edge : Query.rel -> source: Vertex.t -> target: Vertex.t -> unit
end

module Make () : S = struct

  let dl_db = Dl.db_create ()

  let all_vertices_ref : Vertex_set.t ref =
    ref Vertex_set.empty

  let get_all_vertices () = !all_vertices_ref

  let rel_graph_table : (Query.rel, Forest_graph.t) Hashtbl.t =
    Hashtbl.create 20

  let rel_preorder_table : (Query.rel, Forest_graph.t) Hashtbl.t =
    Hashtbl.create 20

  let get_graph rel =
    match Hashtbl.find_opt rel_graph_table rel with
    | None ->
      let gph = Forest_graph.create () in
      Hashtbl.add rel_graph_table rel gph;
      gph
    | Some gph -> gph

  let get_preorder rel =
    match Hashtbl.find_opt rel_preorder_table rel with
    | None ->
      let message = Format.asprintf "Compute reflexive-transitive closure of %s" rel in
      let@ () = Reporter.profile message in
      let gph = Forest_graph.transitive_closure ~reflexive: true @@ get_graph rel in
      Hashtbl.add rel_preorder_table rel gph;
      gph
    | Some gph -> gph

  let get_rel = function
    | Query.Edges -> get_graph
    | Query.Paths -> get_preorder

  let register_iri iri =
    let vtx : Vertex.t = Iri_vertex iri in
    Dl.db_add_fact dl_db @@ Dl.mk_literal (Dl.StringSymbol.make Builtin_relation.is_node) [Dl.mk_const (Vertex.pack vtx)];
    Hashtbl.clear rel_preorder_table;
    all_vertices_ref := Vertex_set.add vtx !all_vertices_ref;
    let@ gph = Seq.iter @~ Hashtbl.to_seq_values rel_graph_table in
    Forest_graph.add_vertex gph vtx

  let add_edge rel ~source ~target =
    Hashtbl.remove rel_preorder_table rel;
    let gph = get_graph rel in
    Forest_graph.add_edge gph source target;
    Dl.db_add_fact dl_db @@
      Dl.mk_literal (Dl.StringSymbol.make rel) [Dl.mk_const (Vertex.pack source); Dl.mk_const (Vertex.pack target)]
end
