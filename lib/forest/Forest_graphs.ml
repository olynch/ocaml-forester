open Forester_prelude
open Forester_core

module type S = sig
  val register_iri : Iri.t -> unit
  val get_all_vertices : unit -> Vertex_set.t
  val get_pred : Query.Pred.t -> Vertex_set.t
  val get_rel : Query.mode -> Query.Rel.t -> Forest_graph.t
  val add_edge : Query.Rel.t -> source: Vertex.t -> target: Vertex.t -> unit
  val add_vertex : Query.Pred.t -> Vertex.t -> unit
end

module Make () : S = struct
  let all_vertices_ref : Vertex_set.t ref =
    ref Vertex_set.empty

  let get_all_vertices () = !all_vertices_ref

  let pred_to_set : (Query.Pred.t, Vertex_set.t) Hashtbl.t =
    Hashtbl.create 20

  let rel_to_graph : (Query.Rel.t, Forest_graph.t) Hashtbl.t =
    Hashtbl.create 20

  let rel_to_preorder : (Query.Rel.t, Forest_graph.t) Hashtbl.t =
    Hashtbl.create 20

  let get_graph rel =
    match Hashtbl.find_opt rel_to_graph rel with
    | None ->
      let gph = Forest_graph.create () in
      Hashtbl.add rel_to_graph rel gph;
      gph
    | Some gph -> gph

  let get_preorder rel =
    match Hashtbl.find_opt rel_to_preorder rel with
    | None ->
      let message = Format.asprintf "Compute reflexive-transitive closure of %s" rel in
      let@ () = Reporter.profile message in
      let gph = Forest_graph.transitive_closure ~reflexive: true @@ get_graph rel in
      Hashtbl.add rel_to_preorder rel gph;
      gph
    | Some gph -> gph

  let get_rel = function
    | Query.Edges -> get_graph
    | Query.Paths -> get_preorder

  let get_pred pred =
    match Hashtbl.find_opt pred_to_set pred with
    | None -> Vertex_set.empty
    | Some set -> set

  let add_vertex pred x =
    let old = get_pred pred in
    Hashtbl.replace pred_to_set pred @@ Vertex_set.add x old

  let register_iri iri =
    Hashtbl.clear rel_to_preorder;
    all_vertices_ref := Vertex_set.add (Iri_vertex iri) !all_vertices_ref;
    let@ gph = Seq.iter @~ Hashtbl.to_seq_values rel_to_graph in
    Forest_graph.add_vertex gph (Iri_vertex iri)

  let add_edge rel ~source ~target =
    Hashtbl.remove rel_to_preorder rel;
    let gph = get_graph rel in
    Forest_graph.add_edge gph source target
end
