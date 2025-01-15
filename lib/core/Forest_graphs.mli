(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** A simple graph database. Used to record the {{!Forester_core.Builtin_relation}relations} that exist between trees.*)

module type S = sig
  val dl_db : Datalog_engine.db
  val register_iri : Iri.t -> unit
  val get_all_vertices : unit -> Vertex_set.t
  val get_rel : Query.mode -> Query.rel -> Forest_graph.t
  val add_edge : Query.rel -> source: Vertex.t -> target: Vertex.t -> unit
  val add_graph : Query.rel -> Forest_graph.t -> unit
end

module Make () : S
