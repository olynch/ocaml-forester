(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

type t

val create : ?size: int -> unit -> t
val add_vertex : t -> Vertex.t -> unit
val add_edge : t -> Vertex.t -> Vertex.t -> unit
val safe_pred : t -> Vertex.t -> Vertex.t list
val safe_succ : t -> Vertex.t -> Vertex.t list
val mem_edge : t -> Vertex.t -> Vertex.t -> bool
val transitive_closure : ?reflexive: bool -> t -> t

val out_degree : t -> Vertex.t -> int

val remove_vertex : t -> Vertex.t -> unit
val transitive_reduction : ?reflexive: bool -> t -> t
val nb_vertex : t -> int
val iter_edges : (Vertex.t -> Vertex.t -> unit) -> t -> unit
val iter_vertex : (Vertex.t -> unit) -> t -> unit
val mem_vertex : t -> Vertex.t -> bool
val topo_fold : (Vertex.t -> 'a -> 'a) -> t -> 'a -> 'a

val iter_succ : (Vertex.t -> unit) -> t -> Vertex.t -> unit
