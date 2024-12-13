(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_compiler

type t

module V:
sig
  type t = string
  val compare : t -> t -> int
  val hash : t -> int
  val equal : t -> t -> bool
  type label = t
  val create : label -> t
  val label : t -> label
end

type vertex = V.t

val empty : unit -> t

val create : ?size: int -> unit -> t
val add_vertex : t -> string -> unit
val add_edge : t -> string -> string -> unit
val transitive_reduction : ?reflexive: bool -> t -> t
val build : Code.tree list -> t
val topo_fold : (string -> 'a -> 'a) -> t -> 'a -> 'a
val nb_vertex : t -> int
val nb_edges : t -> int
val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
val iter_vertex : (V.t -> unit) -> t -> unit
val iter_edges : (V.t -> V.t -> unit) -> t -> unit

val out_degree : t -> vertex -> int
val in_degree : t -> vertex -> int
(* val find : V.t -> t -> V.t *)
