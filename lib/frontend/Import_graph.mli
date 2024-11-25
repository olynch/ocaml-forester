(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

type t
val create : ?size: int -> unit -> t
val add_vertex : t -> string -> unit
val add_edge : t -> string -> string -> unit
val transitive_reduction : ?reflexive: bool -> t -> t
val build : Code.tree list -> t
val topo_fold : (string -> 'a -> 'a) -> t -> 'a -> 'a
