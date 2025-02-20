(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module type S = sig
  val run_query : Types.query -> Vertex_set.t
end

module Make (_ : Forest_graphs.S) : S
