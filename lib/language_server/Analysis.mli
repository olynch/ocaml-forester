(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module G: Forester_forest.Forest_graphs.S
module F: Forester_forest.Forest.S
module L = Lsp.Types

val check : Base.server -> L.DocumentUri.t -> unit

val build_once :
  Base.server ->
  unit ->
  unit

val extract_addr :
  Forester_compiler.Code.node Forester_core.Range.located ->
  string option

val addr_at :
  position: Lsp.Types.Position.t ->
  Forester_compiler.Code.t ->
  string option
