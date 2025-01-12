(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_compiler

type state = {
  forest: State.t;
  should_shutdown: bool;
  lsp_io: LspEio.io;
}

module M = Algaeff.State.Make(struct type t = state end)
include M
