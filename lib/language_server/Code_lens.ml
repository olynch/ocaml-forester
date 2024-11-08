(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module L = Lsp.Types
module F = Analysis.F
module PT = Analysis.PT

let compute (params : L.CodeLensParams.t) =
  let _server = State.get () in
  match params with
  | _ ->
    []
