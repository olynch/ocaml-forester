(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module L = Lsp.Types

let compute
    ({ textDocument = { uri; _ } } as params: L.DidOpenTextDocumentParams.t)
  =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let docs = Forester_frontend.Compiler.documents forest in
  let document =
    Lsp.Text_document.make
      ~position_encoding: `UTF16
      params
  in
  Hashtbl.replace docs uri document;
  Diagnostics.compute document
