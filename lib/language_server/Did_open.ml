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
  let server = State.get () in
  let document =
    Lsp.Text_document.make
      ~position_encoding: `UTF16
      params
  in
  Hashtbl.replace server.index.documents { uri } document;
  Publish.publish_diagnostics uri @@
    Analysis.syntax_diagnostics (`Uri L.TextDocumentIdentifier.{ uri });
  Analysis.check_semantics uri
