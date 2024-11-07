(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module L = Lsp.Types

let compute
    (params : L.DidChangeTextDocumentParams.t)
  =
  let server = State.get () in
  match params with
  | { textDocument = { uri; _ }; contentChanges } ->
    match Hashtbl.find_opt server.index.documents { uri } with
    | Some doc ->
      let new_doc = Lsp.Text_document.apply_content_changes doc contentChanges in
      Hashtbl.add server.index.documents { uri } new_doc;
      Publish.publish_diagnostics uri @@
        Analysis.syntax_diagnostics
          (`Text_document new_doc);
      Analysis.check_semantics uri
    | None -> ()
