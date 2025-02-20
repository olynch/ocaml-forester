(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_compiler
module L = Lsp.Types

let compute
    (params : L.DidChangeTextDocumentParams.t)
  =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  match params with
  | {textDocument = {uri; _}; contentChanges} ->
    let docs = State.documents forest in
    match Hashtbl.find_opt docs uri with
    | None -> assert false
    | Some doc ->
      let new_doc = Lsp.Text_document.apply_content_changes doc contentChanges in
      Eio.traceln "After change, doc has content %s" (Lsp.Text_document.text new_doc);
      Hashtbl.replace docs uri new_doc;
      Diagnostics.compute new_doc
