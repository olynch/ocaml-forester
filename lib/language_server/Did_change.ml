(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module L = Lsp.Types
open Forester_core

let compute
    (params : L.DidChangeTextDocumentParams.t)
  =
  let server = State.get () in
  match params with
  | { textDocument = { uri; _ }; contentChanges } ->
    match Hashtbl.find_opt server.index.documents { uri } with
    | None -> ()
    | Some doc ->
      let new_doc = Lsp.Text_document.apply_content_changes doc contentChanges in
      Hashtbl.add server.index.documents { uri } new_doc;
      (* TODO: Pull out code shared with did_open handler. For some reason using Util.lsp_report does not work *)
      let diagnostics = Hashtbl.create 5 in
      let push d =
        match Util.guess_uri d with
        | Some guessed_uri ->
          Eio.traceln "guessed %s" (Lsp.Uri.to_string guessed_uri);
          begin
            match Hashtbl.find_opt diagnostics uri with
            | Some diags -> Hashtbl.add diagnostics guessed_uri (d :: diags)
            | None -> Hashtbl.add diagnostics guessed_uri [d]
          end
        | None -> Hashtbl.add diagnostics uri [d]
      in
      (
        Reporter.run ~emit: push ~fatal: push @@
          fun () ->
            let syn_diags = Analysis.syntax_diagnostics (`Text_document new_doc) in
            Eio.traceln "got %i diagnostics " (List.length syn_diags);
            begin
              match syn_diags with
              | [] -> Hashtbl.add diagnostics uri []
              | ds -> Hashtbl.add diagnostics uri ds
            end;
            Analysis.check_semantics uri
      );
      Hashtbl.iter
        (
          fun uri ds ->
            Eio.traceln "publishing %i diagnostics to %s" (List.length ds) (Lsp.Uri.to_string uri);
            Publish.to_uri uri ds
        )
        diagnostics
