(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module L = Lsp.Types
open Forester_core

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
  let diagnostics = Hashtbl.create 5 in
  (* TODO: Pull out code shared with did_change handler. For some reason using Util.lsp_report does not work *)
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
        begin
          let syn_diags = Analysis.syntax_diagnostics (`Uri L.TextDocumentIdentifier.{ uri }) in
          begin
            match syn_diags with
            | [] -> Hashtbl.add diagnostics uri []
            | ds -> Hashtbl.add diagnostics uri ds
          end;
          Analysis.check_semantics uri
        end
  );
  Hashtbl.iter
    (
      fun uri ds ->
        Eio.traceln "publishing to %s" (Lsp.Uri.to_string uri);
        Publish.to_uri uri ds
    )
    diagnostics
