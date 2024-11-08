(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core

module L = Lsp.Types
module Lsp_Diagnostic = Lsp.Types.Diagnostic
module Broadcast = Lsp.Server_notification
module RPC = Jsonrpc

type diagnostic = Reporter.Message.t Asai.Diagnostic.t

let send packet =
  let server = State.get () in
  LspEio.send server.lsp_io packet

let render_lsp_related_info (uri : L.DocumentUri.t) (message : Asai.Diagnostic.loctext) : L.DiagnosticRelatedInformation.t =
  let range = LspShims.Loc.lsp_range_of_range message.loc in
  let location = L.Location.create ~uri ~range in
  let message = Asai.Diagnostic.string_of_text message.value in
  L.DiagnosticRelatedInformation.create ~location ~message

let render_lsp_diagnostic (uri : L.DocumentUri.t) (diag : diagnostic) : Lsp_Diagnostic.t =
  let range = LspShims.Loc.lsp_range_of_range diag.explanation.loc in
  let severity = LspShims.Diagnostic.lsp_severity_of_severity @@ diag.severity in
  let code = `String (Reporter.Message.short_code diag.message) in
  let source =
    match Hashtbl.find_opt (State.get ()).index.documents { uri } with
    | None -> None
    | Some doc ->
      Some (Lsp.Text_document.text doc)
  in
  let message = Asai.Diagnostic.string_of_text diag.explanation.value in
  let relatedInformation = Bwd.to_list @@ Bwd.map (render_lsp_related_info uri) diag.extra_remarks in
  Lsp_Diagnostic.create
    ~range
    ~severity
    ~code
    ?source
    ~message: (`String message)
    ~relatedInformation
    ()

let broadcast notif =
  let msg = Broadcast.to_jsonrpc notif in
  send (RPC.Packet.Notification msg)

let to_uri (uri : Lsp.Uri.t) (diagnostics : diagnostic list) =
  let diagnostics = List.map (render_lsp_diagnostic uri) diagnostics in
  let params = L.PublishDiagnosticsParams.create ~uri ~diagnostics () in
  broadcast (PublishDiagnostics params)
