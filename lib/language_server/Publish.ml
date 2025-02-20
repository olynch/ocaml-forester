(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

module L = Lsp.Types
module Lsp_Diagnostic = Lsp.Types.Diagnostic
module Broadcast = Lsp.Server_notification
module RPC = Jsonrpc

type diagnostic = Reporter.diagnostic

type table = (Lsp.Uri.t, diagnostic list) Hashtbl.t

let send packet =
  let server = Lsp_state.get () in
  LspEio.send server.lsp_io packet

let render_lsp_related_info (uri : L.DocumentUri.t) (message : Asai.Diagnostic.loctext) : L.DiagnosticRelatedInformation.t =
  let range = Lsp_shims.Loc.lsp_range_of_range message.loc in
  let location = L.Location.create ~uri ~range in
  let message = Asai.Diagnostic.string_of_text message.value in
  L.DiagnosticRelatedInformation.create ~location ~message

let render_lsp_diagnostic (uri : L.DocumentUri.t) (diag : diagnostic) : Lsp_Diagnostic.t =
  let range = Lsp_shims.Loc.lsp_range_of_range diag.explanation.loc in
  let severity = Lsp_shims.Diagnostic.lsp_severity_of_severity @@ diag.severity in
  let code = `String (Reporter.Message.short_code diag.message) in
  let source =
    let Lsp_state.{forest; _} = Lsp_state.get () in
    match Hashtbl.find_opt (State.documents forest) uri with
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

let publish (uri : Lsp.Uri.t) (diagnostics : diagnostic list) =
  let diagnostics = List.map (render_lsp_diagnostic uri) diagnostics in
  let params = L.PublishDiagnosticsParams.create ~uri ~diagnostics () in
  broadcast (PublishDiagnostics params)
