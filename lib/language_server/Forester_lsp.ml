(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module L = Lsp.Types
module RPC = Jsonrpc
module Server = Lsp_server
module Analysis = Analysis

open Forester_compiler
open Forester_frontend

open Server
open Lsp_error

let unwrap opt err =
  match opt with
  | Some opt -> opt
  | None -> raise @@ Lsp_error err

let print_exn exn =
  let msg = Printexc.to_string exn
  and stack = Printexc.get_backtrace ()
  in
  Eio.traceln "%s\n%s" msg stack

let supported_code_actions = [L.CodeActionKind.Other "new tree"]
let supported_commands = ["new tree"]

let server_capabilities =
  let textDocumentSync =
    let opts =
      L.TextDocumentSyncOptions.create
        ~change: L.TextDocumentSyncKind.Full
        ~openClose: true
        ~save: (`SaveOptions (L.SaveOptions.create ~includeText: false ()))
        ()
    in
    `TextDocumentSyncOptions opts
  in
  let hoverProvider =
    let opts = L.HoverOptions.create () in
    `HoverOptions opts
  in
  let codeActionProvider =
    let opts =
      L.CodeActionOptions.create
        ~codeActionKinds: supported_code_actions
        ()
    in
    `CodeActionOptions opts
  in
  let executeCommandProvider =
    L.ExecuteCommandOptions.create
      ~commands: supported_commands
      ()
  in
  let inlayHintProvider =
    let opts = L.InlayHintOptions.create () in
    `InlayHintOptions opts
  in
  let definitionProvider = `DefinitionOptions (L.DefinitionOptions.create ()) in
  let completionProvider =
    L.CompletionOptions.create
      ~triggerCharacters: ["\\"; "{"; "("; ]
      ~allCommitCharacters: ["}"; ")"; ]
      ()
  in
  let documentLinkProvider =
    L.DocumentLinkOptions.create
      ~resolveProvider: true
      ~workDoneProgress: false
      ()
  in
  let workspaceSymbolProvider =
    `WorkspaceSymbolOptions (L.WorkspaceSymbolOptions.create ())
  in
  let documentSymbolProvider =
    `DocumentSymbolOptions (L.DocumentSymbolOptions.create ())
  in
  (* [NOTE: Position Encodings]
     For various historical reasons, the spec states that we are _required_ to support UTF-16.
     This causes more trouble than it's worth, so we always select UTF-8 as our encoding, even
     if the client doesn't support it. *)
  let positionEncoding
    =
    L.PositionEncodingKind.UTF8
  in
  (* [FIME: Reed M, 09/06/2022] The current verison of the LSP library doesn't support 'positionEncoding' *)
  L.ServerCapabilities.create
    ~textDocumentSync
    ~hoverProvider
    ~codeActionProvider
    ~executeCommandProvider
    ~inlayHintProvider
    ~positionEncoding
    ~completionProvider
    ~definitionProvider
    ~documentSymbolProvider
    ~documentLinkProvider
    ~workspaceSymbolProvider
    ()

let supports_utf8_encoding (init_params : L.InitializeParams.t) =
  let position_encodings =
    Option.value ~default: [] @@
    Option.bind init_params.capabilities.general @@
    fun gcap -> gcap.positionEncodings
  in
  List.mem L.PositionEncodingKind.UTF8 position_encodings

(** Perform the LSP initialization handshake.
    https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize *)
let initialize () =
  let (id, req) =
    unwrap (Request.recv ()) @@
      Handshake_error "Initialization must begin with a request."
  in
  match req with
  | E (Initialize init_params as init_req) ->
    begin
      (* [HACK: Position Encodings]
         If the client doesn't support UTF-8, we shouldn't give up, as it might be using UTF-8 anyways...
         Therefore, we just produce a warning, and try to use UTF-8 regardless. *)
      if not (supports_utf8_encoding init_params) then
        Eio.traceln "Warning: client does not support UTF-8 encoding, which may lead to inconsistent positions.";
      let resp = L.InitializeResult.create ~capabilities: server_capabilities () in
      Request.respond id init_req resp;
      let notif =
        unwrap (Notification.recv ()) @@
          Handshake_error "Initialization must complete with an initialized notification."
      in
      match notif with
      | Initialized ->
        Eio.traceln "Initialized!"
      | _ ->
        raise @@ Lsp_error (Handshake_error "Initialization must complete with an initialized notification.")
    end
  | (E _) ->
    raise @@ Lsp_error (Handshake_error "Initialization must begin with an initialize request.")

(** Perform the LSP shutdown sequence.
    See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#exit *)
let shutdown () =
  let notif =
    unwrap (Notification.recv ()) @@
      Shutdown_error "No requests can be recieved after a shutdown request."
  in
  match notif with
  | Exit ->
    ()
  | _ ->
    raise @@ Lsp_error (Shutdown_error "The only notification that can be recieved after a shutdown request is exit.")

(** {1 Main Event Loop} *)

let rec event_loop () =
  match recv () with
  | Some packet ->
    let _ =
      match packet with
      | RPC.Packet.Request req ->
        let resp = Request.handle req in
        send (RPC.Packet.Response resp)
      | RPC.Packet.Notification notif ->
        Notification.handle notif
      | _ ->
        Eio.traceln "Recieved unexpected packet type."
      | exception exn ->
        print_exn exn
    in
    if should_shutdown () then
      shutdown ()
    else
      event_loop ()
  | None ->
    Eio.traceln "Recieved an invalid message. Shutting down...@."

let start ~env ~(config : Config.t) =
  let lsp_io = LspEio.init env in
  let forest = State_machine.batch_run ~env ~config ~dev: true in
  Server.run
    ~init: { forest; lsp_io; should_shutdown = false; } @@
    fun () ->
      begin
        initialize ();
        event_loop ()
      end
