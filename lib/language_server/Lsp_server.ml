(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Lsp_error

module L = Lsp.Types
module RPC = Jsonrpc
module Lsp_Request = Lsp.Client_request
module Lsp_Notification = Lsp.Client_notification

module Handlers = struct
  module Call_hierarchy = Call_hierarchy
  module Change_configuration = Change_configuration
  module Code_action = Code_action
  module Code_lens = Code_lens
  module Completion = Completion
  module Definitions = Definitions
  module Did_change = Did_change
  module Did_open = Did_open
  module Document_link = Document_link
  module Document_symbols = Document_symbols
  module Highlight = Highlight
  module Hover = Hover
  module Inlay_hint = Inlay_hint
  module Publish = Publish
  module Semantic_tokens = Semantic_tokens
  module Workspace_symbols = Workspace_symbols
end

module Semantic_tokens = Semantic_tokens

let () =
  Printexc.register_printer @@ function
    | Lsp_error (Decode_error err) ->
      Some (Format.asprintf "Lsp Error: Couldn't decode %s" err)
    | Lsp_error (Handshake_error err) ->
      Some (Format.asprintf "Lsp Error: Invalid initialization handshake %s" err)
    | Lsp_error (Shutdown_error err) ->
      Some (Format.asprintf "Lsp Error: Invalid shutdown sequence %s" err)
    | Lsp_error (Unknown_request err) ->
      Some (Format.asprintf "Lsp Error: Unknown request %s" err)
    | Lsp_error (Unknown_notification err) ->
      Some (Format.asprintf "Lsp Error: Unknown notification %s" err)
    | _ -> None

let recv () =
  let server = Lsp_state.get () in
  LspEio.recv server.lsp_io

let send packet =
  let server = Lsp_state.get () in
  LspEio.send server.lsp_io packet

let should_shutdown () =
  let server = Lsp_state.get () in
  server.should_shutdown

let initiate_shutdown () =
  Lsp_state.modify @@ fun st -> {st with should_shutdown = true}

(* I don't understand this request...*)
let document_link_resolve (params : L.DocumentLink.t) =
  match params with
  | link ->
    link

module Request = struct
  type 'resp t = 'resp Lsp.Client_request.t
  type packed = Lsp_Request.packed

  let dispatch : type resp. string -> resp Lsp.Client_request.t -> resp = fun mthd ->
    let open Handlers in
    function
      | Initialize _ ->
        let err = "Server can only recieve a single initialization request." in
        raise @@ Lsp_error (Handshake_error err)
      | Shutdown -> initiate_shutdown ()
      | CodeAction params -> Code_action.compute params
      | CodeActionResolve params -> Code_action.resolve params
      | ExecuteCommand params -> Code_action.execute params
      | TextDocumentHover params -> Hover.compute params
      | TextDocumentCompletion params -> Completion.compute params
      | InlayHint params -> Inlay_hint.compute params
      | TextDocumentDefinition params -> Definitions.compute params
      | DocumentSymbol params -> Document_symbols.compute params
      | TextDocumentLink params -> Document_link.compute params
      | TextDocumentLinkResolve params -> document_link_resolve params
      | WorkspaceSymbol params -> Workspace_symbols.compute params
      | TextDocumentPrepareCallHierarchy params -> Call_hierarchy.compute params
      | CallHierarchyIncomingCalls params -> Call_hierarchy.incoming params
      | CallHierarchyOutgoingCalls params -> Call_hierarchy.outgoing params
      | TextDocumentCodeLens params -> Code_lens.compute params
      | SemanticTokensFull params -> Semantic_tokens.on_full_request params
      | SemanticTokensDelta params -> Semantic_tokens.on_delta_request params
      | _ ->
        raise @@ Lsp_error (Unknown_request mthd)

  let handle (msg : RPC.Request.t) =
    Eio.traceln "Request: %s@." msg.method_;
    match Lsp_Request.of_jsonrpc msg with
    | Ok (E r) ->
      let resp = dispatch msg.method_ r in
      let json = Lsp_Request.yojson_of_result r resp in
      RPC.Response.ok msg.id json
    | Error err ->
      raise (Lsp_error (Decode_error err))

  let recv () =
    Option.bind (recv ()) @@ function
      | RPC.Packet.Request req ->
        begin
          match Lsp_Request.of_jsonrpc req with
          | Ok packed -> Some (req.id, packed)
          | Error err -> raise @@ Lsp_error (Decode_error err)
        end
      | _ -> None

  let respond id req resp =
    let json = Lsp_Request.yojson_of_result req resp in
    send (RPC.Packet.Response (RPC.Response.ok id json))
end

module Notification = struct
  type t = Lsp.Client_notification.t

  let dispatch : string -> t -> unit = fun mthd ->
    function
      | TextDocumentDidOpen params -> Did_open.compute params
      | TextDocumentDidChange params -> Did_change.compute params
      | ChangeConfiguration params -> Change_configuration.compute params
      | DidSaveTextDocument _ -> ()
      | TextDocumentDidClose _ -> ()
      | _ ->
        raise @@ Lsp_error (Unknown_notification mthd)

  let handle (msg : RPC.Notification.t) =
    Eio.traceln "Request: %s@." msg.method_;
    match Lsp_Notification.of_jsonrpc msg with
    | Ok notif ->
      dispatch msg.method_ notif
    | Error err ->
      raise @@ Lsp_error (Decode_error err)

  let recv () =
    Option.bind (recv ()) @@ function
      | RPC.Packet.Notification msg ->
        begin
          match Lsp_Notification.of_jsonrpc msg with
          | Ok notif -> Some notif
          | Error err -> raise @@ Lsp_error (Decode_error err)
        end
      | _ -> None
end

let run ~init k =
  Lsp_state.run ~init k
