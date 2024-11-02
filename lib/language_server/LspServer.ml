(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core

module T = Forester_core.Types
module EP = Eio.Path
module L = Lsp.Types
module RPC = Jsonrpc
module Broadcast = Lsp.Server_notification
module Lsp_Diagnostic = Lsp.Types.Diagnostic
module Lsp_Request = Lsp.Client_request
module Lsp_Notification = Lsp.Client_notification

module F = Analysis.F
module PT = Analysis.PT

let (let*) = Option.bind

type eio_path = Eio.Fs.dir_ty EP.t

type diagnostic = Reporter.Message.t Asai.Diagnostic.t

type lsp_error =
  | DecodeError of string
  | HandshakeError of string
  | ShutdownError of string
  | UnknownRequest of string
  | UnknownNotification of string

exception LspError of lsp_error

let () =
  Printexc.register_printer @@
    function
    | LspError (DecodeError err) ->
      Some (Format.asprintf "Lsp Error: Couldn't decode %s" err)
    | LspError (HandshakeError err) ->
      Some (Format.asprintf "Lsp Error: Invalid initialization handshake %s" err)
    | LspError (ShutdownError err) ->
      Some (Format.asprintf "Lsp Error: Invalid shutdown sequence %s" err)
    | LspError (UnknownRequest err) ->
      Some (Format.asprintf "Lsp Error: Unknown request %s" err)
    | LspError (UnknownNotification err) ->
      Some (Format.asprintf "Lsp Error: Unknown notification %s" err)
    | _ -> None

let recv () =
  let server = State.get () in
  LspEio.recv server.lsp_io

let send packet =
  let server = State.get () in
  LspEio.send server.lsp_io packet

let should_shutdown () =
  let server = State.get () in
  server.should_shutdown

let initiate_shutdown () =
  State.modify @@ fun st -> { st with should_shutdown = true }

(* [TODO: Reed M, 12/12/2022] No code actions for now. *)
let code_action (_params : L.CodeActionParams.t) : L.CodeActionResult.t =
  let action = L.CodeAction.create ~title: "transclude new tree" () in
  Some [`CodeAction action]

(* I don't understand this request...*)
let document_link_resolve (params : L.DocumentLink.t) =
  match params with
  | link ->
    link

module Request = struct
  type 'resp t = 'resp Lsp.Client_request.t
  type packed = Lsp_Request.packed

  let hover = Hover.compute
  let completion = Completion.compute
  let inlay_hint = Inlay_hint.compute
  let definitions = Definitions.compute
  let document_link = Document_link.compute

  let dispatch : type resp. string -> resp t -> resp = fun mthd ->
      function
      | Initialize _ ->
        let err = "Server can only recieve a single initialization request." in
        raise @@ LspError (HandshakeError err)
      | Shutdown ->
        initiate_shutdown ()
      | CodeAction params ->
        code_action params
      | TextDocumentHover params ->
        hover params
      | TextDocumentCompletion params ->
        completion params
      | InlayHint params ->
        inlay_hint params
      | TextDocumentDefinition params ->
        definitions params
      (* | SemanticTokensDelta params -> *)
      (*   Semantic_tokens.on_delta_request params *)
      (* | SemanticTokensFull params -> *)
      (*   Semantic_tokens.on_full_request params *)
      | TextDocumentLink params ->
        document_link params
      | TextDocumentLinkResolve params ->
        document_link_resolve params
      | WorkspaceSymbol params ->
        Workspace_symbols.compute params
      | _ ->
        raise @@ LspError (UnknownRequest mthd)

  let handle (msg : RPC.Request.t) =
    Eio.traceln "Request: %s@." msg.method_;
    match Lsp_Request.of_jsonrpc msg with
    | Ok (E r) ->
      let resp = dispatch msg.method_ r in
      let json = Lsp_Request.yojson_of_result r resp in
      RPC.Response.ok msg.id json
    | Error err ->
      raise (LspError (DecodeError err))

  let recv () =
    Option.bind (recv ()) @@
      function
      | RPC.Packet.Request req ->
        begin
          match Lsp_Request.of_jsonrpc req with
          | Ok packed -> Some (req.id, packed)
          | Error err -> raise @@ LspError (DecodeError err)
        end
      | _ -> None

  let respond id req resp =
    let json = Lsp_Request.yojson_of_result req resp in
    send (RPC.Packet.Response (RPC.Response.ok id json))
end

module Notification = struct
  type t = Lsp.Client_notification.t

  let dispatch : string -> t -> unit = fun mthd ->
      let server = State.get () in
      function
      | TextDocumentDidOpen ({ textDocument = { uri; _ } } as params) ->
        let text_document = Lsp.Text_document.make ~position_encoding: `UTF16 params in
        Hashtbl.replace server.index.documents { uri } text_document;
        Analysis.check uri
      | DidSaveTextDocument { textDocument; _; } ->
        begin
          match Hashtbl.find_opt server.index.documents textDocument with
          (* ocaml-lsp does *this* here: https://github.com/ocaml/ocaml-lsp/blob/8b47925eb44f907b8ec41a44c1b2a55447f1b439/ocaml-lsp-server/src/ocaml_lsp_server.ml#L757 *)
          | _ -> ()
        end
      | TextDocumentDidChange { textDocument = { uri; _ }; contentChanges } ->
        begin
          match Hashtbl.find_opt server.index.documents { uri } with
          | Some doc ->
            let new_doc =
              Lsp.Text_document.apply_content_changes
                doc
                contentChanges
            in
            Hashtbl.replace server.index.documents { uri } new_doc;
            Reporter.lsp_run Publish.publish_diagnostics uri @@
              fun () ->
                Analysis.check uri
          | None ->
            Reporter.lsp_run Publish.publish_diagnostics uri @@
              fun () ->
                Reporter.fatalf Internal_error "%s" "could not find document at %s" (uri |> Lsp.Uri.to_path)
        end
      | _ ->
        raise @@ LspError (UnknownNotification mthd)

  let handle (msg : RPC.Notification.t) =
    Eio.traceln "Request: %s@." msg.method_;
    match Lsp_Notification.of_jsonrpc msg with
    | Ok notif ->
      dispatch msg.method_ notif
    | Error err ->
      raise @@ LspError (DecodeError err)

  let recv () =
    Option.bind (recv ()) @@
      function
      | RPC.Packet.Notification msg ->
        begin
          match Lsp_Notification.of_jsonrpc msg with
          | Ok notif -> Some notif
          | Error err -> raise @@ LspError (DecodeError err)
        end
      | _ -> None
end

let run ~init k =
  State.run ~init k
