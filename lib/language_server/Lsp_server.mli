(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module Semantic_tokens = Semantic_tokens

(** The portion of the protocol that is currently supported by the forester language server.
*)
module Handlers: sig

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareCallHierarchy}[textDocument/prepareCallHierarchy].}
  *)
  module Call_hierarchy: sig
    val incoming : Lsp.Types.CallHierarchyIncomingCallsParams.t -> Lsp.Types.CallHierarchyIncomingCall.t list option
    val outgoing : Lsp.Types.CallHierarchyOutgoingCallsParams.t -> Lsp.Types.CallHierarchyOutgoingCall.t list option
    val compute : Lsp.Types.CallHierarchyPrepareParams.t -> 'a list option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeConfiguration}[workspace/didChangeConfigurationNotification]}
  *)
  module Change_configuration: sig
    val compute : Lsp.Types.DidChangeConfigurationParams.t -> unit
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeLens}[textDocument/codeLens]}
     *)
  module Code_lens: sig
    val compute : Lsp.Types.CodeLensParams.t -> 'a list
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction}[textDocument/codeAction]}
     *)
  module Code_action: sig
    val execute : Lsp.Types.ExecuteCommandParams.t -> Yojson.Safe.t
    val compute : Lsp.Types.CodeActionParams.t -> Lsp.Types.CodeActionResult.t
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#
     textDocument_completion}[textDocument/completion]}
     *)
  module Completion: sig
    val compute : Lsp.Types.CompletionParams.t -> [> `CompletionList of Lsp.Types.CompletionList.t] option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition}[textDocument/definition]}
     *)
  module Definitions: sig
    val compute : Lsp.Types.DefinitionParams.t -> Lsp.Types.Locations.t option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didChange}[textDocument/didChange]}
     *)
  module Did_change: sig
    val compute : Lsp.Types.DidChangeTextDocumentParams.t -> unit
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_didOpen}[textDocument/didOpen]}
     *)
  module Did_open: sig
    val compute : Lsp.Types.DidOpenTextDocumentParams.t -> unit
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentLink}[textDocument/documentLink]}
     *)
  module Document_link: sig
    val compute : Lsp.Types.DocumentLinkParams.t -> Lsp.Types.DocumentLink.t list option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol}[textDocument/documentSymbol]}
     *)
  module Document_symbols: sig
    val compute : Lsp.Types.DocumentSymbolParams.t -> [> `DocumentSymbol of Lsp.Types.DocumentSymbol.t list] option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_highlight}[textDocument/highlight]}
     *)
  module Highlight: sig
    val compute : Lsp.Types.DocumentHighlightParams.t -> Lsp.Types.DocumentHighlight.t list option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover}[textDocument/hover]}
     *)
  module Hover: sig
    val compute : Lsp.Types.HoverParams.t -> Lsp.Types.Hover.t option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint}[textDocument/inlayHint]}
     *)
  module Inlay_hint: sig
    val compute : Lsp.Types.InlayHintParams.t -> Lsp.Types.InlayHint.t list option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens}[textDocument/semanticTokens]}
     *)
  module Semantic_tokens: sig
    val on_full_request : Lsp.Types.SemanticTokensParams.t -> Lsp.Types.SemanticTokens.t option
    val on_delta_request : Lsp.Types.SemanticTokensDeltaParams.t -> [`SemanticTokens of Lsp.Types.SemanticTokens.t | `SemanticTokensDelta of Lsp.Types.SemanticTokensDelta.t] option
  end

  (** Reference: {{:https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_symbol}[workspace/symbol]}
     *)
  module Workspace_symbols: sig
    val compute : Lsp.Types.WorkspaceSymbolParams.t -> Lsp.Types.SymbolInformation.t list option
  end
end

val recv : unit -> Jsonrpc.Packet.t option
val send : Jsonrpc.Packet.t -> unit

val should_shutdown : unit -> bool
val initiate_shutdown : unit -> unit

val run : init: Lsp_state.state -> (unit -> 'a) -> 'a

module Request: sig
  type packed = Lsp.Client_request.packed
  type 'resp t = 'resp Lsp.Client_request.t

  val handle : Jsonrpc.Request.t -> Jsonrpc.Response.t
  val recv : unit -> (Jsonrpc.Id.t * packed) option
  val respond : Jsonrpc.Id.t -> 'resp t -> 'resp -> unit
end

module Notification: sig
  type t = Lsp.Client_notification.t

  val handle : Jsonrpc.Notification.t -> unit
  val recv : unit -> t option
end
