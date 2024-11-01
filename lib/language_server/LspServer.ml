(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_frontend
open Forester_compiler
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
module State = Base.State

let (let*) = Option.bind

module PT = Forester_forest.Plain_text_client.Make(F)(struct let route _ = "todo" end)

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

let broadcast notif =
  let msg = Broadcast.to_jsonrpc notif in
  send (RPC.Packet.Notification msg)

let render_lsp_related_info (uri : L.DocumentUri.t) (message : Asai.Diagnostic.loctext) : L.DiagnosticRelatedInformation.t =
  let range = LspShims.Loc.lsp_range_of_range message.loc in
  let location = L.Location.create ~uri ~range in
  let message = Asai.Diagnostic.string_of_text message.value in
  L.DiagnosticRelatedInformation.create ~location ~message

let render_lsp_diagnostic (uri : L.DocumentUri.t) (diag : diagnostic) : Lsp_Diagnostic.t =
  let range = LspShims.Loc.lsp_range_of_range diag.explanation.loc in
  let severity = LspShims.Diagnostic.lsp_severity_of_severity @@ diag.severity in
  let code = `String (Reporter.Message.short_code diag.message) in
  let source = (State.get ()).source in
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

let publish_diagnostics (uri : Lsp.Uri.t) (diagnostics : diagnostic list) =
  let diagnostics = List.map (render_lsp_diagnostic uri) diagnostics in
  let params = L.PublishDiagnosticsParams.create ~uri ~diagnostics () in
  broadcast (PublishDiagnostics params)

let should_shutdown () =
  let server = State.get () in
  server.should_shutdown

let initiate_shutdown () =
  State.modify @@ fun st -> { st with should_shutdown = true }

(* [TODO: Reed M, 12/12/2022] No code actions for now. *)
let code_action (_params : L.CodeActionParams.t) : L.CodeActionResult.t =
  let action = L.CodeAction.create ~title: "find tree" () in
  Some [`CodeAction action]

let completion
    (params : L.CompletionParams.t)
  =
  match params with
  | {
    context;
    _;
  } ->
    let triggerCharacter =
      match context with
      | Some { triggerCharacter; _ } ->
        triggerCharacter
      | None -> None
    in
    let server = State.get () in
    let addr_items () =
      server.codes
      |> Hashtbl.to_seq_values
      |> List.of_seq
      |> List.filter_map
        (
          fun (tree : Code.tree) ->
            let* addr = tree.addr in
            let* { frontmatter; mainmatter; _ } =
              (F.get_article @@ Iri_scheme.user_iri ~host: server.config.host addr)
            in
            let documentation =
              try
                let render = PT.string_of_content in
                let title = frontmatter.title in
                let taxon = frontmatter.taxon in
                let content =
                  Format.asprintf
                    {|%s
%s
%s
|}
                    (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "# %s" (render s)) title)
                    (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "taxon: %s" (render s)) taxon)
                    (render mainmatter)
                in
                Some (`String content)
              with
                | _ ->
                  Some (`String "computation of my value crashed")
            in
            let insertText =
              match triggerCharacter with
              | Some "{" -> addr ^ "}"
              | Some "(" -> addr ^ ")"
              | Some "[" -> addr ^ "]"
              | _ -> addr
            in
            Some (L.CompletionItem.create ?documentation ~label: addr ~insertText ())
        )
    in
    let trees = server.codes |> Hashtbl.to_seq_values |> List.of_seq in
    let scope_items () =
      let units, _expanded = Forest_reader.expand ~host: server.config.host trees in
      units
      |> Expand.Unit_map.to_list
      |> List.map snd
      |> List.concat_map
        (
          fun trie ->
            let open Yuujinchou in
            trie
            |> Trie.to_seq
            |> List.of_seq
            |> List.filter_map Completion.make
        )
      |> List.append
        (
          Expand.builtins
          |> List.map (fun (a, b) -> (a, (Resolver.P.Term [Range.locate_opt None b], ())))
          |> List.filter_map Completion.make
        )
    in
    let items =
      match triggerCharacter with
      | Some "(" -> addr_items ()
      | Some "{" -> addr_items ()
      | Some "\\" -> scope_items ()
      | _ -> []
    in
    Some
      (
        `CompletionList
          (L.CompletionList.create ~isIncomplete: false ~items ())
      )

let hover
    ({
      position;
      textDocument;
      _
    }: L.HoverParams.t)
    : L.Hover.t option
  =
  let server = State.get () in
  let host = server.config.host in
  let content =
    match Hashtbl.find_opt server.codes { uri = textDocument.uri } with
    | None -> "code of current tree is not stored. this is a bug"
    | Some tree ->
      match Analysis.addr_at ~position tree.code with
      | None -> Format.asprintf "character: %i, line: %i." position.character position.line;
      | Some addr_at_cursor ->
        let iri_under_cursor = Iri_scheme.user_iri ~host addr_at_cursor in
        match F.get_article iri_under_cursor with
        | None -> Format.asprintf "extracted iri %a" pp_iri iri_under_cursor
        | Some { mainmatter; _ } ->
          PT.string_of_content mainmatter
  in
  Some
    (
      L.Hover.create
        ~contents: (
          `MarkupContent
            {
              kind = L.MarkupKind.Markdown;
              value = content
            }
        )
        ()
    )

let definitions
    (params : L.DefinitionParams.t)
    : L.Locations.t option
  =
  match params with
  | {
    position;
    textDocument;
    _;
  } ->
    let server = State.get () in
    let codes = server.codes in
    let* { code; _ } = Hashtbl.find_opt codes { uri = textDocument.uri } in
    let* addr = Analysis.addr_at ~position code in
    let iri = Iri_scheme.user_iri ~host: server.config.host addr in
    let* uri = Hashtbl.find_opt server.resolver iri in
    let range = L.Range.create ~start: { character = 1; line = 0 } ~end_: { character = 1; line = 0 } in
    Some
      (`Location [L.Location.{ uri = uri.uri; range }])

let inlay_hint (params : L.InlayHintParams.t) : L.InlayHint.t list option =
  match params with
  | {
    textDocument;
    _;
  } ->
    let server = State.get () in
    match Hashtbl.find_opt server.codes { uri = textDocument.uri } with
    | None -> None
    | Some { code; _ } ->
      List.filter_map
        (
          fun
              (Range.{ loc; _ } as node)
            ->
            match Option.map Range.view loc with
            | Some (`Range (_, pos)) ->
              let* str = Analysis.extract_addr node in
              let iri = Iri_scheme.user_iri ~host: server.config.host str in
              let* { frontmatter; _ } = F.get_article iri in
              let* title = frontmatter.title in
              let content = " " ^ PT.string_of_content title in
              Some
                (
                  L.InlayHint.create
                    ~position: (LspShims.Loc.lsp_pos_of_pos pos)
                    ~label: (`String content)
                    ()
                )
            | _ -> None
        )
        code
      |> Option.some

(* TODO: handle external links as well? *)
let document_link (params : L.DocumentLinkParams.t) =
  match params with
  | { textDocument; _ } ->
    let server = State.get () in
    let links =
      match Hashtbl.find_opt server.codes textDocument with
      | Some tree ->
        begin
          tree.code
          |> List.filter_map
            (
              fun node ->
                match Range.(node.value) with
                | Code.Group (Squares, [{ value = Text addr; _ }])
                | Code.Group (Parens, [{ value = Text addr; _ }])
                | Code.Group (Braces, [{ value = Text addr; _ }]) ->
                  (* TODO: Need to analyse syn *)
                  let range = (LspShims.Loc.lsp_range_of_range node.loc) in
                  let iri = (Iri_scheme.user_iri ~host: server.config.host addr) in
                  let* target = Hashtbl.find_opt server.resolver iri in
                  let* { frontmatter; _ } = F.get_article iri in
                  let* tooltip = Option.map PT.string_of_content frontmatter.title in
                  let link =
                    L.DocumentLink.create
                      ~range
                      ~target: target.uri
                      ~tooltip
                      ()
                  in
                  Some link
                | _ ->
                  None
            )
        end
      | None ->
        []
    in
    Some links

(* I don't understand this request...*)
let document_link_resolve (params : L.DocumentLink.t) =
  match params with
  | link ->
    link

let workspace_symbol (_params : L.WorkspaceSymbolParams.t) =
  (* let _s = L.WorkspaceSymbol.create ~name: "test" () in *)
  let server = State.get () in
  let symbols =
    server.codes
    |> Hashtbl.to_seq
    |> Seq.map
      (
        fun ((ident : L.TextDocumentIdentifier.t), _) ->
          let location =
            L.Location.{
              range = L.Range.{
                end_ = { character = 0; line = 0; };
                start = { character = 0; line = 0; };
              };
              uri = ident.uri
            }
          in
          let iri =
            ident.uri
            |> Lsp.Uri.to_path
            |> String.split_on_char '/'
            |> List.rev
            |> List.hd
            |> Filename.chop_extension
            |> Iri_scheme.user_iri ~host: server.config.host
          in
          let title =
            match F.get_article iri with
            | None -> "untitled"
            | Some { frontmatter; _ } ->
              begin
                match frontmatter.title with
                | None -> "untitled"
                | Some content ->
                  PT.string_of_content content
              end
          in
          L.SymbolInformation.create ~kind: File ~location ~name: title ()
      )
    |> List.of_seq
  in
  Some symbols

module Request = struct
  type 'resp t = 'resp Lsp.Client_request.t
  type packed = Lsp_Request.packed

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
      | SemanticTokensDelta params ->
        Semantic_tokens.on_delta_request params
      | SemanticTokensFull params ->
        Semantic_tokens.on_full_request params
      | TextDocumentLink params ->
        document_link params
      | TextDocumentLinkResolve params ->
        document_link_resolve params
      | WorkspaceSymbol params ->
        workspace_symbol params
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
        Hashtbl.replace server.documents { uri } text_document;
        Reporter.lsp_run publish_diagnostics uri @@
          fun () ->
            Analysis.check server uri
      | DidSaveTextDocument { textDocument; _; } ->
        begin
          match Hashtbl.find_opt server.documents textDocument with
          (* ocaml-lsp does *this* here: https://github.com/ocaml/ocaml-lsp/blob/8b47925eb44f907b8ec41a44c1b2a55447f1b439/ocaml-lsp-server/src/ocaml_lsp_server.ml#L757 *)
          | _ -> ()
        end
      | TextDocumentDidChange { textDocument = { uri; _ }; contentChanges } ->
        begin
          match Hashtbl.find_opt server.documents { uri } with
          | Some doc ->
            let new_doc =
              Lsp.Text_document.apply_content_changes
                doc
                contentChanges
            in
            Hashtbl.replace server.documents { uri } new_doc;
            Reporter.lsp_run publish_diagnostics uri @@
              fun () ->
                Analysis.check server uri
          | None ->
            Reporter.lsp_run publish_diagnostics uri @@
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
