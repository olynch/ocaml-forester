(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_frontend

module L = Lsp.Types
(* module F = Analysis.F *)
(* module PT = Analysis.PT *)
module T = Types

let compute
    ({
      position;
      textDocument;
      _
    }: L.HoverParams.t)
    : L.Hover.t option
  =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let render = Render.render ~dev: true forest STRING in
  let config = State.config forest in
  let host = config.host in
  let content =
    match Forest.find_opt
      forest.parsed
      (Iri_scheme.uri_to_iri ~host: forest.config.host textDocument.uri) with
    | None -> "code of current tree is not stored. this is a bug"
    | Some tree ->
      (* TODO: use node_at and provide hover for things other than links.*)
      match Analysis.addr_at ~position tree.code with
      | None -> Format.asprintf "character: %i, line: %i." position.character position.line;
      | Some addr_at_cursor ->
        let iri_under_cursor = Iri_scheme.user_iri ~host addr_at_cursor in
        match Forest.get_article iri_under_cursor forest.resources with
        | None ->
          Format.asprintf "Could not get article %a." pp_iri iri_under_cursor
        | Some { mainmatter; frontmatter; _ } ->
          let main = render (Content mainmatter) in
          if main = "" then (Format.asprintf "%a" T.(pp_frontmatter pp_content) frontmatter)
          else main
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
