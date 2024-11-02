(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core

module L = Lsp.Types
module F = Analysis.F
module PT = Analysis.PT
module T = Types
let compute
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
    match Hashtbl.find_opt server.index.codes { uri = textDocument.uri } with
    | None -> "code of current tree is not stored. this is a bug"
    | Some tree ->
      match Analysis.addr_at ~position tree.code with
      | None -> Format.asprintf "character: %i, line: %i." position.character position.line;
      | Some addr_at_cursor ->
        let iri_under_cursor = Iri_scheme.user_iri ~host addr_at_cursor in
        match F.get_article iri_under_cursor with
        | None ->
          Format.asprintf "Could not get article %a. This is a bug." pp_iri iri_under_cursor
        | Some { mainmatter; frontmatter; _ } ->
          let main = PT.string_of_content mainmatter in
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
