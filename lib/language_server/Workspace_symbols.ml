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

let compute (_params : L.WorkspaceSymbolParams.t) =
  let server = State.get () in
  let symbols =
    server.index.codes
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
