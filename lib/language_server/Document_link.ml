(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler

module L = Lsp.Types
module F = Analysis.F
module PT = Analysis.PT

let (let*) = Option.bind

(* TODO: handle external links as well? *)
let compute (params : L.DocumentLinkParams.t) =
  match params with
  | { textDocument; _ } ->
    let server = State.get () in
    let links =
      match Hashtbl.find_opt server.index.codes textDocument with
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
                  let* target = Hashtbl.find_opt server.index.resolver iri in
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
