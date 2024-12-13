(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_frontend
open Forester_forest

module L = Lsp.Types
(* module F = Analysis.F *)
(* module PT = Analysis.PT *)

let (let*) = Option.bind

(* TODO: handle external links as well? *)
let compute (params : L.DocumentLinkParams.t) =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let config = Compiler.get_config forest in
  let module PT = Plain_text_client.Make(struct
    let route = Iri.to_uri
    let forest = forest
  end) in
  match params with
  | { textDocument; _ } ->
    let Lsp_state.{ forest; _ } = Lsp_state.get () in
    (* TODO: Don't recompute resolver every time*)
    (* let resolver = Compiler.make_resolver server.documents in *)
    (* let resolver = (Tree_resolver.Text_document (Compiler.documents forest)) in *)
    let links =
      match Iri_resolver.(resolve (Uri textDocument.uri) To_code forest) with
      | None -> []
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
                  let range = (Lsp_shims.Loc.lsp_range_of_range node.loc) in
                  let iri = (Iri_scheme.user_iri ~host: config.host addr) in
                  let* target = Iri_resolver.(resolve (Iri iri) To_uri forest) in
                  let* { frontmatter; _ } = Compiler.get_article iri forest in
                  let* tooltip = Option.map PT.string_of_content frontmatter.title in
                  let link =
                    L.DocumentLink.create
                      ~range
                      ~target
                      ~tooltip
                      ()
                  in
                  Some link
                | _ ->
                  None
            )
        end
    in
    Some links
