(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_frontend
open Forester_compiler

module L = Lsp.Types

let (let*) = Option.bind

(* TODO: handle external links as well? *)
let compute (params : L.DocumentLinkParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let render = Render.render ~dev: true forest STRING in
  let config = State.config forest in
  match params with
  | {textDocument; _} ->
    let Lsp_state.{forest; _} = Lsp_state.get () in
    let links =
      let iri = Iri_scheme.uri_to_iri ~host: forest.config.host textDocument.uri in
      match Imports.resolve_iri_to_code iri forest with
      | None -> []
      | Some tree ->
        begin
          Code.(tree.code)
          |> List.filter_map
              (fun node ->
                match Range.(node.value) with
                | Code.Group (Squares, [{value = Text addr; _}])
                | Code.Group (Parens, [{value = Text addr; _}])
                | Code.Group (Braces, [{value = Text addr; _}]) ->
                  (* TODO: Need to analyse syn *)
                  let range = (Lsp_shims.Loc.lsp_range_of_range node.loc) in
                  let iri = (Iri_scheme.user_iri ~host: config.host addr) in
                  let* target = Option.map Lsp.Uri.of_path @@ Iri_tbl.find_opt forest.resolver iri in
                  let* {frontmatter; _} = Forest.get_article iri forest.resources in
                  let* tooltip = Option.map (fun c -> render (Content c)) frontmatter.title in
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
