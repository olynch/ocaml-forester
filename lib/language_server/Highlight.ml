(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_forest

module L = Lsp.Types
module T = Types

let compute (params : L.DocumentHighlightParams.t) =
  match params with
  | { textDocument; _ } ->
    let Lsp_state.{ forest; _ } = Lsp_state.get () in
    match Iri_resolver.(resolve (Uri textDocument.uri) To_code forest) with
    | None -> None
    | Some tree ->
      let highlights =
        tree.code
        |> List.filter_map
          (
            fun
                Range.{ loc; value }
              ->
              let range = Lsp_shims.Loc.lsp_range_of_range loc in
              let kind =
                match value with
                | Code.Text _
                | Code.Verbatim _
                | Code.Group (_, _)
                | Code.Math (_, _)
                | Code.Ident _
                | Code.Hash_ident _
                | Code.Xml_ident (_, _)
                | Code.Subtree (_, _)
                | Code.Let (_, _, _)
                | Code.Open _
                | Code.Scope _
                | Code.Put (_, _)
                | Code.Default (_, _)
                | Code.Get _
                | Code.Fun (_, _)
                | Code.Object _
                | Code.Patch _
                | Code.Call (_, _)
                | Code.Import (_, _)
                | Code.Def (_, _, _)
                | Code.Decl_xmlns (_, _)
                | Code.Alloc _
                | Code.Namespace (_, _)
                | Code.Dx_sequent (_, _)
                | Code.Dx_query (_, _, _)
                | Code.Dx_prop (_, _)
                | Code.Dx_var _
                | Code.Dx_const_content _
                | Code.Dx_const_iri _
                | Code.Comment _
                | Code.Error _ ->
                  None
              in
              Some (L.DocumentHighlight.create ~range ?kind ())
          )
      in
      Some highlights
