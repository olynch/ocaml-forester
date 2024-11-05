(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_compiler

module L = Lsp.Types
module F = Analysis.F
module PT = Analysis.PT

let (let*) = Option.bind

(* TODO: handle external links as well? *)
let compute (params : L.DocumentSymbolParams.t) =
  match params with
  | { textDocument; _ } ->
    let server = State.get () in
    match Hashtbl.find_opt server.index.codes textDocument with
    | None -> None
    | Some { code; _ } ->
      let symbols =
        List.filter_map
          (
            fun
                Asai.Range.{ loc; value }
              ->
              let open Code in
              match value with
              | Subtree (addr, _) ->
                let name =
                  match addr with
                  | Some addr -> addr
                  | None -> "anonymous"
                in
                let range = LspShims.Loc.lsp_range_of_range loc in
                Some (L.DocumentSymbol.create ~name ~range ~selectionRange: range ~kind: Namespace ())
              | Text _
              | Verbatim _
              | Group (_, _)
              | Math (_, _)
              | Ident _
              | Hash_ident _
              | Xml_ident (_, _)
              | Let (_, _, _)
              | Open _
              | Scope _
              | Put (_, _)
              | Default (_, _)
              | Get _
              | Fun (_, _)
              | Object _
              | Patch _
              | Call (_, _)
              | Import (_, _)
              | Def (_, _, _)
              | Decl_xmlns (_, _)
              | Alloc _
              | Namespace (_, _)
              | Dx_sequent (_, _)
              | Dx_query (_, _, _)
              | Dx_prop (_, _)
              | Dx_var _
              | Dx_const_content _
              | Dx_const_iri _
              | Comment _
              | Error _ ->
                None
          )
          code
      in
      Some (`DocumentSymbol symbols)
