(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_compiler
open Forester_forest

module L = Lsp.Types

let (let*) = Option.bind

let pp_path = Resolver.Scope.pp_path

let compute (params : L.DocumentSymbolParams.t) =
  match params with
  | { textDocument; _ } ->
    let Lsp_state.{ forest; _ } = Lsp_state.get () in
    match Iri_resolver.(resolve (Uri textDocument.uri) To_code forest) with
    | None -> None
    | Some { code; _ } ->
      let symbols =
        List.filter_map
          (
            fun
                Asai.Range.{ loc; value }
              ->
              let open Code in
              let range = Lsp_shims.Loc.lsp_range_of_range loc in
              let selectionRange = range in
              match value with
              | Subtree (addr, _) ->
                let name =
                  match addr with
                  | Some addr -> addr
                  | None -> "anonymous"
                in
                let range = Lsp_shims.Loc.lsp_range_of_range loc in
                (* TODO: What should the symbol kind of a subtree be? *)
                Some (L.DocumentSymbol.create ~name ~range ~selectionRange ~kind: Namespace ())
              | Object { self; _ } ->
                let name =
                  match self with
                  | Some path -> Format.asprintf "%a" pp_path path
                  | None -> "anonymous"
                in
                Some (L.DocumentSymbol.create ~name ~range ~selectionRange ~kind: Object ())
              | Def (name, _, _) ->
                let name = Format.asprintf "%a" pp_path name in
                Some
                  (
                    L.DocumentSymbol.create ~name ~range ~selectionRange ~kind: Function ()
                  )
              | Namespace (name, _) ->
                let name = Format.asprintf "%a" pp_path name in
                Some
                  (
                    L.DocumentSymbol.create ~name ~range ~selectionRange ~kind: Namespace ()
                  )
              | Ident path ->
                let name = Format.asprintf "%a" pp_path path in
                Some
                  (
                    L.DocumentSymbol.create ~name ~range ~selectionRange ~kind: Constructor ()
                  )
              | Let (path, _, _) ->
                let name = Format.asprintf "%a" pp_path path in
                Some
                  (
                    L.DocumentSymbol.create ~name ~range ~selectionRange ~kind: Constructor ()
                  )
              | Xml_ident (pfx, ident) ->
                let name =
                  match pfx with
                  | None -> Format.asprintf "<%s>" ident
                  | Some pfx ->
                    Format.asprintf "<%s:%s>" pfx ident
                in
                Some
                  (
                    L.DocumentSymbol.create ~name ~range ~selectionRange ~kind: Constructor ()
                  )
              | Hash_ident _ ->
                Some
                  (
                    L.DocumentSymbol.create ~name: "(hash)" ~range ~selectionRange ~kind: Constant ()
                  )
              | Text _
              | Verbatim _
              | Group (_, _)
              | Math (_, _)
              | Open _
              | Scope _
              | Put (_, _)
              | Default (_, _)
              | Get _
              | Fun (_, _)
              | Patch _
              | Call (_, _)
              | Import (_, _)
              | Decl_xmlns (_, _)
              | Alloc _
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
