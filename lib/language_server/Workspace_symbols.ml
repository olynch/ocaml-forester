(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_frontend
open Forester_compiler

(* module State = Analysis.State *)
module L = Lsp.Types
module Unit_map = Forester_compiler.Expand.Unit_map

let compute
    ({ query = _; _ }: L.WorkspaceSymbolParams.t)
  =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  (* let config = State.config forest in *)
  let render = Render.render ~dev: true forest STRING in
  let trees =
    forest
    |> State.parsed
    |> Forest.to_seq_keys
    |> Seq.filter_map
      (
        fun iri ->
          let title =
            match Forest.get_article iri forest.resources with
            | None -> "untitled"
            | Some { frontmatter; _ } ->
              begin
                match frontmatter.title with
                | None -> "untitled"
                | Some content -> render (Content content)
              end
          in
          (* match Option.map (fun tree -> Code.(tree.source_path)) @@ *)
          (*   Forest.find_opt (Compiler.parsed forest) iri with *)
          let uri =
            let (let*) = Option.bind in
            let* tree = Forest.find_opt (State.parsed forest) iri in
            let* source_path = tree.source_path in
            Some (Lsp.Uri.of_path source_path)
          in
          match uri with
          | None -> None
          | Some uri ->
            let location =
              L.Location.{
                range = L.Range.{
                  end_ = { character = 0; line = 0; };
                  start = { character = 0; line = 0; };
                };
                uri;
              }
            in
            Some (L.SymbolInformation.create ~kind: File ~location ~name: title ())
      )
    |> List.of_seq
  in
  let symbols =
    let open Forester_compiler in
    forest
    |> State.units
    |> Unit_map.to_seq
    |> Seq.concat_map
      (
        fun ((iri, exports): iri * _) ->
          exports
          |> Trie.to_seq
          |> Seq.filter_map
            (
              fun (path, data) ->
                let location =
                  let range =
                    snd data
                    |> Lsp_shims.Loc.lsp_range_of_range
                  in
                  (* let iri = Iri_scheme.user_iri ~host: config.host addr in *)
                  let uri =
                    let (let*) = Option.bind in
                    let* tree = Forest.find_opt (State.parsed forest) iri in
                    let* source_path = tree.source_path in
                    Some (Lsp.Uri.of_path source_path)
                  in
                  match uri with
                  | Some uri ->
                    Some (L.Location.{ range; uri })
                  | None -> None
                in
                let kind =
                  match data with
                  | (Resolver.P.Xmlns _, _loc) ->
                    L.SymbolKind.File
                  | (Resolver.P.Term syn, _loc) ->
                    match (List.hd syn).value with
                    | Syn.Fun (_, _) -> Function
                    | Syn.Text _ -> String
                    | Syn.Verbatim _ -> String
                    | Syn.Var _ -> Variable
                    | Syn.Title
                    | Syn.Taxon
                    | Syn.Meta
                    | Syn.Attribution (_, _)
                    | Syn.Tag _
                    | Syn.Date
                    | Syn.Number ->
                      Field
                    | Syn.Object _ -> Object
                    | Syn.Group (_, _)
                    | Syn.Math (_, _)
                    | Syn.Link _
                    | Syn.Subtree (_, _)
                    | Syn.Sym _
                    | Syn.Put (_, _, _)
                    | Syn.Default (_, _, _)
                    | Syn.Get _
                    | Syn.Xml_tag (_, _, _)
                    | Syn.TeX_cs _
                    | Syn.Prim _
                    | Syn.Patch _
                    | Syn.Call (_, _)
                    | Syn.Query_polarity _
                    | Syn.Query_mode _
                    | Syn.Results_of_query
                    | Syn.Query_rel _
                    | Syn.Query_isect
                    | Syn.Query_union
                    | Syn.Query_compl
                    | Syn.Query_isect_fam
                    | Syn.Query_union_fam
                    | Syn.Query_isect_fam_rel
                    | Syn.Query_union_fam_rel
                    | Syn.Query_builtin (_, _)
                    | Syn.Transclude
                    | Syn.Embed_tex
                    | Syn.Ref
                    | Syn.Parent
                    | Syn.Dx_sequent (_, _)
                    | Syn.Dx_query (_, _, _)
                    | Syn.Dx_prop (_, _)
                    | Syn.Dx_var _
                    | Syn.Dx_const (_, _)
                    | Syn.Dx_execute
                    | Syn.Route_asset
                    | Syn.Publish_results_of_query ->
                      L.SymbolKind.File
                in
                match location with
                | None -> None
                | Some location ->
                  Some
                    (
                      L.SymbolInformation.create
                        ~kind
                        ~location
                        ~name: (Format.asprintf "%a" Resolver.Scope.pp_path path)
                        ()
                    )
            )
      )
    |> List.of_seq
  in
  Some (trees @ symbols)
