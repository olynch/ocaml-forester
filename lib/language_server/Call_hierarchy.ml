(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_forest

module T = Types
module L = Lsp.Types
module F = Analysis.F
module PT = Analysis.PT
module G = Analysis.G

let incoming (params : L.CallHierarchyIncomingCallsParams.t) =
  let server = State.get () in
  match params with
  | { item; _ } ->
    let vertex_to_item (v : _ T.vertex) =
      let from = item in
      let fromRanges = [] in
      match v with
      | T.Iri_vertex _ -> L.CallHierarchyIncomingCall.create ~from ~fromRanges
      | T.Content_vertex _ -> L.CallHierarchyIncomingCall.create ~from ~fromRanges
    in
    match item with
    | { uri; _ } ->
      let iri = Util.path_to_iri ~host: server.config.host (Lsp.Uri.to_path uri) in
      let vertex = T.Iri_vertex iri in
      let transclusion_graph = G.get_rel Query.Edges Builtin_relation.transcludes in
      let link_graph = G.get_rel Query.Edges Builtin_relation.links_to in
      let link_items = Forest_graph.safe_succ link_graph vertex |> List.map vertex_to_item in
      let transclusion_items = Forest_graph.safe_succ transclusion_graph vertex |> List.map vertex_to_item in
      Some (link_items @ transclusion_items)

let outgoing (params : L.CallHierarchyOutgoingCallsParams.t) =
  let server = State.get () in
  Eio.traceln "computing outgoing calls";
  match params with
  | { item; _ } ->
    let vertex_to_item (v : _ T.vertex) =
      let to_ = item in
      let fromRanges = [] in
      match v with
      | T.Iri_vertex _ -> L.CallHierarchyOutgoingCall.create ~to_ ~fromRanges
      | T.Content_vertex _ -> L.CallHierarchyOutgoingCall.create ~to_ ~fromRanges
    in
    match item with
    | { uri; _ } ->
      let iri = Util.path_to_iri ~host: server.config.host (Lsp.Uri.to_path uri) in
      let vertex = T.Iri_vertex iri in
      let transclusion_graph = G.get_rel Query.Edges Builtin_relation.transcludes in
      let link_graph = G.get_rel Query.Edges Builtin_relation.links_to in
      let link_items = Forest_graph.safe_pred link_graph vertex |> List.map vertex_to_item in
      Eio.traceln "got %i link items" (List.length link_items);
      let transclusion_items = Forest_graph.safe_pred transclusion_graph vertex |> List.map vertex_to_item in
      Eio.traceln "got %i transclusion items" (List.length transclusion_items);
      Some (link_items @ transclusion_items)

let compute (params : L.CallHierarchyPrepareParams.t) =
  let server = State.get () in
  match params with
  | { position; textDocument; _ } ->
    match Hashtbl.find_opt server.index.codes { uri = textDocument.uri } with
    | None -> None
    | Some tree ->
      let item =
        match Analysis.node_at ~position tree.code with
        | None -> None
        | Some { loc = _; value } ->
          match value with
          | Code.Def (_, _, _)
          | Code.Fun (_, _) ->
            None
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
          | Code.Object _
          | Code.Patch _
          | Code.Call (_, _)
          | Code.Import (_, _)
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
      Option.map (fun item -> [item]) item
