(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler

module T = Types
module L = Lsp.Types

let incoming (params : L.CallHierarchyIncomingCallsParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = State.config forest in
  let module G = (val State.graphs forest) in
  match params with
  | {item; _} ->
    let vertex_to_item (v : _ T.vertex) =
      let from = item in
      let fromRanges = [] in
      match v with
      | T.Iri_vertex _ -> L.CallHierarchyIncomingCall.create ~from ~fromRanges
      | T.Content_vertex _ -> L.CallHierarchyIncomingCall.create ~from ~fromRanges
    in
    match item with
    | {uri; _} ->
      let iri = Iri_scheme.path_to_iri ~host: config.host (Lsp.Uri.to_path uri) in
      let vertex = T.Iri_vertex iri in
      let run_query = Forest.run_datalog_query (State.graphs forest) in
      let fwdlinks = run_query @@ Builtin_queries.fwdlinks_datalog vertex in
      Eio.traceln "got %i link items" (Vertex_set.cardinal fwdlinks);
      let children = run_query @@ Builtin_queries.children_datalog vertex in
      Eio.traceln "got %i transclusion items" (Vertex_set.cardinal children);
      let items = Vertex_set.union fwdlinks children |> Vertex_set.to_list |> List.map vertex_to_item in
      Some items

let outgoing (params : L.CallHierarchyOutgoingCallsParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  let config = State.config forest in
  let module G = (val State.graphs forest) in
  Eio.traceln "computing outgoing calls";
  match params with
  | {item; _} ->
    let vertex_to_item (v : _ T.vertex) =
      let to_ = item in
      let fromRanges = [] in
      match v with
      | T.Iri_vertex _ -> L.CallHierarchyOutgoingCall.create ~to_ ~fromRanges
      | T.Content_vertex _ -> L.CallHierarchyOutgoingCall.create ~to_ ~fromRanges
    in
    match item with
    | {uri; _} ->
      let iri = Iri_scheme.path_to_iri ~host: config.host (Lsp.Uri.to_path uri) in
      let vertex = T.Iri_vertex iri in
      let run_query = Forest.run_datalog_query (State.graphs forest) in
      let backlinks = run_query @@ Builtin_queries.backlinks_datalog vertex in
      Eio.traceln "got %i link items" (Vertex_set.cardinal backlinks);
      let parents = run_query @@ Builtin_queries.context_datalog vertex in
      Eio.traceln "got %i transclusion items" (Vertex_set.cardinal parents);
      let items = Vertex_set.union backlinks parents |> Vertex_set.to_list |> List.map vertex_to_item in
      Some items

let compute (params : L.CallHierarchyPrepareParams.t) =
  let Lsp_state.{forest; _} = Lsp_state.get () in
  match params with
  | {position; textDocument; _} ->
    let iri = Iri_scheme.uri_to_iri ~host: forest.config.host textDocument.uri in
    match Imports.resolve_iri_to_code iri forest with
    | None -> None
    | Some tree ->
      let item =
        match Analysis.node_at ~position Code.(tree.code) with
        | None -> None
        | Some {loc = _; value} ->
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
