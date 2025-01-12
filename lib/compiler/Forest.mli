(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T = Forester_core.Types
module Dx = Forester_core.Datalog_expr

val iri_for_resource : 'a T.resource -> Forester_core__.Base.iri option

type env = (module Forest_graphs.S)

include Hashtbl.S with type key = iri

val legacy_query_engine : env -> (module Legacy_query_engine.S)
[@@@ocaml.deprecated "The legacy query engine is deprecated"]

val analyse_resource : env -> T.content T.resource -> unit

val run_datalog_query :
  (module Forest_graphs.S) -> (string, Forester_core.Vertex.t) Dx.query -> Forester_core.Vertex_set.t

val get_article : key -> T.content T.resource t -> T.content T.article option

val plant_resource :
  T.content T.resource ->
  (module Forest_graphs.S) ->
  T.content T.resource t ->
  unit

val get_expanded_title :
  ?scope:key ->
  ?flags:T.title_flags ->
  T.content T.frontmatter ->
  T.content T.resource t ->
  T.content

val get_content_of_transclusion :
  T.content T.transclusion -> T.content T.resource t -> T.content option

val get_title_or_content_of_vertex :
  ?not_found:(key -> T.content option) ->
  modifier:T.modifier ->
  T.content T.vertex -> T.content T.resource t -> T.content option

val get_all_articles :
  'a T.resource t -> 'a T.article list

val get_all_resources : 
  'a t -> 'a list
