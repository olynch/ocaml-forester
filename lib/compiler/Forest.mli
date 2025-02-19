(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

include module type of Iri_tbl

(**/**)
module T = Forester_core.Types
module Dx = Forester_core.Datalog_expr
val legacy_query_engine : (module Forest_graphs.S) -> (module Legacy_query_engine.S)

val execute_datalog_script : (module Forest_graphs.S) -> (string, Vertex.t) Dx.sequent list -> unit
(**/**)

val analyse_resource : (module Forest_graphs.S) -> T.content T.resource -> unit
(** [analyse_resource graphs resource] traverses {{!Forester_core.Types.resource}[resource]}, recording facts about it in {{!Forester_core.Forest_graphs.S}[graphs]}.

    - When encountering a {{!Forester_core.Types.Link}[Link]}, it adds an edge to the {{!Forester_core.Builtin_relation.links_to}[links_to]} relation in {{!Forester_core.Forest_graphs.S}[graphs]}.

    - When encountering a {{!Forester_core.Types.Transclude}[Transclude]}, it adds an edge to the {{!Forester_core.Builtin_relation.transcludes}[transcludes]} relation in {{!Forester_core.Forest_graphs.S}[graphs]}.

    - When encountering a {{!Forester_core.Types.Datalog_script}[Datalog_script script]}, it runs the script and records the results in {{!Forester_core.Forest_graphs.S}[graphs]}.
    *)

val iri_for_resource : 'a T.resource -> Forester_core__.Base.iri option

val get_article : key -> T.content T.resource t -> T.content T.article option

val run_datalog_query :
  (module Forest_graphs.S) -> (string, Forester_core.Vertex.t) Dx.query -> Forester_core.Vertex_set.t

val plant_resource :
  T.content T.resource ->
  (module Forest_graphs.S) ->
  T.content T.resource t ->
  unit

val get_expanded_title :
  ?scope: key ->
  ?flags: T.title_flags ->
  T.content T.frontmatter ->
  T.content T.resource t ->
  T.content

val get_content_of_transclusion :
  T.content T.transclusion -> T.content T.resource t -> T.content option

val get_title_or_content_of_vertex :
  ?not_found: (key -> T.content option) ->
  modifier: T.modifier ->
  T.content T.vertex ->
  T.content T.resource t ->
  T.content option

val get_all_articles :
  'a T.resource t -> 'a T.article list

val get_all_assets :
  'a T.resource t -> T.asset list

val get_all_resources :
  'a t -> 'a list
