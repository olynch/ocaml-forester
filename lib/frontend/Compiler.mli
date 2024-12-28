(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_forest

(**/**)
module T = Types
type resource = T.content T.resource
(**/**)

(** {1 The compiler frontend}*)

(** Forester source code is processed in three phases:

    {!modules: Forester_compiler.Parse Forester_compiler.Expand Forester_compiler.Eval}

    When a text document is {{!Forester_lsp.Server.Handlers.Did_open}opened} or {{!Forester_lsp.Server.Handlers.Did_change}changed}, the language server parses, expands and evaluates the changed source, accumulating the diagnostics for each phase and finally publishing them to the client.
*)

type state = Forester_forest.State.t


val documents : state -> (Lsp.Uri.t, Lsp.Text_document.t) Hashtbl.t
val parsed : state -> Code.tree Forest.t
val resources : state -> resource Forest.t
val expanded : state -> Syn.tree Forest.t
val graphs : state -> (module Forest_graphs.S)
val get_config : state -> Config.Forest_config.t
val get_env : state -> Eio_unix.Stdenv.base
val get_diagnostics : state -> Forester_forest.Diagnostics.table
val units : state -> Expand.Env.t

val with_config : Config.Forest_config.t -> state -> state
val with_expanded : Syn.t Forest.t -> state -> state

val init : env: Eio_unix.Stdenv.base -> config: Config.Forest_config.t -> state

(** {2 Compilation phases}*)

(** [load ~env config] loads all trees contained in [config.trees] into a [Lsp.Text_document.t] {!type:forest} *)
val load :
  Eio.Fs.dir_ty Eio.Path.t list ->
  state ->
  state

(** [parse ~host config] parses all trees contained in [raw_forest] into a {{!type:Forester_compiler.Code.tree}[Code.tree]} {!type:forest} *)
val parse : quit_on_error: bool -> state -> state

(** [reparse ~host doc forest] parses [doc] and amends the import graph in [forest].*)
val reparse : Lsp.Text_document.t -> state -> state

(** [build_import_graph forest] creates the import graph of [forest] by traversing the trees in [forest]. *)
val build_import_graph : state -> state
val build_import_graph_for : addr: iri -> state -> state

(** [expand  forest] will expand all unit paths in [forest], accumulating the diagnostics. *)
val expand : quit_on_error: bool -> state -> state

val expand_only :
  (* ?quit_on_error:bool -> *)
  (* host:string -> *)
  (* addr:iri -> *)
  (* Tree_resolver.t -> *)
  (* Expand.Env.t * (Lsp.Uri.t, Reporter.diagnostic list) Hashtbl.t * Syn.t Forest.t *)
  iri -> state -> state

(** [eval  forest] will evaluate all trees in [forest]. *)
val eval : state -> state

val get_article : iri -> state -> T.content T.article option

val get_all_resources : state -> resource list

val get_all_articles : state -> T.content T.article list

(* val plant : iri -> 'a -> 'a forest -> unit *)

val plant_resource : resource -> state -> unit

val get_content_of_transclusion : T.content T.transclusion -> state -> T.content option

val get_title_or_content_of_vertex :
  ?not_found: (iri -> T.content option) ->
  modifier: T.modifier ->
  T.content T.vertex ->
  state ->
  T.content option

val get_expanded_title :
  ?scope: iri ->
  ?flags: T.title_flags ->
  T.content T.frontmatter ->
  state ->
  T.content

