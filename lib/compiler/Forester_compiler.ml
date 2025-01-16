(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** The forester compiler*)

(** {1 Base types }*)

module Code = Code
(** Abstract syntax *)

module Syn = Syn

module Symbol = Symbol

module Xml_forester = Xml_forester
(** Definition of the forester XML schema. This is the compilation target.*)

(** {1 Compilation phases}*)

(** {2 Parsing}

    The lexer and parser are implemented with {{: https://ocaml.org/manual/5.3/lexyacc.html} ocamllex} and {{: https://gallium.inria.fr/~fpottier/menhir/} menhir}*)

module Lexer = Lexer

module Parse = Parse

module Resolver = Resolver

module Imports = Imports
(** Create {{!Forester_core.Forest_graph.t}import and dependency graphs}.
    *)

module Expand = Expand
(** Transform {!Code.tree}s into {!Syn.tree}s by {{!Forester_core.Forest_graph.topo_fold}folding} over the {{!Forester_core.Forest_graph}import graph.}*)

module Eval = Eval
(** Transform {!Syn.tree}s into {{!Forester_core.Types.article}[articles]}.*)

module Legacy_query_engine = Legacy_query_engine
(* [@@ocaml.deprecated "Use Datalog_query_engine instead"] *)

(** {1 High-level architecture}

    The compiler needs to support both batch-style and incremental compilation. To this end, we define a {{!State.t}state type} and {{!Phases}transition functions} that act on this state.

    In the future, we want to record more knowledge in {{!field:State.graphs}[graphs]} of the {{!State.t}state} and derive the information we need for the language server via the query system.

    *)

module Config = Config
(** Configuration*)

module Forest = Forest
(** Augmented hash table used throughout compilation phases.*)

module State = State
module Phases = Phases
module Asset_router = Asset_router

module Iri_resolver = Iri_resolver
(** Maps URIs and IRIs to paths, documents or code*)

module Iri_util = Iri_util

module Diagnostic_store = Diagnostic_store

(** {1 IO}*)

(** {2 LaTeX pipeline}*)

module LaTeX_pipeline = LaTeX_pipeline
module LaTeX_template = LaTeX_template
module Job = Job
(** Definition of LaTeX jobs*)

module Export_for_test = Export_for_test
