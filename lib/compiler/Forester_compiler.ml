(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** This library implements the compiler for Forester.
    *)

module Code = Code
(** Abstract syntax *)

module Syn = Syn

module Symbol = Symbol

(***)

module Resolver = Resolver
module Expand = Expand
module Eval = Eval
module Parse = Parse
module Lexer = Lexer

module Xml_forester = Xml_forester
(** Definition of the forester XML schema*)

module Forest_graph = Forest_graph
(** The graph type used by the datalog database *)

module Forest_graphs = Forest_graphs

module Imports = Imports
(** Analysis of the abstract syntax to create import and dependency graphs *)

module Config = Config
(** Configuration*)

module Forest = Forest
(**)

module State = State
module Phases = Phases
module Asset_router = Asset_router
module Legacy_query_engine = Legacy_query_engine
[@@ocaml.deprecated "Use Datalog_query_engine instead"]

module Iri_resolver = Iri_resolver
module Iri_util = Iri_util

module Diagnostic_store = Diagnostic_store

module Job = Job
(** Definition of LaTeX jobs*)

module LaTeX_pipeline = LaTeX_pipeline
module LaTeX_template = LaTeX_template

module Export_for_test = Export_for_test
