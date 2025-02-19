(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** Core types *)

include Base
(**@closed*)

module Iri_tbl = Iri_tbl

module Iri_scheme = Iri_scheme

(** {1 Vertices}

    The type of vertices used by the {{!Forester_core.Forest_graphs}graph database}

    *)

module Vertex = Vertex
module Vertex_set = Vertex_set

(** {1 Error handling}*)

(** {2 Compiler diagnostics and other errors.}*)

module Reporter = Reporter

(** {2 Source locations}
    These are used by compiler diagnostics and the language server.*)

module Range = Range

(** {1 Query system}
    *)

module Query = Query

(** {2 Builtins}*)
module Builtin_relation = Builtin_relation
module Builtin_queries = Builtin_queries

module Forest_graph = Forest_graph
(** The graph type used by the datalog database. Used in particular to track imports, links and transclusions. *)

module Forest_graphs = Forest_graphs
(** The graph database*)

(** {2 Datalog bindings}*)

module Datalog_engine = Datalog_engine
module Datalog_expr = Datalog_expr
module Datalog_eval = Datalog_eval

module Prim = Prim

module TeX_cs = TeX_cs

module Types = Types

module Trie = Trie
