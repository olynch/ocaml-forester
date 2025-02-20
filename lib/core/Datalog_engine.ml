(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module T = Types
module V = struct
  include Vertex
  let to_string = show
end

module S = Datalog.BottomUp.Hashcons(V)
module D = Datalog.BottomUp.Make(S)

type relation = string
type vertex = Vertex.t
type term = D.term
type literal = D.literal
type clause = D.clause
type var = int

let symbol_of_string str : D.symbol =
  let vtx = T.Content_vertex (T.Content [T.Text str]) in
  S.make vtx

let pack_vertex vtx : D.symbol = S.make vtx

let var_of_string = String.hash

let mk_var = D.mk_var
let mk_const vtx = D.mk_const (pack_vertex vtx)
let mk_literal rel = D.mk_literal (symbol_of_string rel)
let mk_clause = D.mk_clause

let vertex_of_term = function
  | D.Const x -> (x : S.t :> Vertex.t)
  | _ -> failwith "const_of_term: unexpected variable"

type db = D.db
let db_create = D.db_create
let db_add_fact db lit = D.db_add_fact db lit
let db_add db clause = D.db_add db clause

type answers = D.Query.set

let list_of_answers answers =
  List.map (Array.map vertex_of_term) @@
    D.Query.to_list answers

let ask = D.Query.ask
