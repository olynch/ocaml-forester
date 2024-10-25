(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude

module Dx = Datalog_expr
module D = Datalog_engine

let eval_var (var : Dx.var) =
  D.mk_var (D.var_of_string var)

let eval_const (vtx : Vertex.t) =
  D.mk_const vtx

let eval_term : _ Dx.term -> D.term = function
  | Dx.Var var -> eval_var var
  | Dx.Const c -> eval_const c

let eval_prop (prop : _ Dx.prop) : D.literal =
  let args = List.map eval_term prop.args in
  D.mk_literal prop.rel args

let eval_premises = List.map eval_prop

let eval_sequent (sequent : _ Dx.sequent) : D.clause =
  D.mk_clause (eval_prop sequent.conclusion) @@
    eval_premises sequent.premises

let eval_script : _ Dx.script -> D.clause list =
  List.map eval_sequent

let run_query (db : D.db) (query : (string, Vertex.t) Dx.query) : Vertex_set.t =
  let answers =
    D.ask
      db
      ~neg: (eval_premises query.negatives)
      [|D.var_of_string query.var|] @@
      eval_premises query.positives
  in
  Vertex_set.of_list @@
    let@ answer = List.map @~ D.list_of_answers answers in
    answer.(0)
