(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Dx := Datalog_expr
module D := Datalog_engine

val eval_var : Dx.var -> D.term
val eval_const : Vertex.t -> D.term
val eval_term : Vertex.t Dx.term -> D.term
val eval_prop : (string, Vertex.t) Dx.prop -> D.literal
val eval_sequent : (string, Vertex.t) Dx.sequent -> D.clause
val eval_script : (string, Vertex.t) Dx.script -> D.clause list

val run_query : D.db -> (string, Vertex.t) Dx.query -> Vertex_set.t
