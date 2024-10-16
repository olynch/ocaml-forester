open Forester_prelude

module Dx = Datalog_expr
module D = Datalog_engine

let int_of_var = String.hash

let eval_var (var : Dx.var) =
  D.mk_var @@ int_of_var var

let eval_const (vtx : Vertex.t) =
  D.mk_const @@ Vertex.pack vtx

let eval_term : _ Dx.term -> D.term = function
  | Dx.Var var -> eval_var var
  | Dx.Const c -> eval_const c

let eval_prop (prop : _ Dx.prop) : D.literal =
  let rel = D.StringSymbol.make prop.rel in
  let args = List.map eval_term prop.args in
  D.mk_literal rel args

let eval_premises = List.map eval_prop

let eval_sequent (sequent : _ Dx.sequent) : D.clause =
  D.mk_clause (eval_prop sequent.conclusion) @@
    eval_premises sequent.premises

let eval_script : _ Dx.script -> D.clause list =
  List.map eval_sequent

let run_query (db : D.db) (query : (string, Vertex.t) Dx.query) : Vertex_set.t =
  let answers =
    D.Query.ask db ~neg: (eval_premises query.negatives) [|int_of_var query.var|] @@ eval_premises query.positives
  in
  Vertex_set.of_list @@
    let@ answer = List.filter_map @~ D.Query.to_list answers in
    let item = Array.get answer 0 in
    match item with
    | Const sym -> Vertex.unpack sym
    | _ -> None
