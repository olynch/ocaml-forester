(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

module type S = sig
  val run_query : Types.query -> Vertex_set.t
end

module Make (Graphs: Forest_graphs.S) : S = struct
  module Q = Query

  let eval_vertex ~env : (_, Q.dbix) Q.vertex_expr -> _ = function
    | Query.Vertex vertex -> vertex
    | Query.Var ix ->
      begin
        match List.nth_opt env ix with
        | Some vtx -> vtx
        | None -> Reporter.fatalf Type_error "Bound variable not found in environment when evaluating query"
      end

  let query_rel mode pol rel vtx =
    let fn =
      match pol with
      | Q.Incoming -> Forest_graph.safe_pred
      | Q.Outgoing -> Forest_graph.safe_succ
    in
    let gph = Graphs.get_rel mode rel in
    Vertex_set.of_list @@ fn gph vtx

  let check_rel mode pol rel vtx vtx' =
    let gph = Graphs.get_rel mode rel in
    match pol with
    | Q.Incoming -> Forest_graph.mem_edge gph vtx' vtx
    | Q.Outgoing -> Forest_graph.mem_edge gph vtx vtx'

  let rec check_query ~env (q : Types.query) vtx =
    match q with
    | Rel (mode, pol, rel, vtx_val') ->
      let vtx' = eval_vertex ~env vtx_val' in
      check_rel mode pol rel vtx' vtx
    | Isect qs -> check_isect ~env qs vtx
    | Union qs -> check_union ~env qs vtx
    | Complement q ->
      not @@ check_query ~env q vtx
    | Union_fam (q, scope) ->
      let xs = Vertex_set.to_list @@ run_query ~env q in
      let@ x = List.exists @~ xs in
      check_query ~env: (x :: env) scope.body vtx
    | Isect_fam (q, scope) ->
      let xs = Vertex_set.to_list @@ run_query ~env q in
      let@ x = List.for_all @~ xs in
      check_query ~env: (x :: env) scope.body vtx

  and check_isect ~env qs vtx =
    let@ q = List.for_all @~ qs in
    check_query ~env q vtx

  and check_isect' qs vtx =
    let@ env, q = List.for_all @~ qs in
    check_query ~env q vtx

  and check_union ~env qs vtx =
    let@ q = List.exists @~ qs in
    check_query ~env q vtx

  and run_query ~env (q : Types.query) : Vertex_set.t =
    match q with
    | Rel (mode, pol, rel, vtx_val) ->
      let vtx = eval_vertex ~env vtx_val in
      query_rel mode pol rel vtx
    | Isect qs -> run_isect ~env qs
    | Union qs -> run_union ~env qs
    | Complement q ->
      Vertex_set.diff (Graphs.get_all_vertices ()) @@ run_query ~env q
    | Union_fam (q, scope) ->
      let xs = Vertex_set.to_list @@ run_query ~env q in
      let qs =
        let@ x = List.map @~ xs in
        x :: env, scope.body
      in
      run_union' qs
    | Isect_fam (q, scope) ->
      let xs = Vertex_set.to_list @@ run_query ~env q in
      let qs =
        let@ x = List.map @~ xs in
        x :: env, scope.body
      in
      run_isect' qs

  and run_isect ~env qs = run_isect' @@ List.map (fun q -> env, q) qs

  and run_union ~env qs = run_union' @@ List.map (fun q -> env, q) qs

  and run_union' qs =
    let alg (env, q) = Vertex_set.union (run_query ~env q) in
    List.fold_right alg qs Vertex_set.empty

  and run_isect' = function
    | [] -> Graphs.get_all_vertices ()
    | (env, q) :: qs ->
      run_query ~env q |> Vertex_set.filter @@ check_isect' qs

  let run_query = run_query ~env: []
end
