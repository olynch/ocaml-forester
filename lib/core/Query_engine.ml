open Base

module type S = sig
  val run_query : Query.dbix Query.expr -> Addr_set.t
end

module Make (Graphs : Forester_graphs.S) : S =
struct
  module Q = Query

  let query_rel mode pol rel addr =
    let fn =
      match pol with
      | Q.Incoming -> Addr_graph.safe_pred
      | Q.Outgoing -> Addr_graph.safe_succ
    in
    let gph = Graphs.get mode rel in
    Addr_set.of_list @@ fn gph addr

  let check_rel mode pol rel addr addr' =
    let gph = Graphs.get mode rel in
    match pol with
    | Q.Incoming -> Addr_graph.mem_edge gph addr' addr
    | Q.Outgoing -> Addr_graph.mem_edge gph addr addr'

  let rec check_query ~env (q : Query.dbix Query.expr) addr =
    match q with
    | Rel (mode, pol, rel, addr_val') ->
      let addr' = eval_addr ~env addr_val' in
      check_rel mode pol rel addr' addr
    | Isect qs -> check_isect ~env qs addr
    | Union qs -> check_union ~env qs addr
    | Complement q ->
      not @@ check_query ~env q addr
    | Union_fam (q, scope) ->
      let xs = Addr_set.to_list @@ run_query ~env q in
      xs |> List.exists @@ fun x ->
      check_query ~env:(x :: env) scope.body addr
    | Isect_fam (q, scope) ->
      let xs = Addr_set.to_list @@ run_query ~env q in
      xs |> List.exists @@ fun x ->
      check_query ~env:(x :: env) scope.body addr

  and eval_addr ~env : Q.dbix Q.addr_expr -> _ =
    function
    | Query.Addr addr -> addr
    | Query.Var ix ->
      begin
        match List.nth_opt env ix with
        | Some addr -> addr
        | None -> Reporter.fatalf Type_error "Bound variable not found in environment when evaluating query"
      end

  and check_isect ~env qs addr =
    qs |> List.for_all @@ fun q ->
    check_query ~env q addr

  and check_isect' qs addr =
    qs |> List.for_all @@ fun (env, q) ->
    check_query ~env q addr

  and check_union ~env qs addr =
    qs |> List.exists @@ fun q ->
    check_query ~env q addr

  and run_query ~env (q : Query.dbix Query.expr) : Addr_set.t =
    match q with
    | Rel (mode, pol, rel, addr_val) ->
      let addr = eval_addr ~env addr_val in
      query_rel mode pol rel addr
    | Isect qs -> run_isect ~env qs
    | Union qs -> run_union ~env qs
    | Complement q ->
      Addr_set.diff (Graphs.get_all_addrs ()) @@ run_query ~env q
    | Union_fam (q, scope) ->
      let xs = Addr_set.to_list @@ run_query ~env q in
      let qs =
        xs |> List.map @@ fun x ->
        x :: env, scope.body
      in
      run_union' qs
    | Isect_fam (q, scope) ->
      let xs = Addr_set.to_list @@ run_query ~env q in
      let qs =
        xs |> List.map @@ fun x ->
        x :: env, scope.body
      in
      run_isect' qs

  and run_isect ~env qs = run_isect' @@ List.map (fun q -> env, q) qs

  and run_union ~env qs = run_union' @@ List.map (fun q -> env, q) qs

  and run_union' qs =
    let alg (env, q) = Addr_set.union (run_query ~env q) in
    List.fold_right alg qs Addr_set.empty

  and run_isect' =
    function
    | [] -> Graphs.get_all_addrs ()
    | (env, q) :: qs ->
      run_query ~env q |> Addr_set.filter @@ check_isect' qs

  and fold_set_operation opr running =
    function
    | [] -> running
    | q :: qs ->
      let s = run_query q in
      fold_set_operation opr (opr running s) qs

  let run_query = run_query ~env:[]
end
