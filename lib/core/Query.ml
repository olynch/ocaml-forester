open Base

module Set = struct
  type t = string
  [@@deriving repr]

  let pp = Format.pp_print_string

  let make_builtin name = "org.forester.set." ^ name

  let references = make_builtin "references"
  let people = make_builtin "people"
end

module Rel = struct
  type t = string
  [@@deriving repr]

  let pp = Format.pp_print_string

  let make_builtin name = "org.forester.rel." ^ name

  let links = make_builtin "links"
  let transclusion = make_builtin "transclusion"
  let authors = make_builtin "authors"
  let contributors = make_builtin "contributors"
  let taxa = make_builtin "taxa"
  let tags = make_builtin "tags"
end

type rel = Rel.t

type mode =
  | Edges
  | Paths
[@@deriving show, repr]

type polarity =
  | Incoming
  | Outgoing
[@@deriving show, repr]

type dbix = int
[@@deriving show, repr]

type ('vertex, 'var) vertex_expr =
  | Vertex of 'vertex
  | Var of 'var
[@@deriving show, repr]

type 'a binder = { body: 'a }
[@@deriving show, repr]

type ('vertex, 'var) expr =
  | Rel of mode * polarity * Rel.t * ('vertex, 'var) vertex_expr
  | Isect of ('vertex, 'var) expr list
  | Union of ('vertex, 'var) expr list
  | Complement of ('vertex, 'var) expr
  | Union_fam of ('vertex, 'var) expr * ('vertex, 'var) expr binder
  | Isect_fam of ('vertex, 'var) expr * ('vertex, 'var) expr binder
[@@deriving show]

let expr_t vertex_t var_t =
  let open Repr in
  mu @@
    fun expr_t ->
      variant
        "expr"
        begin
          fun rel isect union complement union_fam isect_fam ->
            function
            | Rel (x1, x2, x3, x4) -> rel (x1, x2, x3, x4)
            | Isect x -> isect x
            | Union x -> union x
            | Complement x -> complement x
            | Union_fam (x, y) -> union_fam (x, y)
            | Isect_fam (x, y) -> isect_fam (x, y)
        end
      |~ case1
        "Rel"
        (quad mode_t polarity_t Rel.t (vertex_expr_t vertex_t var_t))
        (fun (x1, x2, x3, x4) -> Rel (x1, x2, x3, x4))
      |~ case1 "Isect" (list expr_t) (fun x -> Isect x)
      |~ case1 "Union" (list expr_t) (fun x -> Union x)
      |~ case1 "Complement" expr_t (fun x -> Complement x)
      |~ case1
        "Union_fam"
        (pair expr_t (binder_t expr_t))
        (fun (x, y) -> Union_fam (x, y))
      |~ case1
        "Isect_fam"
        (pair expr_t (binder_t expr_t))
        (fun (x, y) -> Isect_fam (x, y))
      |> sealv

(** A heuristic for computing an intersection of queries. *)
let rec query_cost q =
  match q with
  | Rel _ -> 1
  | Isect qs ->
    List.fold_left (fun i q -> min (query_cost q) i) 1000 qs
  | Union qs ->
    List.fold_left (fun i q -> max (query_cost q) i) 0 qs
  | Union_fam (q, scope) ->
    query_cost q * query_cost scope.body
  | Isect_fam (q, scope) ->
    query_cost q * query_cost scope.body
  | Complement _ -> 900

let sort_by_ascending_cost qs =
  qs
  |> List.sort @@
    fun q0 q1 ->
      compare (query_cost q0) (query_cost q1)

let sort_by_descending_cost qs =
  qs
  |> List.sort @@
    fun q0 q1 ->
      compare (query_cost q1) (query_cost q0)

let rel mode pol rel a =
  Rel (mode, pol, rel, a)

let rec isect qs =
  match sort_by_ascending_cost qs with
  | Isect qs :: qs' -> isect @@ qs @ qs'
  | qs -> Isect qs

let rec union qs =
  match sort_by_descending_cost qs with
  | Union qs :: qs' -> union @@ qs @ qs'
  | qs -> Union qs

let rec complement = function
  | Union qs -> isect @@ List.map complement qs
  | Complement q -> q
  | q -> Complement q

type 'name lnvar =
  | F of 'name
  | B of dbix
[@@deriving show]

let rec close_expr k x = function
  | Rel (mode, pol, rel, a) -> Rel (mode, pol, rel, close_vertex_expr k x a)
  | Isect qs -> Isect (List.map (close_expr k x) qs)
  | Union qs -> Union (List.map (close_expr k x) qs)
  | Complement q -> Complement (close_expr k x q)
  | Union_fam (q, scope) -> Union_fam (close_expr k x q, close_scope k x scope)
  | Isect_fam (q, scope) -> Isect_fam (close_expr k x q, close_scope k x scope)

and close_scope k x scope =
  { body = close_expr (k + 1) x scope.body }

and close_vertex_expr k x = function
  | Vertex vertex -> Vertex vertex
  | Var var -> Var (close_vertex_var k x var)

and close_vertex_var k x = function
  | F name when x = name -> B k
  | F name -> F name
  | B i when i < k -> B i
  | B i -> B (i + 1)

module type Name = sig
  type t
  val fresh : unit -> t
end

module Global_name: Name = struct
  type t = int
  let fresh () = Oo.id object end
end

module Locally_nameless (N: Name) = struct
  type 'vertex lnexpr = ('vertex, N.t lnvar) expr

  exception Distill of N.t

  let rec distill : _ expr -> ('vertex, dbix) expr = function
    | Rel (mode, pol, rel, a) -> Rel (mode, pol, rel, distill_vertex_expr a)
    | Isect qs -> Isect (List.map distill qs)
    | Union qs -> Union (List.map distill qs)
    | Complement q -> Complement (distill q)
    | Union_fam (q, scope) -> Union_fam (distill q, distill_scope scope)
    | Isect_fam (q, scope) -> Isect_fam (distill q, distill_scope scope)

  and distill_scope scope =
    { body = distill scope.body }

  and distill_vertex_expr = function
    | Vertex vertex -> Vertex vertex
    | Var var -> Var (distill_lnvar var)

  and distill_lnvar = function
    | F name -> raise @@ Distill name
    | B ix -> ix

  let bind x qx : (_, _ lnvar) expr binder =
    { body = close_expr 0 x qx }

  let isect_fam q x qx =
    Isect_fam (q, bind x qx)

  let union_fam q x qx =
    Union_fam (q, bind x qx)

  let isect_fam_rel q mode pol r =
    let name = N.fresh () in
    isect_fam q name @@ rel mode pol r (Var (F name))

  let union_fam_rel q mode pol r : (_, _ lnvar) expr =
    let name = N.fresh () in
    union_fam q name @@ rel mode pol r (Var (F name))
end
