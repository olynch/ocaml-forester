module Rel =
struct
  type t = string
  let pp = Format.pp_print_string
  let show x = x

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

open Base

type dbix = int
[@@deriving show]

type name = Symbol.t
[@@deriving show]

type lnvar =
  | F of name
  | B of dbix
[@@deriving show]

type 'var addr_expr =
  | Addr of addr
  | Var of 'var
[@@deriving show]

type 'a binder = {body : 'a}
[@@deriving show]

type 'var expr =
  | Rel of mode * polarity * Rel.t * 'var addr_expr
  | Isect of 'var expr list
  | Union of 'var expr list
  | Complement of 'var expr
  | Union_fam of 'var expr * 'var expr binder
  | Isect_fam of 'var expr * 'var expr binder
[@@deriving show]

let rec open_expr k a =
  function
  | Rel (mode, pol, rel, a') -> Rel (mode, pol, rel, open_addr_expr k a a')
  | Isect qs -> Isect (List.map (open_expr k a) qs)
  | Union qs -> Union (List.map (open_expr k a) qs)
  | Complement q -> Complement (open_expr k a q)
  | Isect_fam (q, scope) ->
    Isect_fam (open_expr k a q, open_scope k a scope)
  | Union_fam (q, scope) ->
    Union_fam (open_expr k a q, open_scope k a scope)

and open_scope k a scope =
  {body = open_expr (k + 1) a scope.body}

and open_addr_expr k a =
  function
  | Addr addr -> Addr addr
  | Var x -> open_lnvar k a x

and open_lnvar k a =
  function
  | F x -> Var (F x)
  | B i when i = k -> a
  | B x -> Var (B x)


let rec close_expr k x =
  function
  | Rel (mode, pol, rel, a) -> Rel (mode, pol, rel, close_addr_expr k x a)
  | Isect qs -> Isect (List.map (close_expr k x) qs)
  | Union qs -> Union (List.map (close_expr k x) qs)
  | Complement q -> Complement (close_expr k x q)
  | Union_fam (q, scope) -> Union_fam (close_expr k x q, close_scope k x scope)
  | Isect_fam (q, scope) -> Isect_fam (close_expr k x q, close_scope k x scope)

and close_scope k x scope =
  {body = close_expr (k + 1) x scope.body}

and close_addr_expr k x =
  function
  | Addr addr -> Addr addr
  | Var var -> Var (close_addr_var k x var)

and close_addr_var k x =
  function
  | F name when x = name -> B k
  | F name -> F name
  | B i when i < k -> B i
  | B i -> B (i + 1)

let bind x qx : lnvar expr binder =
  {body = close_expr 0 x qx}

let unbind scope =
  let name = Symbol.fresh [] in
  let var = Var (F name) in
  name, open_expr 0 var scope.body


let isect_fam q x qx =
  Isect_fam (q, bind x qx)

let union_fam q x qx =
  Union_fam (q, bind x qx)

let rel mode pol rel a =
  Rel (mode, pol, rel, a)

let isect_fam_rel q mode pol r =
  let name = Symbol.fresh [] in
  isect_fam q name @@ rel mode pol r (Var (F name))

let union_fam_rel q mode pol r : lnvar expr =
  let name = Symbol.fresh [] in
  union_fam q name @@ rel mode pol r (Var (F name))

exception Distill of name

let rec distill_expr : lnvar expr -> dbix expr =
  function
  | Rel (mode, pol, rel, a) -> Rel (mode, pol, rel, distill_addr_expr a)
  | Isect qs -> Isect (List.map distill_expr qs)
  | Union qs -> Union (List.map distill_expr qs)
  | Complement q -> Complement (distill_expr q)
  | Union_fam (q, scope) -> Union_fam (distill_expr q, distill_scope scope)
  | Isect_fam (q, scope)-> Isect_fam (distill_expr q, distill_scope scope)

and distill_scope scope =
  {body = distill_expr scope.body}

and distill_addr_expr =
  function
  | Addr addr -> Addr addr
  | Var var -> Var (distill_lnvar var)

and distill_lnvar =
  function
  | F name -> raise @@ Distill name
  | B ix -> ix


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
  qs |> List.sort @@ fun q0 q1 ->
  compare (query_cost q0) (query_cost q1)

let sort_by_descending_cost qs =
  qs |> List.sort @@ fun q0 q1 ->
  compare (query_cost q1) (query_cost q0)


let rec isect qs =
  match sort_by_ascending_cost qs with
  | Isect qs :: qs' -> isect @@ qs @ qs'
  | qs -> Isect qs

let rec union qs =
  match sort_by_descending_cost qs with
  | Union qs :: qs' -> union @@ qs @ qs'
  | qs -> Union qs

let rec complement =
  function
  | Union qs -> isect @@ List.map complement qs
  | Complement q -> q
  | q -> Complement q


let tree_under x =
  rel Paths Outgoing Rel.transclusion x


let has_taxon taxon =
  rel Edges Incoming Rel.taxa (Addr (User_addr taxon))

let hereditary_contributors addr =
  let q_non_ref_under =
    isect [
      tree_under addr;
      complement @@ has_taxon "reference"
    ]
  in
  let q_all_contributors =
    union_fam_rel
      q_non_ref_under
      Edges
      Outgoing
      Rel.contributors
  in
  let q_authors = rel Edges Outgoing Rel.authors addr in
  isect [q_all_contributors; complement q_authors]


let references addr =
  isect [
    union_fam_rel (tree_under addr) Edges Outgoing Rel.links;
    has_taxon "reference"
  ]

let context addr =
  rel Edges Incoming Rel.transclusion addr

let backlinks addr =
  rel Edges Incoming Rel.links addr

let related addr =
  isect [
    rel Edges Outgoing Rel.links addr;
    complement @@ has_taxon "reference"
  ]

let contributions addr =
  union [
    rel Edges Incoming Rel.authors addr;
    rel Edges Incoming Rel.contributors addr
  ]
