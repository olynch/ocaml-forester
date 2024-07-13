module Rel =
struct
  type t = Symbol.t
  let pp = Symbol.pp
  let show = Symbol.show

  let symbol x = x

  let make_builtin name =
    Symbol.fresh ["forester"; "graph"; name]

  let links = make_builtin "links"
  let transclusion = make_builtin "transclusion"
  let authorship = make_builtin "authorship"
  let contributorship = make_builtin "contributorship"
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

type rel_query = mode * polarity * Rel.t
[@@deriving show]

let rel_query_t =
  let open Repr in
  triple mode_t polarity_t Symbol.repr

type ('addr, 'r) view =
  | Rel of rel_query * 'addr
  | Isect of 'r list
  | Union of 'r list
  | Complement of 'r
  | Isect_fam of 'r * rel_query
  | Union_fam of 'r * rel_query
[@@deriving show, repr]

type 'addr t = Q of ('addr, 'addr t) view
[@@deriving show]

let query_t addr_t =
  let open Repr in
  mu 
    (fun query_t -> variant "query"
      (fun q -> function | Q x -> q x)
      |~ case1 "Q" (view_t addr_t query_t) (fun x -> Q x)
      |> sealv)

let view (Q q) = q
let make q = Q q

let rec map_view f =
  function
  | Rel (rq, x) -> Rel (rq, f x)
  | Isect qs -> Isect (List.map (map f) qs)
  | Union qs -> Union (List.map (map f) qs)
  | Isect_fam (q, rq) -> Isect_fam (map f q, rq)
  | Union_fam (q, rq) -> Union_fam (map f q, rq)
  | Complement q -> Complement (map f q)

and map f (Q q) =
  Q (map_view f q)


(** A heuristic for computing an intersection of queries. *)
let rec query_cost q =
  match view q with
  | Rel _ -> 200
  | Isect qs ->
    List.fold_left (fun i q -> min (query_cost q) i) 1000 qs
  | Union qs ->
    List.fold_left (fun i q -> max (query_cost q) i) 0 qs
  | Isect_fam (q, k) -> query_cost q
  | Union_fam (q, k) -> query_cost q
  | Complement _ -> 900

let sort_by_ascending_cost qs =
  qs |> List.sort @@ fun q0 q1 ->
  compare (query_cost q0) (query_cost q1)

let sort_by_descending_cost qs =
  qs |> List.sort @@ fun q0 q1 ->
  compare (query_cost q1) (query_cost q0)


let rec isect qs : 'a t =
  match sort_by_ascending_cost qs with
  | Q (Isect qs) :: qs' -> isect @@ qs @ qs'
  | qs -> make @@ Isect qs

let rec union qs =
  match sort_by_descending_cost qs with
  | Q (Union qs) :: qs' -> union @@ qs @ qs'
  | qs -> make @@ Union qs

let rec complement =
  function
  | Q (Union qs) -> isect @@ List.map complement qs
  | Q (Complement q) -> q
  | q -> make @@ Complement q

let rel mode pol rel addr =
  make @@ Rel ((mode, pol, rel), addr)

let tree_under x =
  rel Paths Outgoing Rel.transclusion x

let isect_fam q mode pol rel =
  make @@ Isect_fam (q, (mode, pol, rel))

let union_fam q mode pol rel =
  make @@ Union_fam (q, (mode, pol, rel))



open Base

let has_taxon taxon =
  rel Edges Incoming Rel.taxa @@ User_addr taxon

let hereditary_contributors addr =
  let q_non_ref_under =
    isect [
      tree_under addr;
      complement @@ has_taxon "reference"
    ]
  in
  let q_all_contributors =
    union_fam
      q_non_ref_under
      Edges
      Outgoing
      Rel.contributorship
  in
  let q_authors = rel Edges Outgoing Rel.authorship addr in
  isect [q_all_contributors; complement q_authors]


let references addr =
  isect [
    union_fam (tree_under addr) Edges Outgoing Rel.links;
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
    rel Edges Incoming Rel.authorship addr;
    rel Edges Incoming Rel.contributorship addr
  ]
