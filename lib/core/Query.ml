open Base

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

type polarity =
  | Incoming
  | Outgoing
[@@deriving show]

type rel_query = polarity * Rel.t
[@@deriving show]

type ('addr, 'r) view =
  | Rel of polarity * Rel.t * 'addr
  | Tree_under of 'addr
  | Isect of 'r list
  | Union of 'r list
  | Complement of 'r
  | Isect_fam of 'r * rel_query
  | Union_fam of 'r * rel_query
[@@deriving show]

type 'addr t = Q of ('addr, 'addr t) view
[@@deriving show]

let view (Q q) = q
let make q = Q q

let rec map_view f =
  function
  | Rel (pol, rel, x) -> Rel (pol, rel, f x)
  | Tree_under x -> Tree_under (f x)
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
  | Tree_under _ -> 100
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

let rel pol rel addr =
  make @@ Rel (pol, rel, addr)

let tree_under x =
  make @@ Tree_under x

let isect_fam q pol rel =
  make @@ Isect_fam (q, (pol, rel))

let union_fam q pol rel =
  make @@ Union_fam (q, (pol, rel))

let has_taxon taxon =
  rel Incoming Rel.taxa @@ User_addr taxon

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
      Outgoing
      Rel.contributorship
  in
  let q_authors = rel Outgoing Rel.authorship addr in
  isect [q_all_contributors; complement q_authors]


let references addr =
  isect [
    union_fam (tree_under addr) Outgoing Rel.links;
    has_taxon "reference"
  ]

let context addr =
  rel Incoming Rel.transclusion addr

let backlinks addr =
  rel Incoming Rel.links addr

let related addr =
  isect [
    rel Outgoing Rel.links addr;
    complement @@ has_taxon "reference"
  ]

let contributions addr =
  union [
    rel Incoming Rel.authorship addr;
    rel Incoming Rel.contributorship addr
  ]
