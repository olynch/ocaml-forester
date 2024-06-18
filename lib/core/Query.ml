open Base

type rel_name = [`Links | `Transclusion | `Authorship | `Contributorship | `Tags | `Taxa ]
[@@deriving show]

type rel_query = [`Incoming | `Outgoing] * rel_name
[@@deriving show]

type 'addr t =
  | Rel of [`Incoming |  `Outgoing] * rel_name * 'addr
  | Tree_under of 'addr
  | Isect of 'addr t list
  | Union of 'addr t list
  | Complement of 'addr t
  | Isect_fam of 'addr t * rel_query
  | Union_fam of 'addr t * rel_query
[@@deriving show]

let rec map f =
  function
  | Rel (pol, rel, x) -> Rel (pol, rel, f x)
  | Tree_under x -> Tree_under (f x)
  | Isect qs -> Isect (List.map (map f) qs)
  | Union qs -> Union (List.map (map f) qs)
  | Isect_fam (q, rq) -> Isect_fam (map f q, rq)
  | Union_fam (q, rq) -> Union_fam (map f q, rq)
  | Complement q -> Complement (map f q)


(** A heuristic for computing an intersection of queries. *)
let rec query_cost =
  function
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


let rec isect qs =
  match sort_by_ascending_cost qs with
  | Isect qs :: qs' -> isect @@ qs @ qs'
  | qs -> Isect qs

and union qs =
  match sort_by_descending_cost qs with
  | Union qs :: qs' -> union @@ qs @ qs'
  | qs -> Union qs

let rec complement =
  function
  | Union qs -> isect @@ List.map complement qs
  | Complement q -> q
  | q -> Complement q

let rel pol rel addr =
  Rel (pol, rel, addr)

let tree_under x = Tree_under x
let isect_fam q pol rel =
  Isect_fam (q, (pol, rel))

let union_fam q pol rel =
  Union_fam (q, (pol, rel))

let has_taxon taxon =
  rel `Incoming `Taxa @@ User_addr taxon
