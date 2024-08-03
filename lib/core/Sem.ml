open Base
open Forester_prelude

module MethodTable = Map.Make (String)

module Text_modifier =
struct
  type t = Sentence_case | Identity
  [@@deriving show]
end

type modifier = Text_modifier.t
let pp_modifier = Text_modifier.pp

type node =
  | Text of string
  | Verbatim of string
  | Transclude of transclusion_opts * addr
  | Subtree of transclusion_opts * tree
  | Query_tree of transclusion_opts * query
  | Link of addr * t option * modifier
  | Xml_tag of xml_resolved_qname * (xml_resolved_qname * t) list * t
  | TeX_cs of TeX_cs.t
  | Math of math_mode * t
  | Embed_tex of embedded_tex
  | Img of string
  | Prim of Prim.t * t
  | Ref of addr
[@@deriving show]

and embedded_tex = {preamble : t; source : t}
[@@deriving show]

and transclusion_opts =
  {toc : bool;
   show_heading : bool;
   show_metadata : bool;
   title_override : t option;
   taxon_override : string option;
   expanded : bool;
   numbered : bool}
[@@deriving show]

and t = node Range.located list

and tree =
  {fm : frontmatter;
   bm : backmatter_section list;
   body : t}
[@@deriving show]

and frontmatter =
  {title : t option;
   taxon : string option;
   authors : addr list;
   contributors : addr list;
   dates : Date.t list;
   addr : addr;
   metas : (string * t) list;
   tags: string list;
   physical_parent : addr option;
   designated_parent : addr option;
   source_path : string option;
   number : string option}
[@@deriving show]

and backmatter_section =
  | Backmatter_section of {title: t; query : query}

and value =
  | VContent of t
  | VClo of value Env.t * Symbol.t binding list * Syn.t
  | VQuery_polarity of Query.polarity
  | VQuery_mode of Query.mode
  | VQuery of query
  | VSym of Symbol.t
  | VObject of Symbol.t
[@@deriving show]

and query =
  | Rel of Query.rel_query * query_addr_value
  | Isect of query list
  | Union of query list
  | Complement of query
  | Union_fam of query * Symbol.t * query
  | Isect_fam of query * Symbol.t * query
[@@deriving show]

and query_addr_value =
  | Addr of addr
  | Var of Symbol.t

type obj_method =
  {body : Syn.t;
   self : Symbol.t;
   super : Symbol.t;
   env : value Env.t}

type obj =
  {prototype : Symbol.t option;
   methods : obj_method MethodTable.t}

let is_whitespace node =
  match Range.(node.value) with
  | Text txt -> String.trim txt = ""
  | _ -> false

let strip_whitespace =
  List.filter @@ fun x -> not @@ is_whitespace x

let trim_whitespace xs =
  let rec trim_front xs =
    match xs with
    | x :: xs when is_whitespace x ->
      trim_front xs
    | xs -> xs
  and trim_back xs =
    List.rev @@ trim_front @@ List.rev xs
  in
  trim_back @@ trim_front xs


let sentence_case nodes =
  let map_head f =
    function
    | [] -> []
    | x :: xs -> f x :: xs
  in
  let map_located f node =
    Range.{node with value = f node.value}
  in
  nodes |> map_head @@ map_located @@ function
  | Text str -> Text (String_util.sentence_case str)
  | Link (addr, title, _) -> Link (addr, title, Sentence_case)
  | node -> node


let apply_modifier =
  function
  | Text_modifier.Sentence_case -> sentence_case
  | Text_modifier.Identity -> Fun.id

(** Best-effort rendering of a nodes as a string, to use in text-only contexts.*)
let string_of_nodes =
  let rec render nodes =
    String.concat "" @@
    List.filter_map render_node nodes
  and render_node located =
    match Range.(located.value) with
    | Text s -> Some s
    | Verbatim s -> Some s
    | Link (_, Some title, _) -> Some (render title)
    | Xml_tag (_, _, bdy) | Math (_, bdy) -> Some (render bdy)
    | Embed_tex {source; _} -> Some (render source)
    | Prim (_, x) -> Some (render x)
    | Transclude _ | Subtree _ | Query_tree _ | TeX_cs _ | Img _ | Link _ | Ref _ -> None
  in
  render

module Util =
struct
  let peek_title (tree : tree) =
    match tree.fm.title with
    | Some ({value = Text txt; _} :: _) -> Some txt
    | _ -> None

  let peek_addr (tree : tree) =
    tree.fm.addr

  let tags (tree : tree) =
    tree.fm.tags

  let taxon (tree : tree) =
    tree.fm.taxon

  let authors (tree : tree) =
    tree.fm.authors

  let basic_comparator =
    let latest_date tree =
      let sorted_dates = tree.fm.dates |> List.sort (Compare.invert Date.compare) in
      List.nth_opt sorted_dates 0
    in
    let by_date =
      Fun.flip @@
      Compare.under latest_date @@
      Compare.option Date.compare
    in
    let by_title = Compare.under peek_title @@ Compare.option String.compare in
    Compare.cascade by_date by_title

  let sort =
    List.sort basic_comparator

  let sort_for_index =
    let by_has_parent = Compare.under (fun x -> Option.is_some x.fm.designated_parent) @@ Bool.compare in
    List.sort @@ Compare.cascade by_has_parent basic_comparator

end

module Query =
struct

  (** A heuristic for computing an intersection of queries. *)
  let rec query_cost q =
    match q with
    | Rel _ -> 1
    | Isect qs ->
      List.fold_left (fun i q -> min (query_cost q) i) 1000 qs
    | Union qs ->
      List.fold_left (fun i q -> max (query_cost q) i) 0 qs
    | Union_fam (q, _, q') -> query_cost q * query_cost q'
    | Isect_fam (q, _, q') -> query_cost q * query_cost q'
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

  let rel mode pol rel addr =
    Rel ((mode, pol, rel), Addr addr)

  let tree_under x =
    rel Paths Outgoing Query.Rel.transclusion x

  let isect_fam_rel q mode pol rel =
    let x = Symbol.fresh [] in
    Isect_fam (q, x, Rel ((mode, pol, rel), Var x))

  let union_fam_rel q mode pol rel : query =
    let x = Symbol.fresh [] in
    Union_fam (q, x, Rel ((mode, pol, rel), Var x))

  let has_taxon taxon =
    rel Edges Incoming Query.Rel.taxa (User_addr taxon)

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
        Query.Edges
        Query.Outgoing
        Query.Rel.contributors
    in
    let q_authors = rel Edges Outgoing Query.Rel.authors addr in
    isect [q_all_contributors; complement q_authors]


  let references addr =
    isect [
      union_fam_rel (tree_under addr) Query.Edges Query.Outgoing Query.Rel.links;
      has_taxon "reference"
    ]

  let context addr =
    rel Edges Incoming Query.Rel.transclusion addr

  let backlinks addr =
    rel Edges Incoming Query.Rel.links addr

  let related addr =
    isect [
      rel Edges Outgoing Query.Rel.links addr;
      complement @@ has_taxon "reference"
    ]

  let contributions addr =
    union [
      rel Edges Incoming Query.Rel.authors addr;
      rel Edges Incoming Query.Rel.contributors addr
    ]

end

let empty_frontmatter ~addr =
  {addr;
   title = None;
   taxon = None;
   dates = [];
   authors = [];
   contributors = [];
   tags = [];
   metas = [];
   physical_parent = None;
   designated_parent = None;
   source_path = None;
   number = None}

let default_backmatter ~addr =
  let make_section title query =
    let title = [Range.locate_opt None @@ Text title] in
    Backmatter_section {title; query}
  in
  [make_section "references" @@ Query.references addr;
   make_section "context" @@ Query.context addr;
   make_section "backlinks" @@ Query.backlinks addr;
   make_section "related" @@ Query.related addr;
   make_section "contributions" @@ Query.contributions addr]

let empty_tree ~addr =
  {fm = empty_frontmatter ~addr;
   body = [];
   bm = default_backmatter ~addr}

let default_transclusion_opts : transclusion_opts =
  {title_override = None;
   taxon_override = None;
   show_heading = true;
   show_metadata = true;
   toc = true;
   expanded = true;
   numbered = true}

let extract_content (x : value Range.located) =
  match x.value with
  | VContent content -> content
  | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected content"

let extract_string (x : value Range.located) =
  x |> extract_content |> string_of_nodes

let extract_obj_ptr (x : value Range.located) =
  match x.value with
  | VObject sym -> sym
  | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected object"

let extract_sym (x : value Range.located) =
  match x.value with
  | VSym x -> x
  | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected symbol"

let extract_query_node (x : value Range.located) =
  match x.value with
  | VQuery q -> q
  | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query node"

let extract_query_polarity (x : value Range.located) =
  match x.value with
  | VQuery_polarity pol -> pol
  | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query polarity"

let extract_query_mode (x : value Range.located) =
  match x.value with
  | VQuery_mode mode -> mode
  | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query mode"

let extract_addr (x : value Range.located) =
  match x.value with
  | VContent content -> User_addr (string_of_nodes content)
  | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected tree address"

