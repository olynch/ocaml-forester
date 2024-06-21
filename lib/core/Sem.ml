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
  | Query of transclusion_opts * addr Query.t
  | Link of addr * t option * modifier
  | Xml_tag of xml_resolved_qname * (xml_resolved_qname * t) list * t
  | TeX_cs of TeX_cs.t
  | Math of math_mode * t
  | Embed_tex of embedded_tex
  | Img of string
  | Prim of Prim.t * t
  | Object of Symbol.t
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

and env = t Env.t
[@@deriving show]

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
  | Backmatter_section of {title: t; query : addr Query.t}

type obj_method =
  {body : Syn.t;
   self : Symbol.t;
   super : Symbol.t;
   env : env}

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
    | Transclude _ | Subtree _ | Query _ | TeX_cs _ | Img _ | Object _ | Link _ | Ref _ -> None
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
