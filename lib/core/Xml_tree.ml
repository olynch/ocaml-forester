open Forester_prelude
open Forester_xml_names
open Base

type xml_qname = Forester_xml_names.xml_qname = { prefix: string; uname: string; xmlns: string option }

type 'content vertex =
  | Iri_vertex of iri
  | Content_vertex of 'content
[@@deriving show, repr]

type section_flags = {
  hidden_when_empty: bool option;
  included_in_toc: bool option;
  header_shown: bool option;
  metadata_shown: bool option;
  numbered: bool option;
  expanded: bool option
}
[@@deriving show, repr]

let default_section_flags =
  {
    hidden_when_empty = None;
    included_in_toc = None;
    header_shown = None;
    metadata_shown = Some false;
    numbered = None;
    expanded = None
  }

type 'content frontmatter_overrides = {
  title: 'content option;
  taxon: 'content option option
}
[@@deriving show, repr]

let default_frontmatter_overrides =
  {
    title = None;
    taxon = None
  }

type xml_attr = { key: xml_qname; value: string }
[@@deriving show, repr]

type 'content xml_elt = {
  name: xml_qname;
  attrs: xml_attr list;
  content: 'content
}
[@@deriving show, repr]

type attribution_role =
  | Author
  | Contributor
[@@deriving show, repr]

type 'content attribution = {
  role: attribution_role;
  vertex: 'content vertex
}
[@@deriving show, repr]

type 'content frontmatter = {
  iri: iri option;
  title: 'content;
  dates: Date.t list;
  attributions: 'content attribution list;
  taxon: 'content option;
  number: string option;
  designated_parent: iri option;
  source_path: string option;
  tags: 'content vertex list;
  metas: (string * 'content) list;
  sets: string list
}
[@@deriving show, repr]

type 'content section = {
  frontmatter: 'content frontmatter;
  mainmatter: 'content;
  flags: section_flags
}
[@@deriving show, repr]

type 'content article = {
  frontmatter: 'content frontmatter;
  mainmatter: 'content;
  backmatter: 'content;
}
[@@deriving show, repr]

type 'content content_target =
  | Full of section_flags * 'content frontmatter_overrides
  | Mainmatter
  | Title
  | Taxon
[@@deriving show, repr]

type modifier =
  Sentence_case | Identity
[@@deriving show, repr]

type 'content transclusion = {
  href: iri;
  target: 'content content_target;
  modifier: modifier
}
[@@deriving show, repr]

type 'content link = {
  href: iri;
  content: 'content
}
[@@deriving show, repr]

type inline_img = {
  format: string;
  base64: string
}
[@@deriving show, repr]

type img =
  | Inline of inline_img
  | Remote of string
[@@deriving show, repr]

type resource_source = {
  type_: string;
  part: string;
  source: string
}
[@@deriving show, repr]

type 'content resource = {
  hash: string;
  content: 'content;
  sources: resource_source list
}
[@@deriving show, repr]

type 'content content_node =
  | Text of string
  | CDATA of string
  | Xml_elt of 'content xml_elt
  | Transclude of 'content transclusion
  | Contextual_number of iri
  | Results_of_query of ('content vertex, Query.dbix) Query.expr
  | Section of 'content section
  | Prim of Prim.t * 'content
  | KaTeX of math_mode * 'content
  | TeX_cs of TeX_cs.t
  | Link of 'content link
  | Img of img
  | Resource of 'content resource
[@@deriving show, repr]

type content =
  Content of content content_node list
[@@deriving show, repr]

let map_content f = function Content nodes -> Content (f nodes)
let extract_content = function Content nodes -> nodes

type query = (content vertex, Query.dbix) Query.expr

let is_whitespace node =
  match node with
  | Text txt -> String.trim txt = ""
  | _ -> false

let strip_whitespace =
  map_content @@
  List.filter @@
  Fun.compose not is_whitespace

let trim_whitespace xs =
  let rec trim_front = function
    | x :: xs when is_whitespace x -> trim_front xs
    | xs -> xs
  and trim_back xs = List.rev @@ trim_front @@ List.rev xs
  in
  trim_back @@ trim_front xs

let default_frontmatter ?iri ?source_path ?designated_parent ?(dates = []) ?(attributions = []) ?taxon ?number ?(metas = []) ?(tags = []) ?(title = Content []) ?(sets = []) () =
  { iri; source_path; designated_parent; dates; attributions; taxon; number; metas; tags; title; sets }

let apply_overrides (overrides : _ frontmatter_overrides) frontmatter =
  {
    frontmatter with
    title = Option.value ~default: frontmatter.title overrides.title;
    taxon = Option.value ~default: frontmatter.taxon overrides.taxon
  }

let article_to_section
    ?(flags = default_section_flags)
    ?(overrides = default_frontmatter_overrides)
    ({ frontmatter; mainmatter; _ }: 'a article)
  =
  {
    frontmatter = apply_overrides overrides frontmatter;
    mainmatter;
    flags
  }

module Comparators (I: sig val string_of_content : content -> string end) = struct
  let compare_content =
    Compare.under I.string_of_content String.compare

  let compare_frontmatter =
    let latest_date (fm : content frontmatter) =
      let sorted_dates = fm.dates |> List.sort @@ Compare.invert Date.compare in
      List.nth_opt sorted_dates 0
    in
    let by_date = Fun.flip @@ Compare.under latest_date @@ Compare.option Date.compare in
    let by_title = compare_content |> Compare.under @@ fun fm -> fm.title in
    Compare.cascade by_date by_title

  let compare_article = compare_frontmatter |> Compare.under @@ fun x -> x.frontmatter
end

let compose_modifier mod0 mod1 =
  match mod0, mod1 with
  | Identity, mod1 -> mod1
  | mod0, Identity -> mod0
  | Sentence_case, Sentence_case -> Sentence_case

let apply_modifier_to_string = function
  | Sentence_case -> String_util.sentence_case
  | Identity -> Fun.id

let rec apply_modifier_to_content_nodes modifier = function
  | [] -> []
  | Text txt1 :: Text txt2 :: content ->
    apply_modifier_to_content_nodes modifier @@ Text (txt1 ^ txt2) :: content
  | node :: content ->
    apply_modifier_to_content_node modifier node :: content

and apply_modifier_to_content modifier =
  map_content (apply_modifier_to_content_nodes modifier)

and apply_modifier_to_content_node modifier = function
  | Text str -> Text (apply_modifier_to_string modifier str)
  | Transclude transclusion ->
    Transclude { transclusion with modifier = compose_modifier modifier transclusion.modifier }
  | Link link -> Link { link with content = apply_modifier_to_content modifier link.content }
  | Prim (p, content) -> Prim (p, apply_modifier_to_content modifier content)
  | node -> node

module TeX_like: sig
    val pp_content : Format.formatter -> content -> unit
    val string_of_content : content -> string
  end
= struct
  let pp_tex_cs fmt = function
    | TeX_cs.Symbol x -> Format.fprintf fmt "\\%c" x
    | TeX_cs.Word x -> Format.fprintf fmt "\\%s " x

  let rec pp_content fmt = function
    | (Content nodes) ->
      (List.iter @@ pp_content_node fmt) nodes

  and pp_content_node fmt = function
    | Text str -> Format.fprintf fmt "%s" str
    | CDATA str -> Format.fprintf fmt "%s" str
    | KaTeX (_, xs) -> pp_content fmt xs
    | TeX_cs cs -> pp_tex_cs fmt cs
    | Xml_elt _ | Transclude _ | Contextual_number _ | Results_of_query _ | Section _ | Prim _ | Link _ | Img _ | Resource _ ->
      Reporter.fatalf Type_error "Cannot render this kind of content as TeX-like string"

  let string_of_content =
    Format.asprintf "%a" pp_content
end
