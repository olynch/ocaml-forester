open Forester_prelude
open Base

type xml_qname = {
  prefix : string;
  (** The prefix to a qualified XML name; this prefix is expected to be rendered in the scope of a corresponding [xmlns] binding. *)

  uname : string;
  (** The unqualified part of the XML name. *)

  xmlns : string option
  (** The XML namespace bound by the current scope to [prefix]. This is not used when serialising to XML, but can be helpful for other analyses. *)
}
[@@deriving repr]

type xml_attr = {key : xml_qname; value : string}
[@@deriving repr]

type 'content attribution =
  | Author of 'content
  | Contributor of 'content
[@@deriving repr]

type date_ = {
  addr : addr option;
  year : int;
  month : int option;
  day : int option
}
[@@deriving repr]

type date =
  | Date of date_
[@@deriving repr]

type 'content meta_ = {
  key : string;
  body : 'content
}
[@@deriving repr]

type 'content meta =
  | Meta of 'content meta_
[@@deriving repr]

type 'content xml_tag = {
  name : xml_qname;
  attrs : xml_attr list;
  content : 'content
}
[@@deriving repr]

type ref = {
  addr : addr;
  taxon : string option;
  number : string option
}
[@@deriving repr]

type 'content local_link = {
  addr : addr;
  content : 'content;
  title : string option
}
[@@deriving repr]

type 'content external_link = {
  href : string;
  content : 'content;
  title : string option;
}
[@@deriving repr]

type tex = {
  display : [`Inline | `Block];
  body : string
}
[@@deriving repr]

type img = {
  src : string;
}
[@@deriving repr]

type embedded_tex = {
  hash : string;
  preamble : string;
  source : string
}
[@@deriving repr]

type ('content, 'tree) content_node =
  | Text of string
  | CDATA of string
  | Xml_tag of 'content xml_tag
  | Prim of Prim.t * 'content
  | Subtree of 'tree
  | Ref of ref
  | Local_link of 'content local_link
  | External_link of 'content external_link
  | TeX of tex
  | Img of img
  | Embedded_tex of embedded_tex
  | Info of string
[@@deriving repr]


type 'content frontmatter = {
  title : 'content option;
  anchor : string option;
  number : string option;
  taxon : string option;
  designated_parent : string option;
  metas : 'content meta list;
  addr : addr option;
  source_path : string option;
  dates : date list;
  last_changed : date option;
  attributions : 'content attribution list
}
[@@deriving repr]

type tree_options = {
  toc : bool;
  numbered : bool;
  show_heading : bool;
  show_metadata : bool;
  expanded : bool;
  root : bool
}
[@@deriving repr]

type 'content tree = {
  options : tree_options;
  frontmatter : 'content frontmatter;
  mainmatter : 'content;
  backmatter : 'content tree list
}

let tree_t content_t =
  let open Repr in
  mu @@ fun tree_t ->
  record "tree"
    (fun
      options
      frontmatter
      mainmatter
      backmatter
      -> {
          options;
          frontmatter;
          mainmatter;
          backmatter;
        })
  |+ field "options" tree_options_t (fun t -> t.options)
  |+ field "frontmatter" (frontmatter_t content_t)(fun t -> t.frontmatter)
  |+ field "mainmatter" content_t(fun t -> t.mainmatter)
  |+ field "backmatter" (list (tree_t))(fun t -> t.backmatter)
  |> sealr

(* Tie the knot *)
type tree_ = Tree of content tree
[@@deriving repr]
and content = Content of (content, tree_) content_node list
[@@deriving repr]

let splice (Content xs) = xs
let splice_tree (Tree tree) = tree
