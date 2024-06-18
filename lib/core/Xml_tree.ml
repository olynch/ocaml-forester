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

type xml_attr = {key : xml_qname; value : string}

type 'content attribution =
  | Author of 'content
  | Contributor of 'content

type date =
  | Date of {
      addr : addr option;
      year : int;
      month : int option;
      day : int option
    }

type 'content meta =
  | Meta of {
      key : string;
      body : 'content
    }

type ('content, 'tree) content_node =
  | Text of string
  | CDATA of string
  | Xml_tag of {
      name : xml_qname;
      attrs : xml_attr list;
      content : 'content
    }
  | Prim of Prim.t * 'content
  | Subtree of 'tree
  | Ref of {
      addr : addr;
      taxon : string option;
      number : string option
    }
  | Local_link of {addr : addr; content : 'content; title : string option}
  | External_link of {href : string; content : 'content; title : string option}
  | TeX of {
      display : [`Inline | `Block];
      body : string
    }
  | Img of {src : string}
  | Embedded_tex of {
      hash : string;
      preamble : string;
      source : string
    }
  | Info of string


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

type tree_options = {
  toc : bool;
  numbered : bool;
  show_heading : bool;
  show_metadata : bool;
  expanded : bool;
  root : bool
}

type 'content tree = {
  options : tree_options;
  frontmatter : 'content frontmatter;
  mainmatter : 'content;
  backmatter : 'content tree list
}

(* Tie the knot *)
type tree_ = Tree of content tree
and content = Content of (content, tree_) content_node list

let splice (Content xs) = xs
let splice_tree (Tree tree) = tree
