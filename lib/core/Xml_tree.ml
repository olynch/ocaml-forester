open Forester_prelude

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
      href : string option;
      year : int;
      month : int option;
      day : int option
    }

type 'content meta =
  | Meta of {
      key : string;
      body : 'content
    }

type link_type = [`Local | `External]

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
      addr : string;
      href : string;
      taxon : string option;
      number : string option
    }
  | Link of {
      type_ : link_type;
      href : string;
      title : string option;
      addr : string option;
      content : 'content
    }
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
  anchor : string;
  number : string option;
  taxon : string option;
  designated_parent : string option;
  metas : 'content meta list;
  route : string;
  addr : string;
  source_path : string option;
  dates : date list;
  last_changed : date option;
  attributions : 'content attribution list
}

(* TODO: generalise *)
type 'tree backmatter_elt =
  | Contributions of 'tree list
  | Related of 'tree list
  | Backlinks of 'tree list
  | Context of 'tree list
  | References of 'tree list

type 'tree backmatter = 'tree backmatter_elt list

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
  backmatter : 'content tree backmatter option
}

(* Tie the knot *)
type tree_ = Tree of content tree
and content = Content of (content, tree_) content_node list

let splice (Content xs) = xs
let splice_tree (Tree tree) = tree