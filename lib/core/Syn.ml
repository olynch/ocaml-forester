open Base
open Forester_prelude

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Link of {dest : t; title : t option}
  | Subtree of string option * tree
  | Lam of Symbol.t list * t
  | Var of Symbol.t
  | Sym of Symbol.t
  | Put of t * t * t
  | Default of t * t * t
  | Get of t
  | Xml_tag of xml_resolved_qname * (xml_resolved_qname * t) list * t
  | TeX_cs of TeX_cs.t
  | Prim of Prim.t
  | Object of {self : Symbol.t; methods: (string * t) list}
  | Patch of {obj : t; self : Symbol.t; super : Symbol.t; methods : (string * t) list}
  | Call of t * string

  | Query_polarity of Query.polarity
  | Query_mode of Query.mode

  | Query_tree
  | Query_rel
  | Query_isect
  | Query_union
  | Query_compl
  | Query_isect_fam
  | Query_union_fam
  | Query_builtin of [`Taxon | `Author | `Tag]

  | Transclude
  | Embed_tex
  | Ref

  | Title
  | Parent
  | Taxon
  | Meta
  | Author
  | Contributor
  | Tag
  | Date
  | Number

[@@deriving show]

and t = node Range.located list
[@@deriving show]

and tree = t

