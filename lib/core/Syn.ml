open Base
open Prelude

type node =
  | Text of string
  | Group of delim * t
  | Math of math_mode * t
  | Link of {dest : t; title : t option}
  | Transclude of string
  | Query of t Query.t
  | Embed_tex of {source : t}
  | Block of t * t
  | Lam of Symbol.t list * t
  | Var of Symbol.t
  | Put of Symbol.t * t * t
  | Default of Symbol.t * t * t
  | Get of Symbol.t
  | If_tex of t * t
  | Xml_tag of string * (string * t) list * t
  | Unresolved of string
  | Prim of Prim.t * t
  | Object of {self : Symbol.t; methods: (string * t) list}
  | Patch of {obj : t; self : Symbol.t; super : Symbol.t; methods : (string * t) list}
  | Call of t * string
[@@deriving show]

and t = node Range.located list
[@@deriving show]

type frontmatter =
  {title : t option;
   addr : addr;
   taxon : string option;
   authors : addr list;
   tags : addr list;
   dates : Date.t list;
   metas : (string * t) list;
   tex_packages : string list;
   source_path : string option}
[@@deriving show]

type tree = frontmatter * t
