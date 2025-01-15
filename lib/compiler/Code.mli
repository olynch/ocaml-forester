type node =
    Text of string
  | Verbatim of string
  | Group of Forester_core.delim * t
  | Math of Forester_core.math_mode * t
  | Ident of Forester_core.Trie.path
  | Hash_ident of string
  | Xml_ident of string option * string
  | Subtree of string option * t
  | Let of Forester_core.Trie.path *
      Forester_core.Trie.path Forester_core.binding list * t
  | Open of Forester_core.Trie.path
  | Scope of t
  | Put of Forester_core.Trie.path * t
  | Default of Forester_core.Trie.path * t
  | Get of Forester_core.Trie.path
  | Fun of Forester_core.Trie.path Forester_core.binding list * t
  | Object of t _object
  | Patch of t patch
  | Call of t * string
  | Import of Forester_core.visibility * string
  | Def of Forester_core.Trie.path *
      Forester_core.Trie.path Forester_core.binding list * t
  | Decl_xmlns of string * string
  | Alloc of Forester_core.Trie.path
  | Namespace of Forester_core.Trie.path * t
  | Dx_sequent of t * t list
  | Dx_query of string * t list * t list
  | Dx_prop of t * t list
  | Dx_var of string
  | Dx_const_content of t
  | Dx_const_iri of t
  | Comment of string
  | Error of string

and t = node Forester_core.Range.located list

and 'a _object = {
  self : Forester_core.Trie.path option;
  methods : (string * 'a) list;
}

and 'a patch = {
  obj : 'a;
  self : Forester_core.Trie.path option;
  methods : (string * 'a) list;
}

val t : t Repr.t
val pp : Format.formatter -> t -> unit

type tree = { source_path : string option; addr : string option; code : t; }

val parens : t -> node
val squares : t -> node
val braces : t -> node

val import_private : string -> node
val import_public : string -> node
val inline_math : t -> node
val display_math : t -> node

