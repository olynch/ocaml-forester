open Forester_core

type 'a _object = {
  self: Trie.path option;
  methods: (string * 'a) list
}
[@@deriving show, repr]

type 'a patch = {
  obj: 'a;
  self: Trie.path option;
  methods: (string * 'a) list
}
[@@deriving show, repr]

type node =
  | Text of string
  | Verbatim of string
  | Group of delim * t
  | Math of math_mode * t
  | Ident of Trie.path
  | Hash_ident of string
  | Xml_ident of string option * string
  | Subtree of string option * t
  | Let of Trie.path * Trie.path binding list * t
  | Open of Trie.path
  | Scope of t
  | Put of Trie.path * t
  | Default of Trie.path * t
  | Get of Trie.path
  | Fun of Trie.path binding list * t
  | Object of t _object
  | Patch of t patch
  | Call of t * string
  | Import of visibility * string
  | Def of Trie.path * Trie.path binding list * t
  | Decl_xmlns of string * string
  | Alloc of Trie.path
  | Namespace of Trie.path * t
  | Dx_sequent of t * t list
  | Dx_query of string * t list * t list
  | Dx_prop of t * t list
  | Dx_var of string
  | Dx_const_content of t
  | Dx_const_iri of t
[@@deriving show, repr]

and t = node Range.located list
[@@deriving show, repr]

type tree = {
  source_path: string option;
  addr: string option;
  code: t
}
[@@deriving show, repr]

let import_private x = Import (Private, x)
let import_public x = Import (Public, x)

let inline_math e = Math (Inline, e)
let display_math e = Math (Display, e)
let parens e = Group (Parens, e)
let squares e = Group (Squares, e)
let braces e = Group (Braces, e)
