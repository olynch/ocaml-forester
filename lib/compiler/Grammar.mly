(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

%{
  open Forester_prelude
  open Forester_core
%}

%token <string option * string> XML_ELT_IDENT
%token <string> DECL_XMLNS
%token <string> TEXT VERBATIM
%token <string> WHITESPACE
%token <string> IDENT

%token <string> HASH_IDENT
%token IMPORT EXPORT DEF NAMESPACE LET FUN OPEN
%token OBJECT PATCH CALL
%token SUBTREE SCOPE PUT GET DEFAULT ALLOC
%token SLASH LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE TICK AT_SIGN HASH
%token EOF

%token DATALOG

%token DX_ENTAILED
%token <string> DX_VAR

%start <Code.t> main

%%

let locate(p) ==
| x = p; { Asai.Range.locate_lex $loc x }

let braces(p) == delimited(LBRACE, p, RBRACE)
let squares(p) == delimited(LSQUARE, p, RSQUARE)
let parens(p) == delimited(LPAREN, p, RPAREN)

let bvar :=
| x = TEXT; { [x] }

let bvar_with_strictness :=
| x = TEXT; {
  match String_util.explode x with
  | '~' :: chars -> Lazy, [String_util.implode chars]
  | _ -> Strict, [x]
 }

let binder == list(squares(bvar_with_strictness))

let ws_or(p) :=
| WHITESPACE; { [] }
| x = p; { [x] }

let ws_list(p) := flatten(list(ws_or(p)))
let separated_ws_list(sep,p) := flatten(separated_list(sep,ws_or(p)))

let textual_node :=
| ~ = TEXT; <Code.Text>
| ~ = WHITESPACE; <Code.Text>
| ~ = head_node1; <Fun.id>

let code_expr == ws_list(locate(head_node1))
let textual_expr == list(locate(textual_node))

let head_node :=
| DEF; (~,~,~) = fun_spec; <Code.Def>
| ALLOC; ~ = ident; <Code.Alloc>
| IMPORT; ~ = txt_arg; <Code.import_private>
| EXPORT; ~ = txt_arg; <Code.import_public>
| NAMESPACE; ~ = ident; ~ = braces(code_expr); <Code.Namespace>
| SUBTREE; ~ = option(squares(wstext)); ~ = braces(ws_list(locate(head_node))); <Code.Subtree>
| FUN; ~ = binder; ~ = arg; <Code.Fun>
| LET; (~,~,~) = fun_spec; <Code.Let>
| ~ = ident; <Code.Ident>
| ~ = HASH_IDENT; <Code.Hash_ident>
| SCOPE; ~ = arg; <Code.Scope>
| PUT; ~ = ident; ~ = arg; <Code.Put>
| DEFAULT; ~ = ident; ~ = arg; <Code.Default>
| GET; ~ = ident; <Code.Get>
| OPEN; ~ = ident; <Code.Open>
| (~,~) = XML_ELT_IDENT; <Code.Xml_ident>
| ~ = DECL_XMLNS; ~ = txt_arg; <Code.Decl_xmlns>
| OBJECT; self = option(squares(bvar)); methods = braces(ws_list(method_decl)); { Code.Object {self;  methods } }
| PATCH; obj = braces(code_expr); self = option(squares(bvar)); methods = braces(ws_list(method_decl)); { Code.Patch {obj; self; methods} }
| CALL; ~ = braces(code_expr); ~ = txt_arg; <Code.Call>
| DATALOG; LBRACE; list(WHITESPACE); ~ = dx_sequent_node; RBRACE; <>
| ~ = VERBATIM; <Code.Verbatim>
| ~ = delimited(HASH_LBRACE, textual_expr, RBRACE); <Code.inline_math>
| ~ = delimited(HASH_HASH_LBRACE, textual_expr, RBRACE); <Code.display_math>
| ~ = braces(textual_expr); <Code.braces>
| ~ = squares(textual_expr); <Code.squares>
| ~ = parens(textual_expr); <Code.parens>

let head_node1 :=
| ~ = head_node; <>
| DX_ENTAILED; { Code.Text "-:" }
| x = DX_VAR; { Code.Text ("?" ^ x) }
| TICK; { Code.Text "'" }
| AT_SIGN; { Code.Text "@" }
| HASH; { Code.Text "#" }

let method_decl :=
| k = squares(TEXT); list(WHITESPACE); v = arg; { k, v }

let ident :=
| ~ = separated_nonempty_list(SLASH, IDENT); <>

let ws_or_text :=
| x = TEXT; { x }
| x = WHITESPACE; { x }

let wstext :=
| xs = list(ws_or_text); { String.concat "" xs }

let arg :=
| braces(textual_expr)
| located_str = locate(VERBATIM);
  { [{located_str with value = Code.Verbatim located_str.value}] }

let txt_arg == braces(wstext)
let fun_spec == ~ = ident; ~ = binder; ~ = arg; <>

let main :=
| ~ = ws_list(locate(head_node)); EOF; <>

let dx_rel :=
| x = locate(ident); { [Range.{x with value = Code.Ident x.value}] }
| TICK; ~ = arg; <>

let dx_term_node :=
| ~ = DX_VAR; <Code.Dx_var>
| TICK; ~ = arg; <Code.Dx_const_content>
| AT_SIGN; ~ = arg; <Code.Dx_const_iri>

let dx_term :=
| x = locate(dx_term_node); { [x] }

let dx_prop_node :=
| ~ = dx_rel; ~ = ws_list(dx_term); <Code.Dx_prop>

let dx_prop :=
| p = locate(dx_prop_node); { [p] }

let dx_sequent_node :=
| ~ = dx_prop; DX_ENTAILED; ~ = ws_list(braces(dx_prop)); <Code.Dx_sequent>
| x = DX_VAR; list(WHITESPACE); DX_ENTAILED; pos = ws_list(braces(dx_prop)); { Code.Dx_query (x, pos, []) }
| ~ = DX_VAR; list(WHITESPACE); DX_ENTAILED; ~ = ws_list(braces(dx_prop)); HASH; ~ = ws_list(braces(dx_prop)); <Code.Dx_query>

