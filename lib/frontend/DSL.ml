(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* At the moment, this module is mostly for marking up test cases *)

open Forester_core

module T = Types

let txt str = T.Text str

let p content = T.Prim (`P, T.Content content)
let ul content = T.Prim (`Ul, T.Content content)
let ol content = T.Prim (`Ol, T.Content content)
let li content = T.Prim (`Li, T.Content content)
let em content = T.Prim (`Em, T.Content content)
let strong content = T.Prim (`Strong, T.Content content)
let code content = T.Prim (`Code, T.Content content)
let blockquote content = T.Prim (`Blockquote, T.Content content)
let pre content = T.Prim (`Pre, T.Content content)
let figure content = T.Prim (`Figure, T.Content content)
let figcaption content = T.Prim (`Figcaption, T.Content content)
let cdata content = T.CDATA content
let contextual_number href = T.Contextual_number (Iri.of_string href)
let results_of_query query = T.Results_of_query query
let katex m content = T.KaTeX (m, content)
let tex content = T.TeX_cs (Word content)
let img href = T.(Img (Remote href))
let route_of_iri iri = T.Route_of_iri iri

module Datalog = struct
  open Datalog_expr
  let premises ~rel ~args = {rel; args}
  let prop premises conclusion = {premises; conclusion}
  let const v = Const v
end

let datalog_script script = T.Datalog_script script

let section
    ~mainmatter
    ?(frontmatter = T.default_frontmatter ())
    ?(flags = T.default_section_flags)
    ()
  =
  T.Section
    {
      frontmatter;
      mainmatter = T.Content mainmatter;
      flags;
    }

let xml_elt (prefix, uname) content =
  let prefix = Option.value ~default: "" prefix in
  let qname = T.{prefix; uname; xmlns = None} in
  T.Xml_elt
    {
      name = qname;
      attrs = [];
      content = T.Content content
    }

let transclude href =
  T.Transclude
    T.{
      href = Iri.of_string href;
      target = Mainmatter;
      modifier = Identity
    }

let artefact content =
  T.Artefact
    T.{
      hash = "";
      content = Content content;
      sources = []
    }

let link href content =
  T.Link
    {
      href = Iri.of_string href;
      content = T.Content content;
    }

module Code = struct
  open Forester_compiler
  open Code
  open Asai.Range

  let import_private = Fun.compose (locate_opt None) @@ Code.import_private
  let import_public = Fun.compose (locate_opt None) @@ Code.import_public

  let inline_math = Fun.compose (locate_opt None) @@ Code.inline_math
  let display_math = Fun.compose (locate_opt None) @@ Code.display_math
  let parens = Fun.compose (locate_opt None) @@ Code.parens
  let squares = Fun.compose (locate_opt None) @@ Code.squares
  let braces = Fun.compose (locate_opt None) @@ Code.braces

  let text str = locate_opt None @@ Text str
  let verbatim str = locate_opt None @@ Verbatim str
  let math mode nodes = locate_opt None @@ Math (mode, nodes)
  let ident path = locate_opt None @@ Ident path
  let scope nodes = locate_opt None @@ Scope nodes
  let open_ path = locate_opt None @@ Open path
  let group delim nodes = locate_opt None @@ Group (delim, nodes)
  let def p b t = locate_opt None @@ Def (p, b, t)
  let object_ t = locate_opt None @@ Code.Object t
end

module Syn = struct
  open Forester_compiler.Syn
  open Asai.Range
  let fun_ b t = locate_opt None @@ Fun (b, t)
  let prim p = locate_opt None @@ Prim p

  let text s = locate_opt None @@ Text s

  let parens e = locate_opt None @@ Group (Parens, e)
  let squares e = locate_opt None @@ Group (Squares, e)
  let braces e = locate_opt None @@ Group (Braces, e)
end
