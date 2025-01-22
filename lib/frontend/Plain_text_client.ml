(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T = Types

let rec pp_content forest fmt = function
  | T.Content c -> c |> List.iter @@ pp_content_node forest fmt

and pp_content_node
    forest
    fmt
    : 'a T.content_node -> unit
  = function
  | Text txt | CDATA txt -> Format.pp_print_string fmt txt
  | Iri iri -> pp_iri fmt iri
  | Route_of_iri iri -> Format.fprintf fmt "%s" (Iri.to_uri iri)
  | KaTeX (_, content) -> pp_content forest fmt content
  | TeX_cs cs -> Format.fprintf fmt "\\%a" TeX_cs.pp cs
  | Xml_elt elt -> pp_content forest fmt elt.content
  | Transclude trn -> pp_transclusion forest fmt trn
  | Contextual_number addr -> Format.fprintf fmt "[%a]" pp_iri addr
  | Section section -> pp_section forest fmt section
  | Prim (_, content) -> pp_content forest fmt content
  | Link link -> pp_link forest fmt link
  | Results_of_query _ | Results_of_datalog_query _ | Img _ | Artefact _ | Datalog_script _ -> ()

and pp_transclusion forest fmt (transclusion : T.content T.transclusion) =
  match Forest.get_content_of_transclusion transclusion forest with
  | None -> Format.fprintf fmt "<could not resolve transclusion of %a>" pp_iri transclusion.href
  | Some content -> pp_content forest fmt content

and pp_link forest fmt (link : T.content T.link) =
  pp_content forest fmt link.content

and pp_section forest fmt (section : T.content T.section) =
  match section.frontmatter.title with
  | None -> Format.fprintf fmt "<omitted content>"
  | Some title -> Format.fprintf fmt "<omitted content: %a>" (pp_content forest) title

let string_of_content forest =
  Format.asprintf "%a" (pp_content forest)
