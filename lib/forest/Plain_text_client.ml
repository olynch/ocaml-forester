open Forester_core

module T = Xml_tree

module type Params = sig
  val route : iri -> string
end

module Default_params: Params = struct
  let route = Iri.to_uri
end

module type S = sig
  val string_of_content : Xml_tree.content -> string
  val pp_content : Format.formatter -> Xml_tree.content -> unit
end

module Make (F: Forest.S) (P: Params) : S = struct

  let rec pp_content fmt = function
    | T.Content c -> c |> List.iter @@ pp_content_node fmt

  and pp_content_node fmt : 'a T.content_node -> unit = function
    | Text txt | CDATA txt -> Format.pp_print_string fmt txt
    | Iri iri -> pp_iri fmt iri
    | Route_of_iri iri -> Format.fprintf fmt "%s" (P.route iri)
    | KaTeX (_, content) -> pp_content fmt content
    | TeX_cs cs -> Format.fprintf fmt "\\%a" TeX_cs.pp cs
    | Xml_elt elt -> pp_content fmt elt.content
    | Transclude trn -> pp_transclusion fmt trn
    | Contextual_number addr -> Format.fprintf fmt "[%a]" pp_iri addr
    | Section section -> pp_section fmt section
    | Prim (_, content) -> pp_content fmt content
    | Link link -> pp_link fmt link
    | Results_of_query _ | Results_of_datalog_query _ | Img _ | Artefact _ | Datalog_script _ -> ()

  and pp_transclusion fmt (transclusion : T.content T.transclusion) =
    pp_content fmt @@ F.get_content_of_transclusion transclusion

  and pp_link fmt (link : T.content T.link) =
    pp_content fmt link.content

  and pp_section fmt (section : T.content T.section) =
    match section.frontmatter.title with
    | None -> Format.fprintf fmt "<omitted content>"
    | Some title -> Format.fprintf fmt "<omitted content: %a>" pp_content title

  let string_of_content =
    Format.asprintf "%a" pp_content
end
