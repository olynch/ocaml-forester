open Forester_prelude
open Forester_core

module T = Xml_tree
module P = Pure_html
module X = Xml_forester

module type S = sig
  val route : addr -> string option
  val render_article : T.content T.article -> P.node

  val pp_xml : ?stylesheet:string -> Format.formatter -> T.content T.article -> unit
end

module type Params = sig
  val root : string option
end

module Make (Params : Params) (F : Forest.S) () : S = struct

  module PT = Plain_text_client.Make (F)
  module Util = Forest_util.Make (F)
  module Xmlns = Xmlns_effect.Make ()

  module Scope = Algaeff.Reader.Make (struct type t = addr end)

  let transclusion_cache = Hashtbl.create 1000

  let addr_to_string addr =
    Format.asprintf "%a" pp_addr addr

  let addr_type addr =
    match addr with
    | User_addr _ -> "user"
    | Machine_addr _ -> "machine"
    | Anon -> "anon"
    | Hash_addr _ -> "content"

  let route addr =
    if Some addr = (Option.map (fun x -> User_addr x) Params.root) then
      Some "index.xml"
    else
      match addr with
      | User_addr x -> Some (Format.sprintf "%s.xml" x)
      | Machine_addr i -> Some (Format.sprintf "unstable-%i.xml" i)
      | _ -> None

  let render_xml_qname qname =
    let qname = Xmlns.normalise_qname qname in
    match qname.prefix with
    | "" -> qname.uname
    | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

  let render_xml_attr T.{key; value} =
    P.string_attr (render_xml_qname key) "%s" value

  let render_prim_node p =
    X.prim p []

  let render_img =
    function
    | T.Inline {format; base64} ->
      X.img [X.src "data:image/%s;base64,%s" format base64]
    | T.Remote url ->
      X.img [X.src "%s" url]

  let render_xmlns_prefix Xmlns.{prefix; xmlns} =
    P.string_attr ("xmlns:" ^ prefix) "%s" xmlns

  let render_section_flags (dict : T.section_flags) =
    [
      X.optional_ X.show_heading dict.header_shown;
      X.optional_ X.show_metadata dict.metadata_shown;
      X.optional_ X.hidden_when_empty dict.hidden_when_empty;
      X.optional_ X.expanded dict.expanded;
      X.optional_ X.toc dict.included_in_toc;
      X.optional_ X.numbered dict.numbered
    ]

  let rec render_section (section : T.content T.section) : P.node =
    X.tree (render_section_flags section.flags) [
      render_frontmatter section.frontmatter;
      Scope.run ~env:section.frontmatter.addr @@ fun () ->
      X.mainmatter [] @@ render_content section.mainmatter
    ]

  and render_frontmatter (frontmatter : T.content T.frontmatter) : P.node =
    X.frontmatter [] [
      render_attributions frontmatter.addr frontmatter.attributions;
      render_dates frontmatter.dates;
      X.optional (X.source_path [] "%s") frontmatter.source_path;
      X.anchor [] "%i" @@ Oo.id (object end);
      X.addr [X.type_ "%s" @@ addr_type frontmatter.addr] "%s" @@ addr_to_string frontmatter.addr;
      X.optional (X.route [] "%s") @@ route frontmatter.addr;
      X.title [] begin
        let scope = Scope.read () in
        let title = F.get_expanded_title ~scope:(Some scope) frontmatter in
        let cased = T.apply_modifier_to_content Sentence_case title in
        render_content cased
      end;
      X.optional (fun s -> X.taxon [] "%s" @@ String_util.sentence_case s) frontmatter.taxon;
      X.null (List.map render_meta frontmatter.metas)
    ]

  and render_meta (key, body) =
    X.meta [X.name "%s" key] @@
    render_content body

  and render_content (content : T.content) : P.node list =
    match content with
    | T.Text txt0 :: T.Text txt1 :: content ->
      render_content @@ T.Text (txt0 ^ txt1) :: content
    | node :: content ->
      let xs = render_content_node node in
      let ys = render_content content in
      xs @ ys
    | [] -> []

  and render_content_node : T.content_node -> P.node list =
    function
    | Text str ->
      [P.txt "%s" str]
    | CDATA str ->
      [P.txt ~raw:true "<![CDATA[%s]]>" str]
    | Xml_elt elt ->
      let prefixes_to_add, (name, attrs, content) =
        Xmlns.within_scope @@ fun () ->
        render_xml_qname elt.name,
        List.map render_xml_attr elt.attrs,
        render_content elt.content
      in
      let attrs =
        let xmlns_attrs = List.map render_xmlns_prefix prefixes_to_add in
        attrs @ xmlns_attrs
      in
      [P.std_tag name attrs content]
    | Prim (p, content) ->
      [render_prim_node p @@ render_content content]
    | Transclude transclusion ->
      render_transclusion transclusion
    | Link link ->
      render_link link
    | Results_of_query q ->
      let article_to_section =
        T.article_to_section ~flags:{
          T.default_section_flags with
          expanded = Some false;
          numbered = Some false;
          included_in_toc = Some false;
          metadata_shown = Some true
        }
      in
      F.run_query q
      |> Util.get_sorted_articles
      |> List.map (Fun.compose render_section article_to_section)
    | Section section ->
      [render_section section]
    | KaTeX (mode, content) ->
      let display =
        match mode with
        | Inline -> "inline"
        | Display -> "block"
      in
      let body = Format.asprintf "%a" T.TeX_like.pp_content content in
      [X.tex [X.display "%s" display] "<![CDATA[%s]]>" body]
    | TeX_cs cs ->
      (* Should not happen! *)
      [P.txt ~raw:true "\\%s" @@ TeX_cs.show cs]
    | Img img ->
      [render_img img]
    | Resource resource ->
      [render_resource resource]

  and render_resource (resource : T.resource) =
    X.resource [X.hash "%s" resource.hash] [
      X.resource_content [] @@ render_content resource.content;
      render_resource_sources resource.sources
    ]

  and render_resource_sources sources =
    X.null @@ List.map render_resource_source sources

  and render_resource_source source =
    X.resource_source [X.type_ "%s" source.type_; X.resource_part "%s" source.part] "<![CDATA[%s]]>" source.source

  and render_transclusion (transclusion : T.content T.transclusion) : P.node list =
    match Hashtbl.find_opt transclusion_cache transclusion with
    | Some nodes -> nodes
    | None ->
      let content = F.get_content_of_transclusion transclusion in
      let nodes = render_content content in
      Hashtbl.add transclusion_cache transclusion nodes;
      nodes

  and render_link (link : T.content T.link) : P.node list =
    let article_opt = F.get_article @@ User_addr link.href in
    let attrs =
      match article_opt with
      | None ->
        [X.href "%s" link.href;
         X.type_ "external"]
      | Some article ->
        [X.optional_ (X.href "%s") @@ route article.frontmatter.addr;
         X.title_ "%s" @@ PT.string_of_content article.frontmatter.title;
         X.addr_ "%s" @@ addr_to_string article.frontmatter.addr;
         X.type_ "local"]
    in
    [X.link attrs @@ render_content link.content]

  and render_attributions scope attributions =
    let hered_contrs =
      let attribution_name =
        function
        | T.Contributor name -> name
        | T.Author name -> name
      in
      let article_as_contributor (article : _ T.article) =
        match article.frontmatter.addr with
        | User_addr addr when List.for_all (fun z -> not (addr = attribution_name z)) attributions -> Some (T.Contributor addr)
        | _ -> None
      in
      Query.hereditary_contributors (Addr scope)
      |> Query.distill_expr
      |> F.run_query
      |> Util.get_sorted_articles
      |> List.filter_map article_as_contributor
    in
    let all_attributions = List_util.nub (attributions @ hered_contrs) in
    X.authors [] @@ List.map render_attribution all_attributions

  and render_attribution =
    function
    | T.Author x ->
      X.author [] @@ render_bio_link x
    | T.Contributor x ->
      X.contributor [] @@ render_bio_link x

  and render_bio_link ident =
    match F.get_article (User_addr ident) with
    | Some article ->
      render_link {href = ident; content = article.frontmatter.title}
    | None -> [P.txt "%s" ident]


  and render_dates dates =
    X.null @@ List.map render_date dates

  and render_date (date : Date.t) =
    let href_attr =
      let str = Format.asprintf "%a" Date.pp date in
      match F.get_article (User_addr str) with
      | None -> X.null_
      | Some _ -> X.href "%s" str
    in
    X.date [
      href_attr
    ] [
      X.year [] "%i" date.yyyy;
      date.mm |> X.optional @@ X.month [] "%i";
      date.dd |> X.optional @@ X.day [] "%i"
    ]

  let render_article (article : T.content T.article) : P.node =
    let xmlns_prefix = Xmlns.{prefix = X.reserved_prefix; xmlns = X.forester_xmlns} in
    Scope.run ~env:article.frontmatter.addr @@ fun () ->
    Xmlns.run ~reserved:[xmlns_prefix] @@ fun () ->
    X.tree [render_xmlns_prefix xmlns_prefix] [
      render_frontmatter article.frontmatter;
      X.mainmatter [] @@ render_content article.mainmatter;
      X.backmatter [] @@ render_content article.backmatter
    ]

  let pp_xml ?stylesheet fmt article =
    Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
    Format.pp_print_newline fmt ();
    begin
      stylesheet |> Option.iter @@ fun uri ->
      Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" uri
    end;
    Format.pp_print_newline fmt ();
    P.pp_xml fmt @@ render_article article

end
