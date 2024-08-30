open Forester_prelude
open Forester_core

module T = Xml_tree
module P = Pure_html
module X = Xml_forester

module type S = sig
  val route : addr -> string option
  val render_article : T.content T.article -> P.node

  val pp_xml : ?stylesheet: string -> Format.formatter -> T.content T.article -> unit
end

module type Params = sig
  val root : string option
end

module Make (Params: Params) (F: Forest.S) () : S = struct

  module PT = Plain_text_client.Make(F)
  module Util = Forest_util.Make(F)

  module Xmlns = struct
    include Xmlns_effect.Make ()

    let run =
      let xmlns_prefix =
        {
          prefix = X.reserved_prefix;
          xmlns = X.forester_xmlns
        }
      in
      run ~reserved: [xmlns_prefix]
  end

  module Scope = Algaeff.Reader.Make(struct type t = addr end)

  let transclusion_cache = Hashtbl.create 1000

  let addr_to_string addr =
    Format.asprintf "%a" Addr.pp addr

  let addr_type : addr -> string = function
    | User_addr _ -> "user"
    | Machine_addr _ -> "machine"
    | Anon -> "anon"
    | Hash_addr _ -> "content"

  let addr_is_root addr =
    Some addr = Option.map Addr.user_addr Params.root

  let route addr =
    if addr_is_root addr then
      Some "index.xml"
    else
      match addr with
      | User_addr x -> Some (Format.sprintf "%s.xml" x)
      | Machine_addr i -> Some (Format.sprintf "unstable-%i.xml" i)
      | _ -> None

  let get_expanded_title frontmatter =
    let scope = Scope.read () in
    let title = F.get_expanded_title ~scope frontmatter in
    T.map_content (T.apply_modifier_to_content Sentence_case) title

  let render_xml_qname qname =
    let qname = Xmlns.normalise_qname qname in
    match qname.prefix with
    | "" -> qname.uname
    | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

  let render_xml_attr T.{ key; value } =
    P.string_attr (render_xml_qname key) "%s" value

  let render_prim_node p =
    X.prim p []

  let render_img = function
    | T.Inline { format; base64 } ->
      X.img [X.src "data:image/%s;base64,%s" format base64]
    | T.Remote url ->
      X.img [X.src "%s" url]

  let render_xmlns_prefix Xmlns.{ prefix; xmlns } =
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
    let@ () = Xmlns.run in
    X.tree
      (render_section_flags section.flags)
      [
        render_frontmatter section.frontmatter;
        let@ () = Scope.run ~env: section.frontmatter.addr in
        X.mainmatter [] @@ render_content section.mainmatter
      ]

  and render_frontmatter (frontmatter : T.content T.frontmatter) : P.node =
    X.frontmatter
      []
      [
        render_attributions frontmatter.addr frontmatter.attributions;
        render_dates frontmatter.dates;
        X.optional (X.source_path [] "%s") frontmatter.source_path;
        X.anchor [] "%i" @@ Oo.id ( object end);
        X.addr [X.type_ "%s" @@ addr_type frontmatter.addr] "%s" @@ addr_to_string frontmatter.addr;
        X.optional (X.route [] "%s") @@ route frontmatter.addr;
        begin
          let title = get_expanded_title frontmatter in
          X.title [X.text_ "%s" @@ PT.string_of_content title] @@
            render_content title
        end;
        X.optional (fun s -> X.taxon [] "%s" @@ String_util.sentence_case s) frontmatter.taxon;
        X.null (List.map render_meta frontmatter.metas)
      ]

  and render_meta (key, body) =
    X.meta [X.name "%s" key] @@
      render_content body

  and render_content (Content content: T.content) : P.node list =
    match content with
    | T.Text txt0 :: T.Text txt1 :: content ->
      render_content @@ Content (T.Text (txt0 ^ txt1) :: content)
    | node :: content ->
      let xs = render_content_node node in
      let ys = render_content (Content content) in
      xs @ ys
    | [] -> []

  and render_content_node : 'a T.content_node -> P.node list = function
    | Text str ->
      [P.txt "%s" str]
    | CDATA str ->
      [P.txt ~raw: true "<![CDATA[%s]]>" str]
    | Xml_elt elt ->
      let prefixes_to_add, (name, attrs, content) =
        let@ () = Xmlns.within_scope in
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
    | Contextual_number addr ->
      let custom_number =
        let@ article = Option.bind @@ F.get_article addr in
        article.frontmatter.number
      in
      begin
        match custom_number with
        | None -> [X.contextual_number [X.addr_ "%s" @@ addr_to_string addr]]
        | Some num -> [P.txt "%s" num]
      end
    | Link link ->
      render_link link
    | Results_of_query q ->
      let article_to_section =
        T.article_to_section
          ~flags: {
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
      [P.txt ~raw: true "\\%s" @@ TeX_cs.show cs]
    | Img img ->
      [render_img img]
    | Resource resource ->
      [render_resource resource]

  and render_resource (resource : T.content T.resource) =
    X.resource
      [X.hash "%s" resource.hash]
      [
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
    let article_opt = F.get_article @@ Addr.user_addr link.href in
    let attrs =
      match article_opt with
      | None ->
        [
          X.href "%s" link.href;
          X.type_ "external"
        ]
      | Some article ->
        [
          X.optional_ (X.href "%s") @@ route article.frontmatter.addr;
          X.title_ "%s" @@ PT.string_of_content @@ get_expanded_title article.frontmatter;
          X.addr_ "%s" @@ addr_to_string article.frontmatter.addr;
          X.type_ "local"
        ]
    in
    [X.link attrs @@ render_content link.content]

  and render_attributions scope attributions =
    let hered_contrs =
      let attribution_name = function
        | T.Contributor name -> name
        | T.Author name -> name
      in
      let article_as_contributor (article : _ T.article) =
        match article.frontmatter.addr with
        | User_addr addr when List.for_all (fun z -> not (addr = attribution_name z)) attributions -> Some (T.Contributor addr)
        | _ -> None
      in
      let module QLN = Query.Locally_nameless(Query.Global_name) in
      QLN.hereditary_contributors (Addr scope)
      |> QLN.distill
      |> F.run_query
      |> Util.get_sorted_articles
      |> List.filter_map article_as_contributor
    in
    let all_attributions = List_util.nub (attributions @ hered_contrs) in
    X.authors [] @@ List.map render_attribution all_attributions

  and render_attribution = function
    | T.Author x ->
      X.author [] @@ render_bio_link x
    | T.Contributor x ->
      X.contributor [] @@ render_bio_link x

  and render_bio_link ident =
    match F.get_article (Addr.user_addr ident) with
    | Some article ->
      render_link { href = ident; content = article.frontmatter.title }
    | None -> [P.txt "%s" ident]

  and render_dates dates =
    X.null @@ List.map render_date dates

  and render_date (date : Date.t) =
    let href_attr =
      let str = Format.asprintf "%a" Date.pp date in
      match F.get_article (Addr.user_addr str) with
      | None -> X.null_
      | Some _ -> X.href "%s" str
    in
    X.date
      [
        href_attr
      ]
      [
        X.year [] "%i" date.yyyy;
        date.mm |> X.optional @@ X.month [] "%i";
        date.dd |> X.optional @@ X.day [] "%i"
      ]

  let render_article (article : T.content T.article) : P.node =
    let xmlns_prefix = Xmlns.{ prefix = X.reserved_prefix; xmlns = X.forester_xmlns } in
    let@ () = Scope.run ~env: article.frontmatter.addr in
    let@ () = Xmlns.run in
    X.tree
      [
        render_xmlns_prefix xmlns_prefix;
        X.root @@ addr_is_root article.frontmatter.addr
      ]
      [
        render_frontmatter article.frontmatter;
        X.mainmatter [] @@ render_content article.mainmatter;
        X.backmatter [] @@ render_content article.backmatter
      ]

  let pp_xml ?stylesheet fmt article =
    Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
    Format.pp_print_newline fmt ();
    begin
      let@ uri = Option.iter @~ stylesheet in
      Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" uri
    end;
    Format.pp_print_newline fmt ();
    P.pp_xml fmt @@ render_article article
end
