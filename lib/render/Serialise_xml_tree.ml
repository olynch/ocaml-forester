open Forester_prelude
open Forester_core

module X = Xml_tree
module F = Xml_forester
module P = Pure_html

module String_map = Map.Make (String)

module Mainmatter_cache = Algaeff.State.Make (struct type t = P.node String_map.t end)

let render_xml_qname =
  function
  | X.{prefix = ""; uname; _} -> uname
  | X.{prefix; uname; _} -> Format.sprintf "%s:%s" prefix uname

let render_xml_attr X.{key; value} =
  P.string_attr (render_xml_qname key) "%s" value

let render_date (X.Date date) =
  F.date [
    date.href |> F.optional_ @@ F.href "%s"
  ] [
    F.year [] "%i" date.year;
    date.month |> F.optional @@ F.month [] "%i";
    date.day |> F.optional @@ F.day [] "%i"
  ]

let rec render_tree (X.Tree tree) =
  F.tree [
    F.toc tree.options.toc;
    F.numbered tree.options.numbered;
    F.show_heading tree.options.show_heading;
    F.show_metadata tree.options.show_metadata;
    F.expanded tree.options.expanded;
    F.root tree.options.root;
    P.string_attr ("xmlns:" ^ F.reserved_prefix) "%s" F.forester_xmlns
  ] [
    render_frontmatter tree.frontmatter;
    begin
      let cache = Mainmatter_cache.get () in
      let key = tree.frontmatter.route in
      match String_map.find_opt key cache with
      | Some cached -> cached
      | None ->
        let result = render_mainmatter tree.mainmatter in
        Mainmatter_cache.modify (String_map.add key result);
        result
    end;
    tree.backmatter |> F.optional render_backmatter
  ]

and render_frontmatter (fm : _ X.frontmatter) =
  F.frontmatter [] [
    F.anchor [] "%s" fm.anchor;
    F.addr [] "%s" fm.addr;
    F.route [] "%s" fm.route;
    fm.title |> Option.map render_content |> F.optional @@ F.title [];
    fm.taxon |> F.optional @@ F.taxon [] "%s";
    fm.source_path |> F.optional @@ F.source_path [] "%s";
    fm.dates |> List.map render_date |> F.null;
    F.authors [] @@ List.map render_attribution_elt fm.attributions;
    fm.number |> F.optional @@ F.number [] "%s";
    fm.designated_parent |> F.optional @@ F.parent [] "%s";
    fm.metas |> List.map render_meta |> F.null
  ]

and render_mainmatter mm =
  F.mainmatter [] @@ render_content mm

and render_backmatter (bm : _ X.backmatter)  =
  F.backmatter [] @@ List.map render_backmatter_elt bm

and render_backmatter_elt =
  let render_trees trees =
    trees |> List.map @@ fun tree ->
    render_tree @@ X.Tree tree
  in
  function
  | Contributions trees ->
    F.contributions [] @@ render_trees trees
  | Context trees ->
    F.context [] @@ render_trees trees
  | Related trees ->
    F.related [] @@ render_trees trees
  | Backlinks trees ->
    F.backlinks [] @@ render_trees trees
  | References trees ->
    F.references [] @@ render_trees trees

and render_meta (Meta meta) =
  F.meta [F.name "%s" meta.key] @@
  render_content meta.body

and render_attribution_elt =
  function
  | X.Author x ->
    F.author [] @@ render_content x
  | X.Contributor x ->
    F.contributor [] @@ render_content x

and render_content (X.Content xs) =
  List.map render_content_node xs


and render_content_node =
  function
  | X.Text x ->
    P.txt "%s" x
  | X.CDATA x ->
    P.txt ~raw:true "<![CDATA[%s]]>" x
  | X.Prim (p, x) ->
    F.prim p [] @@ render_content x
  | X.Xml_tag {name; attrs; content} ->
    P.std_tag
      (render_xml_qname name)
      (List.map render_xml_attr attrs)
      (render_content content)
  | X.Subtree tree ->
    render_tree tree
  | X.Ref ref ->
    F.ref [
      F.addr_ "%s" ref.addr;
      F.href "%s" ref.href;
      ref.taxon |> F.optional_ @@ F.taxon_ "%s";
      ref.number |> F.optional_ @@ F.number_ "%s"
    ]
  | X.Link link ->
    let type_ =
      match link.type_ with
      | `Local -> "local"
      | `External -> "external"
    in
    F.link [
      F.type_ "%s" type_;
      F.href "%s" link.href;
      link.addr |> F.optional_ @@ F.addr_ "%s";
      link.title |> F.optional_ @@ F.title_ "%s"
    ] @@ render_content link.content
  | X.TeX tex ->
    let display =
      match tex.display with
      | `Inline -> "inline"
      | `Block -> "block"
    in
    F.tex [F.display "%s" display] "<![CDATA[%s]]>" tex.body
  | X.Img img ->
    F.img [F.src "%s" img.src]
  | X.Embedded_tex emb ->
    F.embedded_tex [F.hash "%s" emb.hash] [
      F.embedded_tex_preamble [] "<![CDATA[%s]]>" emb.preamble;
      F.embedded_tex_body [] "<![CDATA[%s]]>" emb.source
    ]
  | X.Info x ->
    F.info [] [P.txt "%s" x]


let pp ?stylesheet fmt tree =
  Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
  Format.pp_print_newline fmt ();
  begin
    stylesheet |> Option.iter @@ fun uri ->
    Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" uri
  end;
  Format.pp_print_newline fmt ();
  P.pp_xml fmt @@ render_tree tree

let run kont =
  Mainmatter_cache.run ~init:String_map.empty kont
