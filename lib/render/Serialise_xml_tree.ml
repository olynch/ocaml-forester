open Forester_prelude
open Forester_core

module X = Xml_tree
module F = Xml_forester
module P = Pure_html

module type I = sig
  val root : string option
end

let addr_to_string addr =
  Format.asprintf "%a" pp_addr addr

let addr_type addr =
  match addr with
  | User_addr _ -> "user"
  | Machine_addr _ -> "machine"

let route ~root addr =
  let is_root = Some addr = Option.map (fun x -> User_addr x) root in
  let ext = "xml" in
  let base =
    match is_root with
    | true -> "index"
    | false ->
      match addr with
      | User_addr addr -> addr
      | Machine_addr ix -> Format.sprintf "unstable-%i" ix
  in
  Format.asprintf "%s.%s" base ext

module Make (I : I) () =
struct
  let mainmatter_cache : (addr, P.node) Hashtbl.t =
    Hashtbl.create 1000

  let route = route ~root:I.root

  let render_xml_qname =
    function
    | X.{prefix = ""; uname; _} -> uname
    | X.{prefix; uname; _} -> Format.sprintf "%s:%s" prefix uname

  let render_xml_attr X.{key; value} =
    P.string_attr (render_xml_qname key) "%s" value

  let render_date (X.Date date) =
    F.date [
      date.addr |> F.optional_ @@ fun addr -> F.href "%s" @@ route addr
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
        match tree.frontmatter.addr with
        | None -> render_mainmatter tree.mainmatter
        | Some key ->
          match Hashtbl.find_opt mainmatter_cache key with
          | Some cached -> cached
          | None ->
            let result = render_mainmatter tree.mainmatter in
            Hashtbl.add mainmatter_cache key result;
            result
      end;
      render_backmatter @@
      List.map (fun x -> X.Tree x) tree.backmatter
    ]

  and render_frontmatter (fm : _ X.frontmatter) =
    F.frontmatter [] [
      fm.anchor |> F.optional @@ F.anchor [] "%s";
      begin
        match fm.addr with
        | None -> F.null []
        | Some addr ->
          F.null [
            F.addr [F.type_ "%s" (addr_type addr)] "%s" (addr_to_string addr);
            F.route [] "%s" (route addr)
          ]
      end;
      fm.title |> Option.map render_content |> F.optional @@ F.title [F.optional_ (F.text_ "%s") fm.title_text];
      fm.taxon |> F.optional @@ F.taxon [] "%s";
      fm.source_path |> F.optional @@ F.source_path [] "%s";
      fm.dates |> List.map render_date |> F.null;
      F.authors [] @@ List.map render_attribution_elt fm.attributions;
      fm.number |> F.optional @@ F.number [] "%s";
      fm.metas |> List.map render_meta |> F.null
    ]

  and render_mainmatter mm =
    F.mainmatter [] @@ render_content mm

  and render_backmatter (bm : X.tree_ list)  =
    F.backmatter [] @@ List.map render_tree bm

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
        F.addr_ "%s" (addr_to_string ref.addr);
        F.href "%s" @@ route ref.addr;
        ref.taxon |> F.optional_ @@ F.taxon_ "%s";
        ref.number |> F.optional_ @@ F.number_ "%s"
      ]
    | X.Local_link link ->
      F.link [
        F.type_ "local";
        F.href "%s" @@ route link.addr;
        F.addr_ "%s" @@ addr_to_string link.addr;
        link.title |> F.optional_ @@ F.title_ "%s"
      ] @@ render_content link.content

    | X.External_link link ->
      F.link [
        F.type_ "external";
        F.href "%s" link.href;
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
      let src_attr =
        match img with
        | Inline {format; base64} ->
          F.src "data:image/%s;base64,%s" format base64
        | Remote src ->
          F.src "%s" src
      in
      F.img [src_attr]
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
end
