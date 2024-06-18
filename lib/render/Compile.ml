open Forester_prelude
open Forester_core

module X = Xml_tree
module F = Xml_forester
module E = Render_effect.Perform

module String_map = Map.Make (String)
module Addr_map = Map.Make (Addr)

module Ancestors = Algaeff.Reader.Make (struct type t = addr list end)
module Current_addr = Algaeff.Reader.Make (struct type t = addr end)
module Mainmatter_cache = Algaeff.State.Make (struct type t = X.content Addr_map.t end)

module Xmlns_map =
struct
  type t =
    {prefix_to_xmlns : string String_map.t;
     xmlns_to_prefixes : string list String_map.t}

  let empty =
    {prefix_to_xmlns = String_map.empty;
     xmlns_to_prefixes = String_map.empty}

  let assoc ~prefix ~xmlns env =
    {prefix_to_xmlns = String_map.add prefix xmlns env.prefix_to_xmlns;
     xmlns_to_prefixes = String_map.add_to_list xmlns prefix env.xmlns_to_prefixes}
end

module Xmlns_prefixes = Algaeff.Reader.Make (Xmlns_map)

let get_xmlns_for_prefix prefix =
  let env = Xmlns_prefixes.read ()  in
  String_map.find_opt prefix env.prefix_to_xmlns

let rec normalise_prefix ?loc ~prefix ~xmlns kont =
  match xmlns with
  | Some xmlns ->
    begin
      let open Xmlns_map in
      let env = Xmlns_prefixes.read () in
      let exception Shadowing in
      try
        begin
          match
            String_map.find_opt prefix env.prefix_to_xmlns,
            String_map.find_opt xmlns env.xmlns_to_prefixes
          with
          | None, (None | Some []) ->
            let env = assoc ~prefix ~xmlns env in
            Xmlns_prefixes.run ~env @@ fun () ->
            kont @@ ([(prefix, xmlns)], prefix)
          | Some xmlns', Some prefixes ->
            if xmlns' = xmlns && List.mem prefix prefixes then
              kont ([], prefix)
            else
              raise Shadowing
          | _, Some (prefix' :: _) ->
            kont ([], prefix')
          | Some xmlns', None ->
            raise Shadowing
        end
      with Shadowing ->
        normalise_prefix ?loc ~prefix:(prefix ^ "_") ~xmlns:(Some xmlns) kont
    end
  | _ ->
    kont ([], prefix)


let compile_date (date : Date.t) =
  let date_addr = User_addr (Format.asprintf "%a" Date.pp date) in
  let href = E.get_doc date_addr |> Option.map @@ fun _doc -> E.route date_addr in
  let year = Date.year date in
  let month = Date.month date in
  let day = Date.day date in
  X.Date {href; year; month; day}

let compile_dates = List.map compile_date

let rec compile_located (located : Sem.node Range.located) =
  match located.value with
  | Sem.Text txt ->
    [X.Text txt]
  | Sem.Verbatim txt ->
    [X.CDATA txt]
  | Sem.Prim (p, xs) ->
    compile_prim p xs
  | Sem.Math (mode, xs) ->
    let body =
      let module TP = Render_TeX_like.Printer in
      Str.global_replace (Str.regexp "\n") " " @@
      TP.contents @@ Render_TeX_like.render ~cfg:{tex = false} xs
    in
    let display =
      match mode with
      | Inline -> `Inline
      | Display -> `Block
    in
    [X.TeX {display; body}]

  | Sem.Link (addr, title, modifier) ->
    begin
      match E.get_doc addr with
      | Some tree ->
        compile_internal_link ~title ~modifier ~addr ~dest:tree
      | None ->
        let url = Format.asprintf "%a" pp_addr addr in
        compile_external_link ~title ~modifier ~url
    end

  | Sem.Ref addr ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%a` for reference" pp_addr addr
      | Some tree ->
        let href = E.route addr in
        let addr = Format.asprintf "%a" pp_addr addr in
        let taxon = tree.fm.taxon |> Option.map String_util.sentence_case in
        let number = tree.fm.number in
        [X.Ref {addr; taxon; href; number}]
    end

  | Sem.Img path ->
    [X.Img {src = path}]

  | Sem.If_tex (_, xs) ->
    X.splice @@ compile_nodes xs

  | Sem.Xml_tag (name, attrs, xs) ->
    let rec fold_attrs tag_prefix updates acc attrs  =
      match attrs with
      | [] ->
        let xmlns_attrs =
          updates |> List.map @@ fun (prefix, xmlns) ->
          X.{key =
               X.{prefix = "xmlns";
                  uname = prefix;
                  xmlns = None};
             value = xmlns}
        in
        let name = X.{prefix = tag_prefix; uname = name.uname; xmlns = get_xmlns_for_prefix tag_prefix} in
        let attrs = xmlns_attrs @ List.rev acc in
        let content = compile_nodes xs in
        X.Xml_tag {name; attrs; content}

      | (k, v) :: attrs ->
        normalise_prefix ?loc:located.loc ~prefix:k.prefix ~xmlns:k.xmlns @@ fun (updates', prefix) ->
        let xml_attr =
          X.{key = X.{prefix; uname = k.uname; xmlns = None};
             value = Render_text.Printer.contents @@ Render_text.render v}
        in
        fold_attrs tag_prefix (updates @ updates') (xml_attr :: acc) attrs
    in

    [normalise_prefix ~prefix:name.prefix ~xmlns:name.xmlns @@ fun (updates, tag_prefix) ->
     fold_attrs tag_prefix updates [] attrs]

  | Sem.TeX_cs name ->
    Reporter.fatalf ?loc:located.loc Resolution_error
      "unresolved TeX control sequence `\\%a`" TeX_cs.pp name

  | Sem.Object _ ->
    Reporter.fatal ?loc:located.loc Type_error
      "tried to compile object closure to XML"

  | Sem.Embed_tex {preamble; source} ->
    let as_tex x =
      Render_TeX_like.Printer.contents @@
      Render_TeX_like.render ~cfg:{tex = true} x
    in
    let preamble = as_tex preamble in
    let source = as_tex source in
    let hash = Digest.to_hex @@ Digest.string @@ preamble ^ source in
    E.enqueue_latex ~name:hash ~preamble ~source;
    [X.Embedded_tex {hash; preamble; source}]

  | Sem.Transclude (opts, addr) ->
    begin
      match E.get_doc addr with
      | None ->
        Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%a` for transclusion" pp_addr addr
      | Some doc ->
        compile_transclusion ~opts doc
    end

  | Sem.Subtree (opts, subtree) ->
    compile_transclusion ~opts subtree

  | Sem.Query (opts, query) ->
    let trees = E.run_query query in
    begin
      match trees with
      | [] ->
        [X.Prim (`P, X.Content [X.Info "Transclusion cycle"])]
      | _ ->
        trees |> List.concat_map @@ fun tree ->
        let opts = Sem.{expanded = false; show_heading = true; title_override = None; taxon_override = None; toc = false; numbered = false; show_metadata = true} in
        compile_transclusion ~opts tree
    end

and compile_transclusion ~opts (tree : Sem.tree) =
  let current = Current_addr.read () in
  let update old_ancestors = current :: old_ancestors in
  Ancestors.scope update @@ fun () ->
  [X.Subtree (compile_tree ~opts tree)]

and compile_title ~(opts : Sem.transclusion_opts) (fm : Sem.frontmatter) =
  let ancestors = Ancestors.read () in
  let title =
    match opts.title_override with
    | Some title -> Some title
    | None ->
      fm.title |> Option.map @@
      Render_util.expand_title_with_parents ~ancestors fm
  in
  title |> Option.map @@ fun title ->
  compile_nodes @@ Sem.sentence_case title

and compile_attributions ~contributors ~authors =
  match authors, contributors with
  | [], [] -> []
  | _ ->
    List.map compile_author authors @ List.map compile_contributor contributors

and compile_author author =
  X.Author (compile_attribution_inner author)

and compile_contributor author =
  X.Contributor (compile_attribution_inner author)

and compile_attribution_inner author =
  let exception Untitled in
  try
    match E.get_doc author with
    | None -> raise Untitled
    | Some biotree ->
      let href = E.route biotree.fm.addr in
      let content =
        match biotree.fm.title with
        | None -> raise Untitled
        | Some title -> compile_nodes title
      in
      let title = biotree.fm.title |> Option.map Sem.string_of_nodes in
      let addr = Option.some @@ Format.asprintf "%a" pp_addr author in
      X.Content [X.Link {type_ = `Local; href; title; addr; content}]
  with Untitled ->
    let name = Format.asprintf "%a" pp_addr author in
    X.Content [X.Text name]


and compile_meta (key, body) =
  let body = compile_nodes body in
  X.Meta {key; body}

and compile_frontmatter ~opts (fm : Sem.frontmatter)  =
  let anchor = string_of_int @@ Oo.id (object end) in
  let title = compile_title ~opts fm in
  let number = fm.number in
  let taxon =
    Option.map String_util.sentence_case @@
    match opts.taxon_override with
    | Some taxon -> Some taxon
    | None -> fm.taxon
  in
  let route = E.route fm.addr in
  let source_path = fm.source_path in
  let addr = Format.asprintf "%a" pp_addr fm.addr in
  let designated_parent =
    fm.designated_parent |> Option.map @@ fun addr ->
    Format.asprintf "%a" pp_addr addr
  in
  let dates = compile_dates fm.dates in
  let attributions = compile_attributions ~contributors:(E.contributors fm.addr) ~authors:fm.authors in
  let last_changed = E.last_changed fm.addr |> Option.map compile_date in
  let metas = fm.metas |> List.map compile_meta in
  X.{title;
     anchor;
     number;
     taxon;
     designated_parent;
     metas;
     route;
     addr;
     source_path;
     dates;
     last_changed;
     attributions}

and compile_tree ?(backmatter = false) ~opts (tree : Sem.tree) =
  Current_addr.run ~env:tree.fm.addr @@ fun  () ->
  let ancestors = Ancestors.read () in
  let options =
    X.{toc = opts.toc;
       numbered = opts.numbered;
       show_heading = opts.show_heading;
       show_metadata = opts.show_metadata;
       expanded = opts.expanded;
       root = E.is_root tree.fm.addr}
  in
  let frontmatter = compile_frontmatter ~opts tree.fm in
  let mainmatter =
    begin
      match tree.fm.addr with
      | addr when List.mem addr ancestors ->
        X.Content [X.Prim (`P, X.Content [X.Info "Transclusion cycle"])]
      | addr ->
        let cache = Mainmatter_cache.get () in
        match Addr_map.find_opt addr cache with
        | Some cached -> cached
        | None ->
          let result = compile_nodes tree.body in
          Mainmatter_cache.modify @@ Addr_map.add addr result;
          result
    end;

  in
  let backmatter =
    if backmatter then
      Some (compile_backmatter tree.fm.addr)
    else
      None
  in
  X.Tree {options; frontmatter; mainmatter; backmatter}

and compile_backmatter addr =
  let opts = {Sem.default_transclusion_opts with numbered = false} in
  let compile_trees =
    List.map @@ fun tree ->
    X.splice_tree @@ compile_tree ~opts tree
  in
  let contributions = compile_trees @@ E.contributions addr in
  let context = compile_trees @@ E.parents addr in
  let related = compile_trees @@ E.related addr in
  let backlinks = compile_trees @@ E.backlinks addr in
  let references = compile_trees @@ E.bibliography addr in
  [X.Contributions contributions;
   X.Context context;
   X.Related related;
   X.Backlinks backlinks;
   X.References references]

and compile_internal_link ~title ~modifier ~addr ~dest =
  let href = E.route addr in
  let ancestors = Ancestors.read () in
  let dest_title =
    dest.fm.title |> Option.map @@
    Render_util.expand_title_with_parents ~ancestors dest.fm
  in
  let content =
    title
    |> Option.fold ~none:dest_title ~some:Option.some
    |> Option.map (Sem.apply_modifier modifier)
    |> Option.value ~default:[Range.locate_opt None @@ Sem.Text "Untitled"]
    |> compile_nodes
  in
  let title =
    match dest_title with
    | None -> None
    | Some t ->
      let title_string =
        String_util.sentence_case @@
        Render_text.Printer.contents @@
        Render_text.render t
      in
      Some title_string
  in
  let addr = Some (Format.asprintf "%a" pp_addr addr) in
  [X.Link {type_ = `Local; href; title; content; addr}]

and compile_external_link ~title ~modifier ~url =
  let href = url in
  let content =
    title
    |> Option.map (Sem.apply_modifier modifier)
    |> Option.value ~default:[Range.locate_opt None @@ Sem.Text url]
    |> compile_nodes
  in
  [X.Link {type_ = `External; href; content; title = None; addr = None}];

and compile_nodes (xs : Sem.t) = X.Content (List.concat_map compile_located xs)

and compile_prim p xs =
  let content = compile_nodes xs in
  [X.Prim (p, content)]

let compile_tree_top tree =
  Ancestors.run ~env:[] @@ fun () ->
  let env = Xmlns_map.assoc ~prefix:F.reserved_prefix ~xmlns:F.forester_xmlns Xmlns_map.empty in
  Xmlns_prefixes.run ~env @@ fun () ->
  compile_tree ~backmatter:true ~opts:Sem.default_transclusion_opts tree


let run kont =
  Mainmatter_cache.run ~init:Addr_map.empty kont
