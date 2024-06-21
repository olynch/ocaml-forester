open Forester_prelude
open Forester_core

module type I =
sig
  val root : string option
  val trees : Sem.tree Addr_map.t
  val run_query : addr Query.t -> Addr_set.t
  val last_changed : addr -> Date.t option
  val enqueue_latex : name:string -> preamble:string -> source:string -> unit
end

module S = Addr_set

module X = Xml_tree
module F = Xml_forester

module Ancestors = Algaeff.Reader.Make (struct type t = addr list end)
module Current_addr = Algaeff.Reader.Make (struct type t = addr end)

module Make (I : I) () =
struct

  let mainmatter_cache : (addr, X.content) Hashtbl.t =
    Hashtbl.create 1000

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

  let is_root addr =
    Some addr = Option.map (fun x -> User_addr x) I.root

  let addr_peek_title scope =
    Option.bind (Addr_map.find_opt scope I.trees) Sem.Util.peek_title


  let get_tree addr =
    Addr_map.find_opt addr I.trees

  let get_sorted_trees addrs : Sem.tree list =
    let find addr =
      match get_tree addr with
      | None -> []
      | Some doc -> [doc]
    in
    Sem.Util.sort @@ List.concat_map find @@ S.elements addrs

  let get_trees_from_query query =
    get_sorted_trees @@ I.run_query query

  let contributors scope =
    let by_title =
      Compare.under addr_peek_title @@
      Compare.option String.compare
    in
    List.sort by_title @@ S.elements @@ I.run_query @@
    Query.hereditary_contributors scope


  let compile_date (date : Date.t) =
    let addr =
      let addr = User_addr (Format.asprintf "%a" Date.pp date) in
      get_tree addr |> Option.map @@ fun _doc -> addr
    in
    let year = Date.year date in
    let month = Date.month date in
    let day = Date.day date in
    X.Date {addr; year; month; day}

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
        match get_tree addr with
        | Some tree ->
          compile_internal_link ~title ~modifier ~addr ~dest:tree
        | None ->
          let url = Format.asprintf "%a" pp_addr addr in
          compile_external_link ~title ~modifier ~url
      end

    | Sem.Ref addr ->
      begin
        match get_tree addr with
        | None ->
          Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%a` for reference" pp_addr addr
        | Some tree ->
          let taxon = tree.fm.taxon |> Option.map String_util.sentence_case in
          let number = tree.fm.number in
          [X.Ref {addr; taxon; number}]
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
               value = Render_text.Printer.contents @@ Render_text.render ~trees:I.trees v}
          in
          fold_attrs tag_prefix (updates @ updates') (xml_attr :: acc) attrs
      in

      [normalise_prefix ~prefix:name.prefix ~xmlns:name.xmlns @@ fun (updates, tag_prefix) ->
       fold_attrs tag_prefix updates [] attrs]

    | Sem.TeX_cs name ->
      Reporter.fatalf ?loc:located.loc Resolution_error
        "unresolved control sequence `\\%a`" TeX_cs.pp name

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
      I.enqueue_latex ~name:hash ~preamble ~source;
      [X.Embedded_tex {hash; preamble; source}]

    | Sem.Transclude (opts, addr) ->
      begin
        match get_tree addr with
        | None ->
          Reporter.fatalf ?loc:located.loc Tree_not_found "could not find tree at address `%a` for transclusion" pp_addr addr
        | Some doc ->
          compile_transclusion ~opts doc
      end

    | Sem.Subtree (opts, subtree) ->
      compile_transclusion ~opts subtree

    | Sem.Query (opts, query) ->
      begin
        match get_trees_from_query query with
        | [] ->
          [X.Prim (`P, X.Content [X.Info "Query returned no results"])]
        | trees ->
          trees |> List.concat_map @@ fun tree ->
          let opts = Sem.{expanded = false; show_heading = true; title_override = None; taxon_override = None; toc = false; numbered = false; show_metadata = true} in
          compile_transclusion ~opts tree
      end

  and compile_transclusion ~opts (tree : Sem.tree) =
    let current = Current_addr.read () in
    let update old_ancestors = current :: old_ancestors in
    Ancestors.scope update @@ fun () ->
    [X.Subtree (compile_tree_inner ~opts tree)]

  and compile_title ~(opts : Sem.transclusion_opts) (fm : Sem.frontmatter) =
    let trees = I.trees in
    let ancestors = Ancestors.read () in
    let title =
      match opts.title_override with
      | Some title -> Some title
      | None ->
        fm.title |> Option.map @@
        Render_util.expand_title_with_parents ~trees ~ancestors fm
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
      match get_tree author with
      | None -> raise Untitled
      | Some biotree ->
        let content =
          match biotree.fm.title with
          | None -> raise Untitled
          | Some title -> compile_nodes title
        in
        let title = biotree.fm.title |> Option.map Sem.string_of_nodes in
        let addr = Option.some @@ Format.asprintf "%a" pp_addr author in
        X.Content [X.Local_link {title; addr = author; content}]
    with Untitled ->
      let name = Format.asprintf "%a" pp_addr author in
      X.Content [X.Text name]


  and compile_meta (key, body) =
    let body = compile_nodes body in
    X.Meta {key; body}

  and compile_frontmatter ~opts (fm : Sem.frontmatter)  =
    let anchor = Option.some @@ string_of_int @@ Oo.id (object end) in
    let title = compile_title ~opts fm in
    let number = fm.number in
    let taxon =
      Option.map String_util.sentence_case @@
      match opts.taxon_override with
      | Some taxon -> Some taxon
      | None -> fm.taxon
    in
    let source_path = fm.source_path in
    let addr = Option.some fm.addr in
    let designated_parent =
      fm.designated_parent |> Option.map @@ fun addr ->
      Format.asprintf "%a" pp_addr addr
    in
    let dates = compile_dates fm.dates in
    let contributors = contributors fm.addr in
    let attributions = compile_attributions ~contributors ~authors:fm.authors in
    let last_changed = I.last_changed fm.addr |> Option.map compile_date in
    let metas = fm.metas |> List.map compile_meta in
    X.{title;
       anchor;
       number;
       taxon;
       designated_parent;
       metas;
       addr;
       source_path;
       dates;
       last_changed;
       attributions}

  and compile_tree_inner ?(include_backmatter = false) ~opts (tree : Sem.tree) =
    Current_addr.run ~env:tree.fm.addr @@ fun  () ->
    let ancestors = Ancestors.read () in
    let options =
      X.{toc = opts.toc;
         numbered = opts.numbered;
         show_heading = opts.show_heading;
         show_metadata = opts.show_metadata;
         expanded = opts.expanded;
         root = is_root tree.fm.addr}
    in
    let frontmatter = compile_frontmatter ~opts tree.fm in
    let mainmatter =
      begin
        match tree.fm.addr with
        | addr when List.mem addr ancestors ->
          X.Content [X.Prim (`P, X.Content [X.Info "Transclusion cycle"])]
        | addr ->
          match Hashtbl.find_opt mainmatter_cache addr with
          | Some cached -> cached
          | None ->
            let result = compile_nodes tree.body in
            Hashtbl.add mainmatter_cache addr result;
            result
      end;
    in
    let backmatter =
      if include_backmatter && not (is_root tree.fm.addr) then
        compile_backmatter tree.fm.addr tree.bm
      else
        []
    in
    X.Tree {options; frontmatter; mainmatter; backmatter}

  and compile_backmatter addr bm =
    let opts = {Sem.default_transclusion_opts with numbered = false} in
    let compile_trees =
      List.map @@ fun tree ->
      X.splice_tree @@ compile_tree_inner ~opts tree
    in
    bm |> List.filter_map @@ function
    | Sem.Backmatter_section {title; query} ->
      let title =
        Option.some @@ compile_nodes @@
        Sem.sentence_case title
      in
      match compile_trees @@ get_trees_from_query query with
      | [] -> None
      | trees ->
        let options =
          X.{toc = false; expanded = true; numbered = false; show_heading = true; show_metadata = false; root = false}
        in
        let frontmatter =
          X.{title; anchor = None; number = None; taxon = None; designated_parent = None; metas = []; addr = None; source_path = None; dates = []; last_changed = None; attributions = []}
        in
        let mainmatter =
          X.Content begin
            trees |> List.map @@ fun tree ->
            let options = X.{tree.options with expanded = false} in
            let tree = X.{tree with options} in
            X.Subtree (X.Tree tree)
          end
        in
        Option.some @@ X.{options; frontmatter; mainmatter; backmatter = []}

  and compile_internal_link ~title ~modifier ~addr ~dest =
    let trees = I.trees in
    let ancestors = Ancestors.read () in
    let dest_title =
      dest.fm.title |> Option.map @@
      Render_util.expand_title_with_parents ~trees ~ancestors dest.fm
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
          Render_text.render ~trees t
        in
        Some title_string
    in
    [X.Local_link {title; content; addr = addr}]

  and compile_external_link ~title ~modifier ~url =
    let href = url in
    let content =
      title
      |> Option.map (Sem.apply_modifier modifier)
      |> Option.value ~default:[Range.locate_opt None @@ Sem.Text url]
      |> compile_nodes
    in
    [X.External_link {href; content; title = None}];

  and compile_nodes (xs : Sem.t) = X.Content (List.concat_map compile_located xs)

  and compile_prim p xs =
    let content = compile_nodes xs in
    [X.Prim (p, content)]

  let compile_tree tree =
    Ancestors.run ~env:[] @@ fun () ->
    let env = Xmlns_map.assoc ~prefix:F.reserved_prefix ~xmlns:F.forester_xmlns Xmlns_map.empty in
    Xmlns_prefixes.run ~env @@ fun () ->
    compile_tree_inner ~include_backmatter:true ~opts:Sem.default_transclusion_opts tree
end
