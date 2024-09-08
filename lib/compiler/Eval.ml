open Forester_prelude
open Forester_core

module T = Xml_tree
module QLN = Query.Locally_nameless(Symbol)

module Env = struct
  include Map.Make(Symbol)
  let pp (pp_el : Format.formatter -> 'a -> unit) (fmt : Format.formatter) (map : 'a t) =
    Format.fprintf fmt "@[<v1>{";
    begin
      let@ k, v = Seq.iter @~ to_seq map in
      Format.fprintf fmt "@[%a ~> %a@]@;" Symbol.pp k pp_el v
    end;
    Format.fprintf fmt "}@]"
end

module V = struct
  type t =
    | Content of T.content
    | Clo of t Env.t * Symbol.t binding list * Syn.t
    | Query_polarity of Query.polarity
    | Query_mode of Query.mode
    | Query_expr of (T.content T.vertex, Symbol.t Query.lnvar) Query.expr
    | Sym of Symbol.t
    | Obj of Symbol.t
    | Bool of bool
  [@@deriving show]

  type obj_method = {
    body: Syn.t;
    self: Symbol.t;
    super: Symbol.t;
    env: t Env.t
  }

  module Method_table = Map.Make(String)

  type obj = {
    prototype: Symbol.t option;
    methods: obj_method Method_table.t
  }

  type located = t Range.located

  let extract_content (node : located) =
    match node.value with
    | Content content -> content
    | _ -> Reporter.fatal ?loc: node.loc Type_error "Expected content"

  let coalesce_text =
    let rec loop acc = function
      | [] -> Option.some @@ String.concat "" @@ Bwd.prepend acc []
      | T.Text txt :: content -> loop (Bwd.snoc acc txt) content
      | T.CDATA txt :: content -> loop (Bwd.snoc acc txt) content
      | _ -> None
    in
    loop Emp

  let extract_text (node : located) =
    let content = extract_content node in
    match coalesce_text (T.extract_content content) with
    | Some txt -> String.trim txt
    | None -> Reporter.fatalf ?loc: node.loc Type_error "Expected text but got: %a" pp node.value

  let extract_query_polarity (x : located) =
    match x.value with
    | Query_polarity pol -> pol
    | _ -> Reporter.fatalf ?loc: x.loc Type_error "Expected query polarity"

  let extract_query_mode (x : located) =
    match x.value with
    | Query_mode mode -> mode
    | _ -> Reporter.fatalf ?loc: x.loc Type_error "Expected query mode"

  let extract_query_expr (x : located) =
    match x.value with
    | Query_expr q -> q
    | _ -> Reporter.fatalf ?loc: x.loc Type_error "Expected query expression"

  let extract_obj_ptr (x : located) =
    match x.value with
    | Obj sym -> sym
    | _ -> Reporter.fatalf ?loc: x.loc Type_error "Expected object"

  let extract_sym (x : located) =
    match x.value with
    | Sym sym -> sym
    | _ -> Reporter.fatalf ?loc: x.loc Type_error "Expected symbol"

  let extract_bool (x : located) =
    match x.value with
    | Bool bool -> bool
    (* TODO: deprecate the following two cases *)
    | Content (T.Content [Text "true"]) -> true
    | Content (T.Content [Text "false"]) -> false
    | _ -> Reporter.fatalf ?loc: x.loc Type_error "Expected boolean"
end

let default_backmatter ~(iri : iri) : T.content =
  let a = Query.Vertex (T.Iri_vertex iri) in
  let module QLN = Query.Locally_nameless(Query.Global_name) in
  let make_section title query =
    let query = QLN.distill query in
    let section =
      let frontmatter = T.default_frontmatter ~title: (T.Content [T.Text title]) () in
      let mainmatter = T.Content [T.Results_of_query query] in
      let flags = { T.default_section_flags with hidden_when_empty = Some true } in
      T.{ frontmatter; mainmatter; flags }
    in
    T.Section section
  in
  T.Content
    [
      make_section "references" @@ Builtin_queries.references a;
      make_section "context" @@ Builtin_queries.context a;
      make_section "backlinks" @@ Builtin_queries.backlinks a;
      make_section "related" @@ Builtin_queries.related a;
      make_section "contributions" @@ Builtin_queries.contributions a
    ]

type job = LaTeX_to_svg of { hash: string; source: string; content: svg: string -> T.content }
type result = { main: T.content T.article; side: T.content T.article list; jobs: job list }

module Tape = Tape_effect.Make ()
module Lex_env = Algaeff.Reader.Make(struct type t = V.t Env.t end)
module Dyn_env = Algaeff.Reader.Make(struct type t = V.t Env.t end)
module Host_env = Algaeff.Reader.Make(struct type t = string option end)
module Heap = Algaeff.State.Make(struct type t = V.obj Env.t end)
module Emitted_trees = Algaeff.State.Make(struct type t = T.content T.article list end)
module Jobs = Algaeff.State.Make(struct type t = job list end)
module Frontmatter = Algaeff.State.Make(struct type t = T.content T.frontmatter end)

let get_frontmatter_overrides ~loc =
  let dynenv = Dyn_env.read () in
  let title =
    let@ value = Option.map @~ Env.find_opt Expand.Builtins.Transclude.title_sym dynenv in
    V.extract_content @@ Range.locate_opt loc value
  in
  let taxon =
    let@ value = Option.map @~ Env.find_opt Expand.Builtins.Transclude.taxon_sym dynenv in
    match V.extract_text @@ Range.locate_opt loc value with
    | "" -> None
    | txt -> Some (T.Content_vertex (T.Content [T.Text txt]))
  in
  T.{ title; taxon }

let get_transclusion_flags ~loc =
  let dynenv = Dyn_env.read () in
  let get_bool key =
    let@ value = Option.map @~ Env.find_opt key dynenv in
    V.extract_bool @@ Range.locate_opt loc value
  in
  let module S = Expand.Builtins.Transclude in
  let open Option_util in
  let flags = T.default_section_flags in
  {
    flags with
    expanded = override flags.expanded @@ get_bool S.expanded_sym;
    header_shown = override flags.header_shown @@ get_bool S.show_heading_sym;
    included_in_toc = override flags.included_in_toc @@ get_bool S.toc_sym;
    numbered = override flags.numbered @@ get_bool S.numbered_sym;
    metadata_shown = override flags.metadata_shown @@ get_bool S.show_metadata_sym;
  }

let resolve_iri ~loc str =
  let base = let host = Host_env.read () in Iri_scheme.base_iri ~host in
  match Iri.of_string str with
  | iri -> Ok (Iri.resolve ~base iri)
  | exception (Iri.Error err) -> Error err

let extract_iri (node : V.located) =
  let text = V.extract_text node in
  resolve_iri ~loc: node.loc text

let extract_vertex ~type_ (node : V.located) =
  match type_ with
  | `Content ->
    Ok (T.Content_vertex (V.extract_content node))
  | `Iri ->
    let@ iri = Result.map @~ extract_iri node in
    T.Iri_vertex iri

let extract_query_vertex_expr ~host ~type_ (node : V.located) =
  match node.value with
  | Sym sym -> Query.Var (Query.F sym)
  | _ ->
    match extract_vertex ~type_ node with
    | Ok vtx -> Query.Vertex vtx
    | Error _ -> Reporter.fatalf ?loc: node.loc Type_error "Expected valid RFC 3987 IRI in query expression"

let rec process_tape () =
  match Tape.pop_node_opt () with
  | None -> V.Content (T.Content [])
  | Some node -> eval_node node

and eval_tape tape =
  Tape.run ~tape process_tape

and eval_pop_arg ~loc =
  Tape.pop_arg ~loc
  |> Range.map eval_tape

and pop_content_arg ~loc =
  eval_pop_arg ~loc
  |> V.extract_content

and pop_text_arg ~loc =
  eval_pop_arg ~loc
  |> V.extract_text

and eval_node node : V.t =
  let loc = node.loc in
  match node.value with
  | Var x ->
    eval_var ~loc x
  | Bool bool ->
    focus ?loc @@ V.Bool bool
  | Text str ->
    emit_content_node ~loc @@ T.Text str
  | Prim p ->
    let content = pop_content_arg ~loc |> T.extract_content |> T.trim_whitespace in
    emit_content_node ~loc @@ Prim (p, T.Content content)
  | Fun (xs, body) ->
    let env = Lex_env.read () in
    focus_clo env xs body
  | Ref ->
    begin
      match eval_pop_arg ~loc |> extract_iri with
      | Ok href when Iri.scheme href = Iri_scheme.scheme ->
        let content =
          T.Content
            [
              T.Transclude { href; target = T.Taxon; modifier = Sentence_case };
              T.Text " ";
              T.Contextual_number href
            ]
        in
        emit_content_node ~loc @@ Link { href; content }
      | Ok iri ->
        Reporter.fatalf ?loc Type_error "Cannot refer to content with non-forester IRI %a" pp_iri iri
      | Error _ ->
        Reporter.fatalf ?loc Type_error "Expected valid RFC 3987 IRI in ref"
    end
  | Link { title; dest } ->
    let host = Host_env.read () in
    let dest = { node with value = dest } |> Range.map eval_tape in
    let href =
      match extract_iri dest with
      | Ok iri -> iri
      | Error _ -> Reporter.fatalf ?loc Type_error "Expected valid RFC 3987 IRI in link"
    in
    let content =
      match title with
      | None -> T.Content [T.Transclude { href; target = T.Title; modifier = Identity }]
      | Some title -> { node with value = eval_tape title } |> V.extract_content
    in
    emit_content_node ~loc @@ Link { href; content }
  | Math (mode, body) ->
    let content =
      { node with value = eval_tape body } |> V.extract_content
    in
    emit_content_node ~loc @@ KaTeX (mode, content)
  | Xml_tag (name, attrs, body) ->
    let rec process : _ list -> T.xml_attr list = function
      | [] -> []
      | (key, v) :: attrs ->
        T.{ key; value = V.extract_text { node with value = eval_tape v } } :: process attrs
    in
    let name = T.{ prefix = name.prefix; uname = name.uname; xmlns = name.xmlns } in
    let content = { node with value = eval_tape body } |> V.extract_content in
    emit_content_node ~loc @@ T.Xml_elt { name; attrs = process attrs; content }
  | Query_polarity pol ->
    focus ?loc @@ V.Query_polarity pol
  | Query_mode mode ->
    focus ?loc @@ V.Query_mode mode
  | Query_set ->
    let set = eval_pop_arg ~loc |> V.extract_text in
    focus ?loc @@ V.Query_expr (Query.pred set)
  | Query_rel type_ ->
    let host = Host_env.read () in
    let mode = eval_pop_arg ~loc |> V.extract_query_mode in
    let pol = eval_pop_arg ~loc |> V.extract_query_polarity in
    let rel = eval_pop_arg ~loc |> V.extract_text in
    let vtx = eval_pop_arg ~loc |> extract_query_vertex_expr ~host ~type_ in
    focus ?loc @@ V.Query_expr (Query.rel mode pol rel vtx)
  | Query_isect ->
    let queries =
      let@ arg = List.map @~ Tape.pop_args () in
      arg |> Range.map eval_tape |> V.extract_query_expr
    in
    focus ?loc @@ V.Query_expr (Query.isect queries)
  | Query_union ->
    let queries =
      let@ arg = List.map @~ Tape.pop_args () in
      arg |> Range.map eval_tape |> V.extract_query_expr
    in
    focus ?loc @@ V.Query_expr (Query.union queries)
  | Query_compl ->
    let q = eval_pop_arg ~loc |> V.extract_query_expr in
    focus ?loc @@ V.Query_expr (Complement q)
  | Query_isect_fam ->
    let q = eval_pop_arg ~loc |> V.extract_query_expr in
    let qfun = Tape.pop_arg ~loc in
    let x = Symbol.fresh () in
    let qx =
      let tape = qfun.value @ [{ node with value = Syn.Sym x }] in
      { node with value = eval_tape tape } |> V.extract_query_expr
    in
    focus ?loc @@ V.Query_expr (QLN.isect_fam q x qx)
  | Query_union_fam ->
    let q = eval_pop_arg ~loc |> V.extract_query_expr in
    let qfun = Tape.pop_arg ~loc in
    let x = Symbol.fresh () in
    let qx =
      let tape = qfun.value @ [{ node with value = Syn.Sym x }] in
      { node with value = eval_tape tape } |> V.extract_query_expr
    in
    focus ?loc @@ V.Query_expr (QLN.union_fam q x qx)
  | Query_isect_fam_rel ->
    let q = eval_pop_arg ~loc |> V.extract_query_expr in
    let mode = eval_pop_arg ~loc |> V.extract_query_mode in
    let pol = eval_pop_arg ~loc |> V.extract_query_polarity in
    let rel = pop_text_arg ~loc in
    focus ?loc @@ V.Query_expr (QLN.isect_fam_rel q mode pol rel)
  | Query_union_fam_rel ->
    let q = eval_pop_arg ~loc |> V.extract_query_expr in
    let mode = eval_pop_arg ~loc |> V.extract_query_mode in
    let pol = eval_pop_arg ~loc |> V.extract_query_polarity in
    let rel = pop_text_arg ~loc in
    focus ?loc @@ V.Query_expr (QLN.union_fam_rel q mode pol rel)
  | Query_builtin (builtin, type_) ->
    let host = Host_env.read () in
    let vtx = eval_pop_arg ~loc |> extract_query_vertex_expr ~host ~type_ in
    let r =
      match builtin with
      | `Taxon -> Query.Rel.taxa
      | `Author -> Query.Rel.authors
      | `Tag -> Query.Rel.tags
    in
    let q = Query.rel Edges Incoming r vtx in
    focus ?loc: node.loc @@ V.Query_expr q
  | TeX_cs cs ->
    emit_content_node ~loc @@ TeX_cs cs
  | Transclude ->
    let flags = get_transclusion_flags ~loc in
    let overrides = get_frontmatter_overrides ~loc in
    let href_arg = eval_pop_arg ~loc in
    let href =
      match extract_iri href_arg with
      | Ok iri when Iri.scheme iri = Iri_scheme.scheme -> iri
      | Ok iri ->
        Reporter.fatalf ?loc Type_error "Cannot transclude content with non-forester IRI %a" pp_iri iri
      | Error _ ->
        Reporter.fatalf ?loc Type_error "Expected valid RFC 3987 IRI in transclusion"
    in
    emit_content_node ~loc @@ T.Transclude { href; target = Full (flags, overrides); modifier = Identity }
  | Subtree (addr_opt, nodes) ->
    let flags = get_transclusion_flags ~loc in
    let overrides = get_frontmatter_overrides ~loc in
    let host = Host_env.read () in
    let iri =
      match addr_opt with
      | Some addr -> Iri_scheme.user_iri ~host addr
      | None -> Iri_scheme.fresh ~host
    in
    let subtree = eval_tree_inner ~iri nodes in
    let frontmatter = Frontmatter.get () in
    let subtree = { subtree with frontmatter = { subtree.frontmatter with iri = Some iri; designated_parent = frontmatter.iri } } in
    Emitted_trees.modify @@ List.cons subtree;
    let transclusion = T.{ href = iri; target = Full (flags, overrides); modifier = Identity } in
    emit_content_node ~loc @@ Transclude transclusion
  | Results_of_query ->
    let query =
      eval_pop_arg ~loc
      |> V.extract_query_expr
      |> QLN.distill
    in
    emit_content_node ~loc @@ Results_of_query query
  | Embed_tex ->
    let preamble = pop_content_arg ~loc |> T.TeX_like.string_of_content in
    let body = pop_content_arg ~loc |> T.TeX_like.string_of_content in
    let source = LaTeX_template.to_string ~preamble ~body in
    let hash = Digest.to_hex @@ Digest.string source in
    let content ~svg =
      let base64 = Base64.encode_string svg in
      let img = T.Inline T.{ format = "svg+xml"; base64 } in
      let content = T.Content [T.Img img] in
      let sources =
        [
          T.{ type_ = "latex"; part = "preamble"; source = preamble };
          T.{ type_ = "latex"; part = "body"; source = body }
        ]
      in
      let resource = T.{ hash; content; sources } in
      T.Content [T.Resource resource]
    in
    let job = LaTeX_to_svg { hash; source; content } in
    Jobs.modify (List.cons job);
    let transclusion =
      let host = Host_env.read () in
      let href = Iri_scheme.hash_iri ~host hash in
      let target = T.Mainmatter in
      T.{ href; target; modifier = Identity }
    in
    emit_content_node ~loc @@ T.Transclude transclusion
  | Object { self; methods } ->
    let table =
      let env = Lex_env.read () in
      let add (name, body) =
        let super = Symbol.fresh () in
        V.Method_table.add name V.{ body; self; super; env }
      in
      List.fold_right add methods V.Method_table.empty
    in
    let sym = Symbol.named ["obj"] in
    Heap.modify @@ Env.add sym V.{ prototype = None; methods = table };
    focus ?loc: node.loc @@ V.Obj sym
  | Patch { obj; self; super; methods } ->
    let obj_ptr = { node with value = obj } |> Range.map eval_tape |> V.extract_obj_ptr in
    let table =
      let env = Lex_env.read () in
      let add (name, body) =
        V.Method_table.add
          name
          V.{ body; self; super; env }
      in
      List.fold_right add methods V.Method_table.empty
    in
    let sym = Symbol.named ["obj"] in
    Heap.modify @@ Env.add sym V.{ prototype = Some obj_ptr; methods = table };
    focus ?loc: node.loc @@ V.Obj sym
  | Group (d, body) ->
    let l, r = delim_to_strings d in
    let content =
      let body = V.extract_content { node with value = eval_tape body } in
      T.Content (T.Text l :: T.extract_content body @ [T.Text r])
    in
    focus ?loc: node.loc @@ V.Content content
  | Call (obj, method_name) ->
    let sym = { node with value = obj } |> Range.map eval_tape |> V.extract_obj_ptr in
    let rec call_method (obj : V.obj) =
      let proto_val = obj.prototype |> Option.map @@ fun ptr -> V.Obj ptr in
      match V.Method_table.find_opt method_name obj.methods with
      | Some mthd ->
        let env =
          let env = Env.add mthd.self (V.Obj sym) mthd.env in
          match proto_val with
          | None -> env
          | Some proto_val ->
            Env.add mthd.super proto_val env
        in
        let@ () = Lex_env.run ~env in
        eval_tape mthd.body
      | None ->
        match obj.prototype with
        | Some proto ->
          call_method @@ Env.find proto @@ Heap.get ()
        | None ->
          Reporter.fatalf
            ?loc: node.loc
            Type_error
            "tried to call unbound method `%s`"
            method_name
    in
    let result = call_method @@ Env.find sym @@ Heap.get () in
    focus ?loc: node.loc result
  | Put (k, v, body) ->
    let k = { node with value = k } |> Range.map eval_tape |> V.extract_sym in
    let body =
      let@ () = Dyn_env.scope (Env.add k (eval_tape v)) in
      eval_tape body
    in
    focus ?loc: node.loc body
  | Default (k, v, body) ->
    let k = { node with value = k } |> Range.map eval_tape |> V.extract_sym in
    let body =
      let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval_tape v) flenv in
      let@ () = Dyn_env.scope upd in
      eval_tape body
    in
    focus ?loc: node.loc body
  | Get k ->
    let k = { node with value = k } |> Range.map eval_tape |> V.extract_sym in
    let env = Dyn_env.read () in
    begin
      match Env.find_opt k env with
      | None ->
        Reporter.fatalf
          ?loc: node.loc
          Resolution_error
          "could not find fluid binding named %a in environment %a"
          Symbol.pp
          k
          (Env.pp V.pp)
          env
      | Some v -> focus ?loc: node.loc v
    end
  | Verbatim str ->
    emit_content_node ~loc @@ CDATA str
  | Add_to_set ->
    let set = eval_pop_arg ~loc |> V.extract_text in
    Frontmatter.modify (fun fm -> { fm with sets = set :: fm.sets });
    process_tape ()
  | Title ->
    let title = pop_content_arg ~loc in
    Frontmatter.modify (fun fm -> { fm with title = title });
    process_tape ()
  | Parent ->
    let host = Host_env.read () in
    let parent_arg = eval_pop_arg ~loc in
    let parent =
      match extract_iri parent_arg with
      | Ok iri -> iri
      | Error _ -> Reporter.fatalf ?loc Type_error "Expected valid RFC 3987 IRI in parent declaration"
    in
    Frontmatter.modify (fun fm -> { fm with designated_parent = Some parent });
    process_tape ()
  | Meta ->
    let k = pop_text_arg ~loc in
    let v = pop_content_arg ~loc in
    Frontmatter.modify (fun fm -> { fm with metas = fm.metas @ [k, v] });
    process_tape ()
  | Attribution (role, type_) ->
    let arg = eval_pop_arg ~loc in
    let vertex =
      match extract_vertex ~type_ arg with
      | Ok vtx -> vtx
      | Error _ ->
        let corrected_attribution_code =
          match role with
          | Author -> "\\author/content"
          | Contributor -> "\\contributor/content"
        in
        Reporter.emitf ?loc Type_warning "Expected valid RFC 3987 IRI in attribution. Use `%s` instead if you intend an unlinked attribution." corrected_attribution_code;
        T.Content_vertex (V.extract_content arg)
    in
    let attribution = T.{ role; vertex } in
    Frontmatter.modify (fun fm -> { fm with attributions = fm.attributions @ [attribution] });
    process_tape ()
  | Tag type_ ->
    let arg = eval_pop_arg ~loc in
    let vertex =
      match extract_vertex ~type_ arg with
      | Ok vtx -> vtx
      | Error _ ->
        let corrected = "\\tag/content" in
        Reporter.emitf ?loc Type_warning "Expected valid RFC 3987 IRI in tag. Use `%s` instead if you intend an unlinked attribution." corrected;
        T.Content_vertex (V.extract_content arg)
    in
    Frontmatter.modify (fun fm -> { fm with tags = fm.tags @ [vertex] });
    process_tape ()
  | Date ->
    let date_str = pop_text_arg ~loc in
    begin
      match Date.parse date_str with
      | None ->
        Reporter.fatalf ?loc: node.loc Parse_error "Invalid date string `%s`" date_str
      | Some date ->
        Frontmatter.modify (fun fm -> { fm with dates = fm.dates @ [date] });
        process_tape ()
    end
  | Number ->
    let num = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> { fm with number = Some num });
    process_tape ()
  | Taxon type_ ->
    let arg = eval_pop_arg ~loc in
    let vertex =
      match extract_vertex ~type_ arg with
      | Ok vtx -> vtx
      | Error _ ->
        let corrected_code = "\\taxon/content" in
        Reporter.emitf ?loc Type_warning "Expected valid RFC 3987 IRI in taxon. Use `%s` instead if you intend an unlinked taxon." corrected_code;
        T.Content_vertex (V.extract_content arg)
    in
    Frontmatter.modify (fun fm -> { fm with taxon = Some vertex });
    process_tape ()
  | Sym sym ->
    focus ?loc: node.loc @@ V.Sym sym

and eval_var ~loc x =
  match Env.find_opt x @@ Lex_env.read () with
  | Some v -> focus ?loc v
  | None ->
    Reporter.fatalf
      ?loc
      Resolution_error
      "could not find variable named %a"
      Symbol.pp
      x

and focus ?loc = function
  | V.Clo (rho, xs, body) ->
    focus_clo ?loc rho xs body
  | V.Content (T.Content content) ->
    begin
      match process_tape () with
      | V.Content (T.Content content') -> V.Content (T.Content (content @ content'))
      | value -> value
    end
  | V.Query_expr _ | V.Query_mode _ | V.Query_polarity _ | V.Sym _ | V.Obj _ | V.Bool _ as v ->
    begin
      match process_tape () with
      | V.Content content when T.strip_whitespace content = T.Content [] -> v
      | _ -> Reporter.fatalf ?loc Type_error "Expected solitary node"
    end

and focus_clo ?loc rho xs body =
  match xs with
  | [] ->
    focus ?loc @@
      let@ () = Lex_env.run ~env: rho in
      eval_tape body
  | (strategy, y) :: ys ->
    match Tape.pop_arg_opt () with
    | Some arg ->
      let yval =
        match strategy with
        | Strict -> eval_tape arg.value
        | Lazy -> V.Clo (Lex_env.read (), [(Strict, Symbol.fresh ())], arg.value)
      in
      let rhoy = Env.add y yval rho in
      focus_clo ?loc rhoy ys body
    | None ->
      begin
        match process_tape () with
        | Content nodes when T.strip_whitespace nodes = T.Content [] -> Clo (rho, xs, body)
        | _ -> Reporter.fatalf ?loc Type_error "Expected %i additional arguments" (List.length xs)
      end

and emit_content_node ~loc content =
  focus ?loc @@ Content (T.Content [content])

and eval_tree_inner ~(iri : iri) (tree : Syn.tree) : T.content T.article =
  let attribution_is_author attr =
    match T.(attr.role) with
    | T.Author -> true
    | _ -> false
  in
  let outer_frontmatter = Frontmatter.get () in
  let attributions = List.filter attribution_is_author outer_frontmatter.attributions in
  let frontmatter =
    T.default_frontmatter
      ~iri
      ~attributions
      ?source_path: outer_frontmatter.source_path
      ~dates: outer_frontmatter.dates
      ()
  in
  let@ () = Frontmatter.run ~init: frontmatter in
  let mainmatter = { value = eval_tape tree; loc = None } |> V.extract_content in
  let frontmatter = Frontmatter.get () in
  let backmatter = default_backmatter ~iri in
  T.{ frontmatter; mainmatter; backmatter }

let eval_tree ~host ~iri ~source_path (tree : Syn.tree) : result =
  let fm = T.default_frontmatter ~iri ?source_path () in
  let@ () = Frontmatter.run ~init: fm in
  let@ () = Emitted_trees.run ~init: [] in
  let@ () = Jobs.run ~init: [] in
  let@ () = Heap.run ~init: Env.empty in
  let@ () = Lex_env.run ~env: Env.empty in
  let@ () = Dyn_env.run ~env: Env.empty in
  let@ () = Host_env.run ~env: host in
  let main = eval_tree_inner ~iri tree in
  let side = Emitted_trees.get () in
  let jobs = Jobs.get () in
  { main; side; jobs }
