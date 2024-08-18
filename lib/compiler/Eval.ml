open Forester_prelude
open Forester_core

module T = Xml_tree
module QLN = Query.Locally_nameless (Symbol)

module Env =
struct
  include Map.Make (Symbol)
  let pp (pp_el : Format.formatter -> 'a -> unit) : Format.formatter -> 'a t -> unit =
    fun fmt map ->
    Format.fprintf fmt "@[<v1>{";
    begin
      map |> iter @@ fun k v ->
      Format.fprintf fmt "@[%a ~> %a@]@;" Symbol.pp k pp_el v
    end;
    Format.fprintf fmt "}@]"
end

module V =
struct
  type t =
    | Content of T.content
    | Clo of t Env.t * Symbol.t binding list * Syn.t
    | Query_polarity of Query.polarity
    | Query_mode of Query.mode
    | Query_expr of Symbol.t Query.lnvar Query.expr
    | Sym of Symbol.t
    | Obj of Symbol.t
    | Bool of bool
  [@@deriving show]

  type obj_method =
    {body : Syn.t;
     self : Symbol.t;
     super : Symbol.t;
     env : t Env.t}

  module MethodTable = Map.Make (String)

  type obj =
    {prototype : Symbol.t option;
     methods : obj_method MethodTable.t}

  type located = t Range.located

  let extract_content (node : located) =
    match node.value with
    | Content content -> content
    | _ -> Reporter.fatal ?loc:node.loc Type_error "Expected content"

  let coalesce_text =
    let rec loop acc =
      function
      | [] -> Option.some @@ String.concat "" @@ List.rev acc
      | T.Text txt :: content -> loop (txt :: acc) content
      | _ -> None
    in
    loop []


  let extract_text (node : located) =
    let content = extract_content node in
    match coalesce_text content with
    | Some txt -> String.trim txt
    | None -> Reporter.fatalf ?loc:node.loc Type_error "Expected address but got: %a" pp node.value

  let extract_query_polarity (x : located) =
    match x.value with
    | Query_polarity pol -> pol
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query polarity"

  let extract_query_mode (x : located) =
    match x.value with
    | Query_mode mode -> mode
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query mode"

  let extract_query_addr_expr (x : located) =
    match x.value with
    | Sym sym -> Query.Var (Query.F sym)
    | Content [Text txt] -> Query.Addr (Addr.user_addr txt)
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected addr expression in query"

  let extract_query_expr (x : located) =
    match x.value with
    | Query_expr q -> q
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query expression"

  let extract_obj_ptr (x : located) =
    match x.value with
    | Obj sym -> sym
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected object"

  let extract_sym (x : located) =
    match x.value with
    | Sym sym -> sym
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected symbol"

  let extract_bool (x : located) =
    match x.value with
    | Bool bool -> bool
    (* TODO: deprecate the following two cases *)
    | Content [Text "true"] -> true
    | Content [Text "false"] -> false
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected boolean"
end


let default_backmatter ~addr : T.content =
  let a = Query.Addr addr in
  let module QLN = Query.Locally_nameless (Query.Global_name) in
  let make_section title query =
    let query = QLN.distill query in
    let section =
      let frontmatter =
        {T.empty_frontmatter with
         title = [T.Text title]}
      in
      let mainmatter = [T.Results_of_query query] in
      let flags = {T.default_section_flags with hidden_when_empty = Some true} in
      T.{frontmatter; mainmatter; flags}
    in
    T.Section section
  in
  [make_section "references" @@ QLN.references a;
   make_section "context" @@ QLN.context a;
   make_section "backlinks" @@ QLN.backlinks a;
   make_section "related" @@ QLN.related a;
   make_section "contributions" @@ QLN.contributions a]




type job = LaTeX_to_svg of {hash : string; source : string; content : svg:string -> T.content}
type result = {main : T.content T.article; side : T.content T.article list; jobs : job list}

module Tape = Tape_effect.Make ()
module Lex_env = Algaeff.Reader.Make (struct type t = V.t Env.t end)
module Dyn_env = Algaeff.Reader.Make (struct type t = V.t Env.t end)
module Heap = Algaeff.State.Make (struct type t = V.obj Env.t end)
module Emitted_trees = Algaeff.State.Make (struct type t = T.content T.article list end)
module Jobs = Algaeff.State.Make (struct type t = job list end)
module Frontmatter = Algaeff.State.Make (struct type t = T.content T.frontmatter end)

let get_frontmatter_overrides ~loc =
  let dynenv = Dyn_env.read () in
  let title =
    Env.find_opt Expand.Builtins.Transclude.title_sym dynenv |> Option.map @@ fun value ->
    V.extract_content @@ Range.locate_opt loc value
  in
  let taxon =
    Env.find_opt Expand.Builtins.Transclude.taxon_sym dynenv |> Option.map @@ fun value ->
    match V.extract_text @@ Range.locate_opt loc value with
    | "" -> None
    | txt -> Some txt
  in
  T.{title; taxon}

let get_transclusion_flags ~loc =
  let dynenv = Dyn_env.read () in
  let get_bool key =
    Env.find_opt key dynenv |> Option.map @@ fun value ->
    V.extract_bool @@ Range.locate_opt loc value
  in
  let module S = Expand.Builtins.Transclude in
  let open Option_util in
  let flags = T.default_section_flags in
  { flags with
    expanded = override flags.expanded @@ get_bool S.expanded_sym;
    header_shown = override flags.header_shown @@ get_bool S.show_heading_sym;
    included_in_toc = override flags.included_in_toc @@ get_bool S.toc_sym;
    numbered = override flags.numbered @@ get_bool S.numbered_sym;
    metadata_shown = override flags.metadata_shown @@ get_bool S.show_metadata_sym;
  }

let rec process_tape () =
  match Tape.pop_node_opt () with
  | None -> V.Content []
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
    let content = pop_content_arg ~loc |> T.trim_whitespace in
    emit_content_node ~loc @@ Prim (p, content)

  | Fun (xs, body) ->
    let env = Lex_env.read () in
    focus_clo env xs body

  | Ref ->
    let href = pop_text_arg ~loc in
    let addr = Addr.user_addr href in
    let content = [
      T.Transclude {addr; target = T.Taxon; modifier = Sentence_case};
      T.Text " ";
      T.Transclude {addr; target = T.Number; modifier = Identity}
    ] in
    emit_content_node ~loc @@ Link {href; content}

  | Link {title; dest} ->
    let href = {node with value = dest} |> Range.map eval_tape |> V.extract_text in
    let content =
      match title with
      | None -> [T.Transclude {addr = Addr.user_addr href; target = T.Title; modifier = Identity}]
      | Some title -> {node with value = eval_tape title} |> V.extract_content
    in
    emit_content_node ~loc @@ Link {href; content}

  | Math (mode, body) ->
    let content =
      {node with value = eval_tape body} |> V.extract_content
    in
    emit_content_node ~loc @@ KaTeX (mode, content)

  | Xml_tag (name, attrs, body) ->
    let rec process : _ list -> T.xml_attr list =
      function
      | [] -> []
      | (key, v) :: attrs ->
        T.{key; value = V.extract_text {node with value = eval_tape v}} :: process attrs
    in
    let name = {prefix = name.prefix; uname = name.uname; xmlns = name.xmlns} in
    let content = {node with value = eval_tape body} |> V.extract_content in
    emit_content_node ~loc @@ T.Xml_elt {name; attrs = process attrs; content}

  | Query_polarity pol ->
    focus ?loc @@ V.Query_polarity pol

  | Query_mode mode ->
    focus ?loc @@ V.Query_mode mode

  | Query_rel ->
    let mode = eval_pop_arg ~loc |> V.extract_query_mode in
    let pol = eval_pop_arg ~loc |> V.extract_query_polarity in
    let rel = eval_pop_arg ~loc |> V.extract_text in
    let addr = eval_pop_arg ~loc |> V.extract_query_addr_expr in
    focus ?loc @@ V.Query_expr (Query.rel mode pol rel addr)

  | Query_isect ->
    let queries =
      Tape.pop_args () |> List.map @@ fun arg ->
      arg |> Range.map eval_tape |> V.extract_query_expr
    in
    focus ?loc @@ V.Query_expr (Query.isect queries)

  | Query_union ->
    let queries =
      Tape.pop_args () |> List.map @@ fun arg ->
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
      let tape = qfun.value @ [{node with value = Syn.Sym x}] in
      {node with value = eval_tape tape} |> V.extract_query_expr
    in
    focus ?loc @@ V.Query_expr (QLN.isect_fam q x qx)

  | Query_union_fam ->
    let q = eval_pop_arg ~loc |> V.extract_query_expr in
    let qfun = Tape.pop_arg ~loc in
    let x = Symbol.fresh () in
    let qx =
      let tape = qfun.value @ [{node with value = Syn.Sym x}] in
      {node with value = eval_tape tape} |> V.extract_query_expr
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

  | Query_builtin builtin ->
    let addr = eval_pop_arg ~loc |> V.extract_query_addr_expr in
    let r =
      match builtin with
      | `Taxon -> Query.Rel.taxa
      | `Author -> Query.Rel.authors
      | `Tag -> Query.Rel.tags
    in
    let q = Query.rel Edges Incoming r addr in
    focus ?loc:node.loc @@ V.Query_expr q

  | TeX_cs cs ->
    emit_content_node ~loc @@ TeX_cs cs

  | Transclude ->
    let flags = get_transclusion_flags ~loc in
    let overrides = get_frontmatter_overrides ~loc in
    let addr = Addr.user_addr (pop_text_arg ~loc) in
    emit_content_node ~loc @@ T.Transclude {addr; target = Full (flags, overrides); modifier = Identity}

  | Subtree (addr_opt, nodes) ->
    let flags = get_transclusion_flags ~loc in
    let overrides = get_frontmatter_overrides ~loc in
    let addr =
      match addr_opt with
      | Some addr -> Addr.user_addr addr
      | None -> Addr.fresh ()
    in
    let subtree = eval_tree_inner ~addr nodes in
    let frontmatter = Frontmatter.get () in
    let subtree = {subtree with frontmatter = {subtree.frontmatter with addr; designated_parent = Some frontmatter.addr}} in
    Emitted_trees.modify @@ List.cons subtree;
    let transclusion = T.{addr; target = Full (flags, overrides); modifier = Identity} in
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
      let img = T.Inline T.{format = "svg+xml"; base64} in
      let content = [T.Img img] in
      let sources = [
        T.{type_ = "latex"; part = "preamble"; source = preamble};
        T.{type_ = "latex"; part = "body"; source = body}
      ] in
      let resource = T.{hash; content; sources} in
      [T.Resource resource]
    in
    let job = LaTeX_to_svg {hash; source; content} in
    Jobs.modify (List.cons job);
    let transclusion =
      let addr = Addr.hash_addr hash in
      let target = T.Mainmatter in
      T.{addr; target; modifier = Identity}
    in
    emit_content_node ~loc @@ T.Transclude transclusion

  | Object {self; methods} ->
    let table =
      let env = Lex_env.read () in
      let add (name, body) =
        let super = Symbol.fresh () in
        V.MethodTable.add name V.{body; self; super; env}
      in
      List.fold_right add methods V.MethodTable.empty
    in
    let sym = Symbol.named ["obj"] in
    Heap.modify @@ Env.add sym V.{prototype = None; methods = table};
    focus ?loc:node.loc @@ V.Obj sym

  | Patch {obj; self; super; methods} ->
    let obj_ptr = {node with value = obj} |> Range.map eval_tape |> V.extract_obj_ptr in
    let table =
      let env = Lex_env.read () in
      let add (name, body) =
        V.MethodTable.add name
          V.{body; self; super; env}
      in
      List.fold_right add methods V.MethodTable.empty
    in
    let sym = Symbol.named ["obj"] in
    Heap.modify @@ Env.add sym V.{prototype = Some obj_ptr; methods = table};
    focus ?loc:node.loc @@ V.Obj sym

  | Group (d, body) ->
    let l, r = delim_to_strings d in
    let content =
      T.Text l
      :: V.extract_content {node with value = eval_tape body}
      @ [T.Text r]
    in
    focus ?loc:node.loc @@ V.Content content

  | Call (obj, method_name) ->
    let sym = {node with value = obj} |> Range.map eval_tape |> V.extract_obj_ptr in
    let rec call_method (obj : V.obj) =
      let proto_val =
        obj.prototype |> Option.map @@ fun ptr ->
        V.Obj ptr
      in
      match V.MethodTable.find_opt method_name obj.methods with
      | Some mthd ->
        let env =
          let env = Env.add mthd.self (V.Obj sym) mthd.env in
          match proto_val with
          | None -> env
          | Some proto_val ->
            Env.add mthd.super proto_val env
        in
        Lex_env.run ~env @@ fun () ->
        eval_tape mthd.body
      | None ->
        match obj.prototype with
        | Some proto ->
          call_method @@ Env.find proto @@ Heap.get ()
        | None ->
          Reporter.fatalf ?loc:node.loc Type_error
            "tried to call unbound method `%s`" method_name
    in
    let result = call_method @@ Env.find sym @@ Heap.get () in
    focus ?loc:node.loc result

  | Put (k, v, body) ->
    let k = {node with value = k} |> Range.map eval_tape |> V.extract_sym in
    let body =
      Dyn_env.scope (Env.add k @@ eval_tape v) @@ fun () ->
      eval_tape body
    in
    focus ?loc:node.loc body

  | Default (k, v, body) ->
    let k = {node with value = k} |> Range.map eval_tape |> V.extract_sym in
    let body =
      let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval_tape v) flenv in
      Dyn_env.scope upd @@ fun () ->
      eval_tape body
    in
    focus ?loc:node.loc body

  | Get k ->
    let k = {node with value = k} |> Range.map eval_tape |> V.extract_sym in
    let env = Dyn_env.read () in
    begin
      match Env.find_opt k env with
      | None ->
        Reporter.fatalf ?loc:node.loc Resolution_error
          "could not find fluid binding named %a in environment %a"
          Symbol.pp k
          (Env.pp V.pp) env
      | Some v -> focus ?loc:node.loc v
    end

  | Verbatim str ->
    emit_content_node ~loc @@ CDATA str

  | Title ->
    let title = pop_content_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with title = title});
    process_tape ()

  | Parent ->
    let addr = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with designated_parent = Some (Addr.user_addr addr)});
    process_tape ()

  | Meta ->
    let k = pop_text_arg ~loc  in
    let v = pop_content_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with metas = fm.metas @ [k,v]});
    process_tape ()

  | Author ->
    let author = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with attributions = fm.attributions @ [T.Author author]});
    process_tape ()

  | Contributor ->
    let author = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with attributions = fm.attributions @ [T.Contributor author]});
    process_tape ()

  | Tag ->
    let tag = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with tags = fm.tags @ [tag]});
    process_tape ()

  | Date ->
    let date_str = pop_text_arg ~loc in
    begin
      match Date.parse date_str with
      | None ->
        Reporter.fatalf ?loc:node.loc Parse_error "Invalid date string `%s`" date_str
      | Some date ->
        Frontmatter.modify (fun fm -> {fm with dates = fm.dates @ [date]});
        process_tape ()
    end

  | Number ->
    let num = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with number = Some num});
    process_tape ()

  | Taxon ->
    let taxon = pop_text_arg ~loc in
    Frontmatter.modify (fun fm -> {fm with taxon = Some taxon});
    process_tape ()

  | Sym sym ->
    focus ?loc:node.loc @@ V.Sym sym

and eval_var ~loc x =
  match Env.find_opt x @@ Lex_env.read () with
  | Some v -> focus ?loc v
  | None ->
    Reporter.fatalf ?loc Resolution_error
      "could not find variable named %a"
      Symbol.pp x

and focus ?loc =
  function
  | V.Clo (rho, xs, body) ->
    focus_clo ?loc rho xs body

  | V.Content content ->
    begin
      match process_tape () with
      | V.Content content' -> V.Content (content @ content')
      | value -> value
    end

  | V.Query_expr _ | V.Query_mode _ | V.Query_polarity _ | V.Sym _ | V.Obj _ | V.Bool _ as v ->
    begin
      match process_tape () with
      | V.Content content when T.strip_whitespace content = [] -> v
      | _ -> Reporter.fatalf ?loc Type_error "Expected solitary node"
    end

and focus_clo ?loc rho xs body =
  match xs with
  | [] ->
    focus ?loc @@
    Lex_env.run ~env:rho @@ fun () ->
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
        | Content nodes when T.strip_whitespace nodes = [] ->
          Clo (rho, xs, body)
        | _ ->
          Reporter.fatalf ?loc Type_error "Expected %i additional arguments" (List.length xs)
      end

and emit_content_node ~loc content =
  focus ?loc @@ Content [content]

and eval_tree_inner ~addr (tree : Syn.tree) : T.content T.article =
  let attribution_is_author =
    function
    | T.Author _ -> true
    | _ -> false
  in
  let outer_frontmatter = Frontmatter.get () in
  let frontmatter =
    {T.empty_frontmatter with
     addr;
     source_path = outer_frontmatter.source_path;
     attributions = List.filter attribution_is_author outer_frontmatter.attributions;
     dates = outer_frontmatter.dates}
  in
  Frontmatter.run ~init:frontmatter @@ fun () ->
  let mainmatter = {value = eval_tape tree; loc = None} |> V.extract_content in
  let frontmatter = Frontmatter.get () in
  let backmatter = default_backmatter ~addr in
  T.{frontmatter; mainmatter; backmatter}

let eval_tree ~addr ~source_path (tree : Syn.tree) : result =
  let fm = {T.empty_frontmatter with addr; source_path} in
  Frontmatter.run ~init:fm @@ fun () ->
  Emitted_trees.run ~init:[] @@ fun () ->
  Jobs.run ~init:[] @@ fun () ->
  Heap.run ~init:Env.empty @@ fun () ->
  Lex_env.run ~env:Env.empty @@ fun () ->
  Dyn_env.run ~env:Env.empty @@ fun () ->
  let main = eval_tree_inner ~addr tree in
  let side = Emitted_trees.get () in
  let jobs = Jobs.get () in
  {main; side; jobs}
