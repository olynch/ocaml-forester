open Forester_prelude
open Base
open Bwd
module Q = Query

module G =
struct
  module G = Graph.Imperative.Digraph.ConcreteBidirectional (Addr)
  include G
  include Graph.Oper.I (G)

  let safe_succ g x =
    if mem_vertex g x then succ g x else []

  let safe_fold_succ f g x acc =
    if mem_vertex g x then
      fold_succ f g x acc
    else
      acc

  let safe_pred g x =
    if mem_vertex g x then pred g x else []
end

module Make () =
struct
  module Graphs =
  struct
    let all_addrs_ref : Addr_set.t ref =
      ref Addr_set.empty

    let rel_to_graph : (Q.Rel.t, G.t) Hashtbl.t =
      Hashtbl.create 20

    let rel_to_preorder : (Q.Rel.t, G.t) Hashtbl.t =
      Hashtbl.create 20

    let get_graph rel =
      match Hashtbl.find_opt rel_to_graph rel with
      | None ->
        let gph = G.create () in
        Hashtbl.add rel_to_graph rel gph;
        gph
      | Some gph -> gph

    let get_preorder rel =
      match Hashtbl.find_opt rel_to_preorder rel with
      | None ->
        let message = Format.asprintf "Computing reflexive-transitive closure of %s" rel in
        Reporter.profile message @@ fun () ->
        let gph = G.transitive_closure ~reflexive:true @@ get_graph rel in
        Hashtbl.add rel_to_preorder rel gph;
        gph
      | Some gph -> gph

    let get (mode : Q.mode) =
      match mode with
      | Edges -> get_graph
      | Paths -> get_preorder

    let register_addr addr =
      Hashtbl.clear rel_to_preorder;
      all_addrs_ref := Addr_set.add addr !all_addrs_ref;
      rel_to_graph |> Hashtbl.iter @@ fun _ gph ->
      G.add_vertex gph addr

    let add_edge rel ~source ~target =
      Hashtbl.remove rel_to_preorder rel;
      let gph = get_graph rel in
      G.add_edge gph source target
  end

  module Lex_env = Algaeff.Reader.Make (struct type t = Sem.value Env.t end)
  module Dyn_env = Algaeff.Reader.Make (struct type t = Sem.value Env.t end)
  module Heap = Algaeff.State.Make (struct type t = Sem.obj Env.t end)
  module Emitted_trees = Algaeff.State.Make (struct type t = Sem.tree list end)
  module Fm = Algaeff.State.Make (struct type t = Sem.frontmatter end)
  module Scope = Algaeff.State.Make (Addr)

  let get_transclusion_opts () =
    let dynenv = Dyn_env.read () in
    let title_override =
      Option.bind (Env.find_opt Expand.Builtins.Transclude.title_sym dynenv) @@ function
      | Sem.VContent content -> Some content
      | _ -> None
    in
    let taxon_override =
      match Env.find_opt Expand.Builtins.Transclude.taxon_sym dynenv with
      | Some (VContent [{value = Sem.Text text; _}]) -> Some text
      | _ -> None
    in
    let get_bool key default =
      match Env.find_opt key dynenv with
      | Some (VContent [{value = Sem.Text "true"; _}]) -> true
      | Some (VContent [{value = Sem.Text "false"; _}]) -> false
      | _ -> default
    in
    let expanded = get_bool Expand.Builtins.Transclude.expanded_sym true in
    let show_heading = get_bool Expand.Builtins.Transclude.show_heading_sym true in
    let toc = get_bool Expand.Builtins.Transclude.toc_sym true in
    let numbered = get_bool Expand.Builtins.Transclude.numbered_sym true in
    let show_metadata = get_bool Expand.Builtins.Transclude.show_metadata_sym false in
    Sem.{title_override; taxon_override; toc; show_heading; expanded; numbered; show_metadata}

  module Tape : sig
    val run : tape:Syn.t -> (unit -> 'a) -> 'a
    val pop_node_opt : unit -> Syn.node Range.located option

    val pop_arg_opt : unit -> Syn.t Range.located option
    val pop_arg : loc:Range.t option -> Syn.t Range.located
    val pop_args : unit -> Syn.t Range.located list
  end =
  struct
    module Tape = Algaeff.State.Make (struct type t = Syn.t end)

    let pop_node_opt () =
      match Tape.get () with
      | node :: nodes ->
        Tape.set nodes;
        Some node
      | [] -> None

    let pop_arg_opt () =
      match Tape.get () with
      | Range.{value = Syn.Group (Braces, arg); _} as node :: nodes ->
        Tape.set nodes;
        Some ({node with value = arg})
      | Range.{value = (Syn.Sym _ | Syn.Verbatim _ | Syn.Var _); _} as node :: nodes ->
        Tape.set nodes;
        Some ({node with value = [node]})
      | _ -> None

    let pop_arg ~loc =
      match pop_arg_opt () with
      | Some arg -> arg
      | None -> Reporter.fatalf ?loc Type_error "Expected argument"

    let pop_args () =
      let rec loop acc =
        match pop_arg_opt () with
        | Some arg -> loop @@ Bwd.Snoc (acc, arg)
        | None -> Bwd.prepend acc []
      in
      loop Bwd.Emp

    let run ~tape =
      Tape.run ~init:tape
  end


  let rec process_tape () =
    match Tape.pop_node_opt () with
    | None -> Sem.VContent []
    | Some node ->
      eval_node node

  and eval_tape nodes =
    Tape.run ~tape:nodes process_tape

  and eval_node node : Sem.value  =
    match node.value with
    | Syn.Var x ->
      begin
        match Env.find_opt x @@ Lex_env.read () with
        | Some v -> focus ?loc:node.loc v
        | None ->
          Reporter.fatalf ?loc:node.loc Resolution_error
            "could not find variable named %a"
            Symbol.pp x
      end

    | Text str ->
      emit_content_node {node with value = Sem.Text str}

    | Prim p ->
      let content = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_content |> Sem.trim_whitespace in
      emit_content_node {node with value = Sem.Prim (p, content)}

    | Fun (xs, body) ->
      let env = Lex_env.read () in
      focus_clo env xs body

    | Ref ->
      let scope = Scope.get () in
      let dest = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_addr in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:dest;
      emit_content_node {node with value = Sem.Ref dest}

    | Link {title; dest} ->
      let scope = Scope.get () in
      let dest = {node with value = dest} |> Range.map eval_tape |> Sem.extract_addr in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:dest;
      let title =
        title |> Option.map @@ fun x ->
        Sem.extract_content {node with value = eval_tape x}
      in
      emit_content_node {node with value = Sem.Link (dest, title, Identity)}

    | Math (mmode, body) ->
      let body = Sem.extract_content {node with value = eval_tape body} in
      emit_content_node {node with value = Sem.Math (mmode, body)}

    | Xml_tag (name, attrs, body) ->
      let rec process =
        function
        | [] -> []
        | (k,v) :: attrs ->
          let processed = process attrs in
          if List.mem_assoc k processed then begin
            Reporter.emitf ?loc:node.loc Duplicate_attribute
              "skipping duplicate XML attribute `%a`" pp_xml_resolved_qname k;
            processed
          end else
            (k, Sem.extract_content {node with value = eval_tape v}) :: processed
      in
      let tag = Sem.Xml_tag (name, process attrs, Sem.extract_content {node with value = eval_tape body}) in
      emit_content_node {node with value = tag}

    | Query_polarity pol ->
      focus ?loc:node.loc @@ VQuery_polarity pol

    | Query_mode mode ->
      focus ?loc:node.loc @@ VQuery_mode mode

    | Query_rel ->
      let mode = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_mode in
      let pol = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_polarity in
      let rel = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string in
      let addr = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_addr in
      focus ?loc:node.loc @@ VQuery (Sem.Query.rel mode pol rel addr)

    | Query_isect ->
      let queries =
        Tape.pop_args () |> List.map @@ fun arg ->
        arg |> Range.map eval_tape |> Sem.extract_query_node
      in
      focus ?loc:node.loc @@ VQuery (Sem.Query.isect queries)

    | Query_union ->
      let queries =
        Tape.pop_args () |> List.map @@ fun arg ->
        arg |> Range.map eval_tape |> Sem.extract_query_node
      in
      focus ?loc:node.loc @@ VQuery (Sem.Query.union queries)

    | Query_compl ->
      let q = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_node in
      focus ?loc:node.loc @@ VQuery (Complement q)

    | Query_isect_fam ->
      let q = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_node in
      let qfun = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_clo in
      focus ?loc:node.loc @@ VQuery (Sem.Isect_fam (q, qfun))

    | Query_union_fam ->
      let q = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_node in
      let qfun = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_clo in
      focus ?loc:node.loc @@ VQuery (Sem.Union_fam (q, qfun))

    | Query_isect_fam_rel ->
      let q = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_node in
      let mode = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_mode in
      let pol = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_polarity in
      let rel = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string in
      focus ?loc:node.loc @@ VQuery (Sem.Query.isect_fam_rel q mode pol rel)

    | Query_union_fam_rel ->
      let q = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_node in
      let mode = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_mode in
      let pol = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_polarity in
      let rel = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string in
      focus ?loc:node.loc @@ VQuery (Sem.Query.union_fam_rel q mode pol rel)

    | Query_builtin builtin ->
      let addr = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_addr  in
      let r =
        match builtin with
        | `Taxon -> Q.Rel.taxa
        | `Author -> Q.Rel.authors
        | `Tag -> Q.Rel.tags
      in
      let q = Sem.Query.rel Edges Incoming r addr in
      focus ?loc:node.loc @@ VQuery q

    | TeX_cs cs ->
      emit_content_node {node with value = Sem.TeX_cs cs}

    | Transclude ->
      let addr = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_addr in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:addr;
      let opts = get_transclusion_opts () in
      emit_content_node {node with value = Sem.Transclude (opts, addr)}

    | Subtree (addr, subtree_nodes) ->
      let addr =
        match addr with
        | Some addr -> User_addr addr
        | None -> Machine_addr (Oo.id (object end))
      in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:addr;
      let opts = get_transclusion_opts () in
      let subtree = eval_tree_inner ~addr subtree_nodes in
      let fm = Fm.get () in
      let subtree = {subtree with fm = {subtree.fm with physical_parent = Some fm.addr; designated_parent = Some fm.addr}} in
      Emitted_trees.modify @@ List.cons subtree;
      emit_content_node {node with value = Sem.Subtree (opts, subtree)}

    | Query_tree ->
      let opts =
        let opts = get_transclusion_opts () in
        match opts.title_override with
        | None -> {opts with show_heading = false; toc = false}
        | Some _ -> opts
      in
      let query = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_query_node in
      emit_content_node {node with value = Sem.Query_tree (opts, query)}

    | Embed_tex ->
      let preamble = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_content in
      let source = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_content in
      let embed = Sem.Embed_tex {preamble; source} in
      emit_content_node {node with value = embed}

    | Object {self; methods} ->
      let table =
        let env = Lex_env.read () in
        let add (name, body) =
          let super = Symbol.fresh [] in
          Sem.MethodTable.add name Sem.{body; self; super; env}
        in
        List.fold_right add methods Sem.MethodTable.empty
      in
      let sym = Symbol.fresh ["obj"] in
      Heap.modify @@ Env.add sym Sem.{prototype = None; methods = table};
      focus ?loc:node.loc @@ VObject sym

    | Patch {obj; self; super; methods} ->
      let obj_ptr = {node with value = obj} |> Range.map eval_tape |> Sem.extract_obj_ptr in
      let table =
        let env = Lex_env.read () in
        let add (name, body) =
          Sem.MethodTable.add name
            Sem.{body; self; super; env}
        in
        List.fold_right add methods Sem.MethodTable.empty
      in
      let sym = Symbol.fresh ["obj"] in
      Heap.modify @@ Env.add sym Sem.{prototype = Some obj_ptr; methods = table};
      focus ?loc:node.loc @@ VObject sym

    | Group (d, body) ->
      let l, r = Base.delim_to_strings d in
      let content =
        Range.locate_opt None (Sem.Text l)
        :: Sem.extract_content {node with value = eval_tape body}
        @ [Range.locate_opt None (Sem.Text r)]
      in
      focus ?loc:node.loc @@ VContent content

    | Call (obj, method_name) ->
      let sym = {node with value = obj} |> Range.map eval_tape |> Sem.extract_obj_ptr in
      let rec call_method (obj : Sem.obj) =
        let proto_val =
          obj.prototype |> Option.map @@ fun ptr ->
          Sem.VObject ptr
        in
        match Sem.MethodTable.find_opt method_name obj.methods with
        | Some mthd ->
          let env =
            let env = Env.add mthd.self (Sem.VObject sym) mthd.env in
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
      let k = {node with value = k} |> Range.map eval_tape |> Sem.extract_sym in
      let body =
        Dyn_env.scope (Env.add k @@ eval_tape v) @@ fun () ->
        eval_tape body
      in
      focus ?loc:node.loc body

    | Default (k, v, body) ->
      let k = {node with value = k} |> Range.map eval_tape |> Sem.extract_sym in
      let body =
        let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval_tape v) flenv in
        Dyn_env.scope upd @@ fun () ->
        eval_tape body
      in
      focus ?loc:node.loc body

    | Get k ->
      let k = {node with value = k} |> Range.map eval_tape |> Sem.extract_sym in
      begin
        let env = Dyn_env.read () in
        match Env.find_opt k env with
        | None ->
          Eio.traceln "getting %a from %a" Symbol.pp k (Env.pp Sem.pp_value) env;
          Reporter.fatalf ?loc:node.loc Resolution_error
            "could not find fluid binding named %a"
            Symbol.pp k
        | Some v -> focus ?loc:node.loc v
      end

    | Verbatim str ->
      emit_content_node {node with value = Sem.Verbatim str}

    | Title ->
      let title = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_content in
      Fm.modify (fun fm -> {fm with title = Some title});
      process_tape ()

    | Parent ->
      let addr = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_addr in
      Fm.modify (fun fm -> {fm with designated_parent = Some addr});
      process_tape ()

    | Meta ->
      let k = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string  in
      let v = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_content in
      Fm.modify (fun fm -> {fm with metas = fm.metas @ [k,v]});
      process_tape ()

    | Author ->
      let addr = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_addr in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.authors ~source:scope ~target:addr;
      Fm.modify (fun fm -> {fm with authors = fm.authors @ [addr]});
      process_tape ()

    | Contributor ->
      let addr = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_addr in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.contributors ~source:scope ~target:addr;
      Fm.modify (fun fm -> {fm with contributors = fm.contributors @ [addr]});
      process_tape ()

    | Tag ->
      let tag = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.tags ~source:scope ~target:(User_addr tag);
      Fm.modify (fun fm -> {fm with tags = fm.tags @ [tag]});
      process_tape ()

    | Date ->
      let date_str = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string in
      begin
        match Date.parse date_str with
        | None ->
          Reporter.fatalf ?loc:node.loc Parse_error "Invalid date string `%s`" date_str
        | Some date ->
          Fm.modify (fun fm -> {fm with dates = fm.dates @ [date]});
          process_tape ()
      end

    | Number ->
      let num = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string in
      Fm.modify (fun fm -> {fm with number = Some num});
      process_tape ()

    | Taxon ->
      let taxon = Tape.pop_arg ~loc:node.loc |> Range.map eval_tape |> Sem.extract_string in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.taxa ~source:scope ~target:(User_addr taxon);
      Fm.modify (fun fm -> {fm with taxon = Some taxon});
      process_tape ()

    | Sym sym ->
      focus ?loc:node.loc @@ VSym sym

  and focus ?loc v =
    match v with
    | VClo (rho, xs, body) ->
      focus_clo ?loc rho xs body

    | VContent content ->
      begin
        match process_tape () with
        | VContent content' -> VContent (content @ content')
        | value -> value
      end

    | VQuery _ | VQuery_mode _ | VQuery_polarity _ | VSym _ | VObject _ | VAddr _ ->
      begin
        match process_tape () with
        | VContent content when Sem.strip_whitespace content = [] -> v
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
          | Lazy -> VClo (Lex_env.read (), [(Strict, Symbol.fresh [])], arg.value)
        in
        let rhoy = Env.add y yval rho in
        focus_clo ?loc rhoy ys body
      | None ->
        begin
          match process_tape () with
          | VContent nodes when Sem.strip_whitespace nodes = [] ->
            VClo (rho, xs, body)
          | _ ->
            Reporter.fatalf ?loc Type_error "Expected %i additional arguments" (List.length xs)
        end

  and emit_content_node content =
    focus ?loc:content.loc @@ VContent [content]

  and eval_tree_inner ~addr (tree : Syn.tree) : Sem.tree =
    Graphs.register_addr addr;
    let scope =
      match addr with
      | User_addr _ -> addr
      | _ -> Scope.get ()
    in
    Scope.run ~init:scope @@ fun () ->
    let outer_fm = Fm.get () in
    let fm =
      {(Sem.empty_frontmatter ~addr) with
       source_path = outer_fm.source_path;
       authors = outer_fm.authors;
       dates = outer_fm.dates}
    in
    Fm.run ~init:fm @@ fun () ->
    let bm = Sem.default_backmatter ~addr in (*TODO*)
    let body = Sem.extract_content {value = eval_tape tree; loc = None} in
    let fm = Fm.get () in
    let open Sem in
    {fm; body; bm}

  let eval_tree ~addr ~source_path (tree : Syn.tree) : Sem.tree * Sem.tree list =
    let fm = {(Sem.empty_frontmatter ~addr) with source_path} in
    Fm.run ~init:fm @@ fun () ->
    Scope.run ~init:addr @@ fun () ->
    Emitted_trees.run ~init:[] @@ fun () ->
    Heap.run ~init:Env.empty @@ fun () ->
    Lex_env.run ~env:Env.empty @@ fun () ->
    Dyn_env.run ~env:Env.empty @@ fun () ->
    let tree = eval_tree_inner ~addr tree in
    let emitted = Emitted_trees.get () in
    tree, emitted

  module Query_engine =
  struct
    let query_rel mode pol rel addr =
      let fn =
        match pol with
        | Q.Incoming -> G.safe_pred
        | Q.Outgoing -> G.safe_succ
      in
      let gph = Graphs.get mode rel in
      Addr_set.of_list @@ fn gph addr

    let check_rel mode pol rel addr addr' =
      let gph = Graphs.get mode rel in
      match pol with
      | Q.Incoming -> G.mem_edge gph addr' addr
      | Q.Outgoing -> G.mem_edge gph addr addr'

    let rec check_query (q : Sem.query) addr =
      match q with
      | Rel ((mode, pol, rel), addr') ->
        check_rel mode pol rel addr' addr
      | Isect qs -> check_isect qs addr
      | Union qs -> check_union qs addr
      | Complement q ->
        not @@ check_query q addr
      | Isect_fam (q, qclo) ->
        let xs = Addr_set.to_list @@ run_query q in
        xs |> List.for_all @@ fun x ->
        check_query (inst_qclo qclo x) addr
      | Union_fam (q, qclo) ->
        let xs = Addr_set.to_list @@ run_query q in
        xs |> List.exists @@ fun x ->
        check_query (inst_qclo qclo x) addr

    and check_isect qs addr =
      qs |> List.for_all @@ fun q ->
      check_query q addr

    and check_union qs addr =
      qs |> List.exists @@ fun q ->
      check_query q addr


    and run_query (q : Sem.query) : Addr_set.t =
      match q with
      | Rel ((mode, pol, rel), addr) ->
        query_rel mode pol rel addr
      | Isect qs -> run_isect qs
      | Union qs -> run_union qs
      | Complement q ->
        Addr_set.diff !Graphs.all_addrs_ref @@ run_query q
      | Isect_fam (q, qclo) ->
        let xs = Addr_set.to_list @@ run_query q in
        run_isect @@ List.map (inst_qclo qclo) xs
      | Union_fam (q, qclo) ->
        let xs = Addr_set.to_list @@ run_query q in
        run_union @@ List.map (inst_qclo qclo) xs

    and inst_qclo qclo addr =
      match qclo with
      | QClo_rel (mode, pol, rel) ->
        Sem.Query.rel mode pol rel addr
      | QClo (env, z, qz) ->
        let vz = Sem.VAddr addr in
        let value =
          Heap.run ~init:Env.empty @@ fun () ->
          Lex_env.run ~env:(Env.add z vz env) @@ fun () ->
          Dyn_env.run ~env:Env.empty @@ fun () ->
          eval_tape qz
        in
        Sem.extract_query_node {value; loc = None}

    and run_isect =
      function
      | [] -> !Graphs.all_addrs_ref
      | q :: qs ->
        run_query q |> Addr_set.filter @@ check_isect qs

    and run_union qs =
      let alg q = Addr_set.union (run_query q) in
      List.fold_right alg qs Addr_set.empty

    and fold_set_operation opr running =
      function
      | [] -> running
      | q :: qs ->
        let s = run_query q in
        fold_set_operation opr (opr running s) qs
  end


  let run_query = Query_engine.run_query
end
