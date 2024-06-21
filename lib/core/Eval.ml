open Forester_prelude
open Base

module Q = Query

module G =
struct
  module G = Graph.Imperative.Digraph.Concrete (Addr)
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

    let rec check_query q addr =
      match Q.view q with
      | Q.Rel ((mode, pol, rel), addr') ->
        check_rel mode pol rel addr' addr
      | Q.Isect qs -> check_isect qs addr
      | Q.Union qs -> check_union qs addr
      | Q.Complement q ->
        not @@ check_query q addr
      | Q.Isect_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        xs |> List.for_all @@ fun x ->
        check_rel mode pol rel x addr
      | Q.Union_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        xs |> List.exists @@ fun x ->
        check_rel mode pol rel x addr

    and check_isect qs addr =
      qs |> List.for_all @@ fun q ->
      check_query q addr

    and check_union qs addr =
      qs |> List.exists @@ fun q ->
      check_query q addr


    and run_query q =
      match Q.view q with
      | Q.Rel ((mode, pol, rel), addr) ->
        query_rel mode pol rel addr
      | Q.Isect qs -> run_isect qs
      | Q.Union qs -> run_union qs
      | Q.Complement q ->
        Addr_set.diff !Graphs.all_addrs_ref @@ run_query q
      | Q.Isect_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        run_isect @@ List.map (Q.rel mode pol rel) xs
      | Q.Union_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        run_union @@ List.map (Q.rel mode pol rel) xs

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

  module Lex_env = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
  module Dyn_env = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
  module Heap = Algaeff.State.Make (struct type t = Sem.obj Env.t end)
  module Emitted_trees = Algaeff.State.Make (struct type t = Sem.tree list end)
  module Fm = Algaeff.State.Make (struct type t = Sem.frontmatter end)
  module Scope = Algaeff.State.Make (Addr)

  let get_transclusion_opts () =
    let dynenv = Dyn_env.read () in
    let title_override = Env.find_opt Expand.Builtins.Transclude.title_sym dynenv in
    let taxon_override =
      match Env.find_opt Expand.Builtins.Transclude.taxon_sym dynenv with
      | Some [{value = Sem.Text text; _}] -> Some text
      | _ -> None
    in
    let get_bool key default =
      match Env.find_opt key dynenv with
      | Some [{value = Sem.Text "true"; _}] -> true
      | Some [{value = Sem.Text "false"; _}] -> false
      | _ -> default
    in
    let expanded = get_bool Expand.Builtins.Transclude.expanded_sym true in
    let show_heading = get_bool Expand.Builtins.Transclude.show_heading_sym true in
    let toc = get_bool Expand.Builtins.Transclude.toc_sym true in
    let numbered = get_bool Expand.Builtins.Transclude.numbered_sym true in
    let show_metadata = get_bool Expand.Builtins.Transclude.show_metadata_sym false in
    Sem.{title_override; taxon_override; toc; show_heading; expanded; numbered; show_metadata}

  let pop_arg ~loc rest =
    match rest with
    | Range.{value = Syn.Group (Braces, arg); _} :: rest ->
      arg, rest
    | Range.{value = Syn.Verbatim str; _} as node :: rest ->
      [node], rest
    | _ ->
      Reporter.fatalf ?loc Type_error "Expected argument"

  let rec eval : Syn.t -> Sem.t =
    function
    | [] -> []
    | node :: rest ->
      eval_node node rest

  and eval_node : Syn.node Range.located -> Syn.t -> Sem.t =
    fun node rest ->
    match node.value with
    | Link {title; dest} ->
      let scope = Scope.get () in
      let dest = eval_addr dest in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:dest;
      let title = Option.map eval title in
      {node with value = Sem.Link (dest, title, Identity)} :: eval rest

    | Ref ->
      let dest, rest = pop_arg ~loc:node.loc rest in
      let scope = Scope.get () in
      let dest = eval_addr dest in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:dest;
      {node with value = Sem.Ref dest} :: eval rest

    | Math (mmode, e) ->
      {node with value = Sem.Math (mmode, eval e)} :: eval rest

    | Prim p ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      {node with value = Sem.Prim (p, eval_trim arg)} :: eval rest

    | Xml_tag (name, attrs, body) ->
      let rec process attrs = match attrs with
        | [] -> []
        | (k,v) :: attrs ->
          let processed = process attrs in
          if List.mem_assoc k processed then begin
            Reporter.emitf ?loc:node.loc Duplicate_attribute
              "skipping duplicate XML attribute `%a`" pp_xml_resolved_qname k;
            processed
          end else
            (k, eval v) :: processed
      in
      {node with value = Sem.Xml_tag (name, process attrs, eval body)} :: eval rest

    | TeX_cs cs ->
      {node with value = Sem.TeX_cs cs} :: eval rest

    | Transclude ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:addr;
      let opts = get_transclusion_opts () in
      {node with value = Sem.Transclude (opts, addr)} :: eval rest

    | Subtree (addr, nodes) ->
      let addr =
        match addr with
        | Some addr -> User_addr addr
        | None -> Machine_addr (Oo.id (object end))
      in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:addr;
      let opts = get_transclusion_opts () in
      let subtree = eval_tree_inner ~addr nodes in
      let fm = Fm.get () in
      let subtree = {subtree with fm = {subtree.fm with physical_parent = Some fm.addr; designated_parent = Some fm.addr}} in
      begin
        Emitted_trees.modify @@ fun trees ->
        subtree :: trees
      end;
      {node with value = Sem.Subtree (opts, subtree)} :: eval rest

    | If_tex (x , y) ->
      let x = eval x in
      let y = eval y in
      {node with value = Sem.If_tex (x, y)} :: eval rest

    | Query query ->
      let opts = get_transclusion_opts () in
      let opts =
        match opts.title_override with
        | None -> {opts with show_heading = false; toc = false}
        | Some _ -> opts
      in
      let query = Q.map eval_addr query in
      {node with value = Sem.Query (opts, query)} :: eval rest

    | Embed_tex ->
      let preamble, rest = pop_arg ~loc:node.loc rest in
      let source, rest = pop_arg ~loc:node.loc rest in
      {node with value = Sem.Embed_tex {preamble = eval preamble; source = eval source}} :: eval rest

    | Lam (xs, body) ->
      let rec loop xs rest =
        match xs, rest with
        | [], rest -> eval body, rest
        | x :: xs, Range.{value = Syn.Group (Braces, u); loc = loc'} :: rest ->
          Lex_env.scope (Env.add x (eval u)) @@ fun () ->
          loop xs rest
        | x :: xs, Range.{value = Syn.Verbatim str; loc = loc'} :: rest ->
          let verb = [Range.{value = Sem.Verbatim str; loc = loc'}] in
          Lex_env.scope (Env.add x verb) @@ fun () ->
          loop xs rest
        | _ ->
          Reporter.fatalf Type_error ?loc:node.loc
            "expected function to be applied to `%i` additional arguments"
            (List.length xs)
      in
      let body, rest = loop xs rest in
      body @ eval rest

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
      {node with value = Sem.Object sym} :: eval rest

    | Patch {obj; self; super; methods} ->
      begin
        match eval_strip obj with
        | [Range.{value = Sem.Object obj_ptr; _}] ->
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
          {node with value = Sem.Object sym} :: eval rest
        | xs ->
          Reporter.fatalf ?loc:node.loc Type_error
            "tried to patch non-object"
      end

    | Call (obj, method_name) ->
      begin
        match eval_strip obj with
        | [Range.{value = Sem.Object sym; _}] as obj_val ->
          let rec call_method (obj : Sem.obj) =
            let proto_val =
              obj.prototype |> Option.map @@ fun ptr ->
              [Range.locate_opt None @@ Sem.Object ptr]
            in
            match Sem.MethodTable.find_opt method_name obj.methods with
            | Some mthd ->
              let env =
                let env = Env.add mthd.self obj_val mthd.env in
                match proto_val with
                | None -> env
                | Some proto_val ->
                  Env.add mthd.super proto_val env
              in
              Lex_env.scope (fun _ -> env) @@ fun () ->
              eval mthd.body
            | None ->
              match obj.prototype with
              | Some proto ->
                call_method @@ Env.find proto @@ Heap.get ()
              | None ->
                Reporter.fatalf ?loc:node.loc Type_error
                  "tried to call unbound method `%s`" method_name
          in
          let result = call_method @@ Env.find sym @@ Heap.get () in
          result @ eval rest
        | xs ->
          Reporter.fatalf ?loc:node.loc Type_error
            "tried to call method `%s` on non-object: %a" method_name Sem.pp xs
      end

    | Var x ->
      begin
        match Env.find_opt x @@ Lex_env.read () with
        | None ->
          Reporter.fatalf ?loc:node.loc Resolution_error
            "could not find variable named %a"
            Symbol.pp x
        | Some v -> v @ eval rest
      end

    | Put (k, v, body) ->
      let body =
        Dyn_env.scope (Env.add k @@ eval v) @@ fun () ->
        eval body
      in
      body @ eval rest

    | Default (k, v, body) ->
      let body =
        let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval v) flenv in
        Dyn_env.scope upd @@ fun () ->
        eval body
      in
      body @ eval rest

    | Get key ->
      begin
        let env = Dyn_env.read () in
        match Env.find_opt key env with
        | None ->
          Eio.traceln "getting %a from %a" Symbol.pp key (Env.pp Sem.pp) env;
          Reporter.fatalf ?loc:node.loc Resolution_error
            "could not find fluid binding named %a"
            Symbol.pp key
        | Some v -> v @ eval rest
      end

    | Verbatim str ->
      {node with value = Sem.Verbatim str} :: eval rest

    | Group _ | Text _ ->
      eval_textual @@ node :: rest

    | Title ->
      let title, rest = pop_arg ~loc:node.loc rest in
      let title = eval title in
      Fm.modify (fun fm -> {fm with title = Some title});
      eval rest

    | Parent ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      Fm.modify (fun fm -> {fm with designated_parent = Some addr});
      eval rest

    | Meta ->
      let argk, rest = pop_arg ~loc:node.loc rest in
      let argv, rest = pop_arg ~loc:node.loc rest in
      let k = eval_as_string argk in
      let v = eval argv in
      Fm.modify (fun fm -> {fm with metas = fm.metas @ [k,v]});
      eval rest

    | Author ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.authorship ~source:scope ~target:addr;
      Fm.modify (fun fm -> {fm with authors = fm.authors @ [addr]});
      eval rest

    | Contributor ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.contributorship ~source:scope ~target:addr;
      Fm.modify (fun fm -> {fm with contributors = fm.contributors @ [addr]});
      eval rest

    | Tag ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let tag = eval_as_string arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.tags ~source:scope ~target:(User_addr tag);
      Fm.modify (fun fm -> {fm with tags = fm.tags @ [tag]});
      eval rest

    | Date ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let date = eval_as_string arg in
      begin
        match Date.parse date with
        | None ->
          Reporter.fatalf Parse_error "Invalid date string `%s`" date
        | Some date ->
          Fm.modify (fun fm -> {fm with dates = fm.dates @ [date]});
          eval rest
      end

    | Number ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let num = eval_as_string arg in
      Fm.modify (fun fm -> {fm with number = Some num});
      eval rest

    | Taxon ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let taxon = eval_as_string arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.taxa ~source:scope ~target:(User_addr taxon);
      Fm.modify (fun fm -> {fm with taxon = Some taxon});
      eval rest

  and eval_strip xs = Sem.strip_whitespace @@ eval xs

  and eval_trim xs = Sem.trim_whitespace @@ eval xs

  and eval_textual ?(prefix = []) : Syn.t -> Sem.t =
    function
    | {value = Group (d, xs); _} :: rest ->
      let l, r =
        match d with
        | Braces -> "{", "}"
        | Squares -> "[", "]"
        | Parens -> "(", ")"
      in
      eval_textual ~prefix:(l :: prefix) @@ xs @ Asai.Range.locate_opt None (Syn.Text r) :: rest
    | {value = Text x; _} :: rest ->
      eval_textual ~prefix:(x :: prefix) @@ rest
    | rest ->
      let txt = String.concat "" @@ List.rev prefix in
      Range.locate_opt None (Sem.Text txt) :: eval rest

  and eval_as_string xs =
    Sem.string_of_nodes @@ eval_textual xs

  and eval_addr xs =
    User_addr (eval_as_string xs)

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
    let body = eval tree in
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

  let run_query = Query_engine.run_query
end
