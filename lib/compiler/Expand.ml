open Forester_prelude
open Forester_core

module Unit_map = Map.Make(String)
module R = Resolver
module Sc = R.Scope

type exports = R.P.data Trie.Untagged.t

module Env = struct
  type t = exports Unit_map.t
  let empty = Unit_map.empty
end

module U = Algaeff.State.Make(struct type t = Env.t end)

module Builtins = struct

  let create_sym path =
    let sym = Symbol.named path in
    sym,
    fun () ->
      Sc.include_singleton path @@
        Term [Range.locate_opt None (Syn.Sym sym)]

  let register_builtins builtins =
    Sc.include_subtree [] @@
    Yuujinchou.Trie.of_seq @@
    let@ path, node = Seq.map @~ List.to_seq builtins in
    path, (R.P.Term [Range.locate_opt None node], ())

  module Transclude = struct
    let expanded_sym, alloc_expanded = create_sym ["transclude"; "expanded"]
    let show_heading_sym, alloc_show_heading = create_sym ["transclude"; "heading"]
    let toc_sym, alloc_toc = create_sym ["transclude"; "toc"]
    let numbered_sym, alloc_numbered = create_sym ["transclude"; "numbered"]
    let show_metadata_sym, alloc_show_metadata = create_sym ["transclude"; "metadata"]
  end
end

let rec expand_method_calls (base : Syn.t) : Code.t -> Syn.t * Code.t = function
  | { value = Hash_ident x; loc } :: rest ->
    let base = [Range.{ value = Syn.Call (base, x); loc }] in
    expand_method_calls base rest
  | rest -> base, rest

let rec expand : Code.t -> Syn.t = function
  | [] -> []
  | { value = Hash_ident x; loc } :: rest ->
    { value = Syn.Text x; loc } :: expand rest
  | { value = Text x; loc } :: rest ->
    { value = Syn.Text x; loc } :: expand rest
  | { value = Verbatim x; loc } :: rest ->
    { value = Syn.Verbatim x; loc } :: expand rest
  | { value = Namespace (path, body); _ } :: rest ->
    let result =
      let@ () = Sc.section path in
      expand body
    in
    result @ expand rest
  | { value = Open path; _ } :: rest ->
    let@ () = Sc.section path in
    Sc.modify_visible @@
      R.Lang.union
        [
          R.Lang.all;
          R.Lang.renaming path []
        ];
    expand rest
  | { value = Group (Squares, title); loc = loc1 } :: { value = Group (Parens, dest); _ } :: rest ->
    let dest = expand dest in
    let title = Option.some @@ expand title in
    let link = Syn.Link { dest; title } in
    { value = link; loc = loc1 } :: expand rest
  | { value = Group (Squares, [{ value = Group (Squares, dest); _ }]); loc } :: rest ->
    let dest = expand dest in
    { value = Syn.Link { dest; title = None }; loc } :: expand rest
  | { value = Group (d, xs); loc } :: rest ->
    { value = Syn.Group (d, expand xs); loc } :: expand rest
  | { value = Subtree (addr, nodes); loc } :: rest ->
    let subtree = expand_tree_inner @@ Code.{ source_path = None; addr = addr; code = nodes } in
    { value = Syn.Subtree (addr, subtree); loc } :: expand rest
  | { value = Math (m, xs); loc } :: rest ->
    { value = Syn.Math (m, expand xs); loc } :: expand rest
  | { value = Ident path; loc } :: rest ->
    let out, rest = expand_method_calls (expand_ident loc path) rest in
    out @ expand rest
  | { value = Scope body; _ } :: rest ->
    let body =
      let@ () = Sc.section [] in
      expand body
    in
    body @ expand rest
  | { value = Put (k, v); loc } :: rest ->
    let k = expand_ident loc k in
    let v = expand v in
    [{ value = Syn.Put (k, v, expand rest); loc }]
  | { value = Default (k, v); loc } :: rest ->
    let k = expand_ident loc k in
    let v = expand v in
    [{ value = Syn.Default (k, v, expand rest); loc }]
  | { value = Get k; loc } :: rest ->
    let k = expand_ident loc k in
    { value = Syn.Get k; loc } :: expand rest
  | { value = Fun (xs, body); loc } :: rest ->
    expand_lambda loc (xs, body) :: expand rest
  | { value = Object { self; methods }; loc } :: rest ->
    let self, methods =
      let@ () = Sc.section [] in
      let sym = Symbol.fresh () in
      let var = Range.{ value = Syn.Var sym; loc } in
      begin
        let@ self = Option.iter @~ self in
        Sc.import_singleton self @@ R.P.Term [var]
      end;
      sym, List.map expand_method methods
    in
    { value = Syn.Object { self; methods }; loc } :: expand rest
  | { value = Patch { obj; self; methods }; loc } :: rest ->
    let self, super, methods =
      let@ () = Sc.section [] in
      let self_sym = Symbol.fresh () in
      let super_sym = Symbol.fresh () in
      let self_var = Range.locate_opt None @@ Syn.Var self_sym in
      let super_var = Range.locate_opt None @@ Syn.Var super_sym in
      begin
        let@ self = Option.iter @~ self in
        Sc.import_singleton self @@ Term [self_var];
        Sc.import_singleton (self @ ["super"]) @@ Term [super_var]
      end;
      self_sym, super_sym, List.map expand_method methods
    in
    let patched = Syn.Patch { obj = expand obj; self; super; methods } in
    { value = patched; loc } :: expand rest
  | { value = Call (obj, method_name); loc } :: rest ->
    { value = Syn.Call (expand obj, method_name); loc } :: expand rest
  | { value = Xml_tag (title, attrs, body); loc } :: rest ->
    let title = expand_xml_ident loc title in
    let attrs =
      let@ k, v = List.map @~ attrs in
      expand_xml_ident loc k, expand v
    in
    let body = expand body in
    { value = Syn.Xml_tag (title, attrs, body); loc } :: expand rest
  | { value = Import (vis, dep); loc } :: rest ->
    let units = U.get () in
    let import = Unit_map.find_opt dep units in
    begin
      match import with
      | None ->
        Reporter.emitf ?loc: loc Tree_not_found "Could not find tree named `%s'" dep
      | Some tree ->
        begin
          match vis with
          | Public -> Sc.include_subtree [] tree
          | Private -> Sc.import_subtree [] tree
        end
    end;
    expand rest
  | { value = Let (a, bs, def); loc } :: rest ->
    let lam = expand_lambda loc (bs, def) in
    let@ () = Sc.section [] in
    Sc.import_singleton a @@ Term [lam];
    expand rest
  | { value = Def (path, xs, body); loc } :: rest ->
    let lam = expand_lambda loc (xs, body) in
    Sc.include_singleton path @@ Term [lam];
    expand rest
  | { value = Decl_xmlns (prefix, xmlns); _ } :: rest ->
    let path = ["xmlns"; prefix] in
    Sc.include_singleton path @@ Xmlns { prefix; xmlns };
    expand rest
  | { value = Alloc path; loc } :: rest ->
    let symbol = Symbol.named path in
    Sc.include_singleton path @@ Term [Range.locate_opt loc (Syn.Sym symbol)];
    expand rest

and expand_method (key, body) =
  key, expand body

and expand_lambda loc (xs, body) =
  let@ () = Sc.section [] in
  let syms =
    let@ strategy, x = List.map @~ xs in
    let sym = Symbol.named x in
    let var = Range.locate_opt None @@ Syn.Var sym in
    Sc.import_singleton x @@ Term [var];
    strategy, sym
  in
  Range.{ value = Syn.Fun (syms, expand body); loc }

and expand_ident loc path =
  match Sc.resolve path, path with
  | None, [name] ->
    begin
      match TeX_cs.parse name with
      | None ->
        Reporter.fatalf
          ?loc
          Resolution_error
          "path %a could not be resolved"
          Trie.pp_path
          path
      | Some (cs, rest) ->
        let rest = match rest with "" -> [] | _ -> [Range.{ value = Syn.Text rest; loc }] in
        Range.{ value = Syn.TeX_cs cs; loc } :: rest
    end
  | None, _ ->
    Reporter.fatalf
      ?loc
      Resolution_error
      "path %a could not be resolved"
      Trie.pp_path
      path
  | Some (Term x, ()), _ ->
    let relocate Range.{ value; _ } = Range.{ value; loc } in
    List.map relocate x
  | Some (Xmlns { xmlns; prefix }, ()), _ ->
    Reporter.fatalf
      ?loc
      Resolution_error
      "path %a resolved to xmlns:%s=\"%s\" instead of term"
      Trie.pp_path
      path
      xmlns
      prefix

and expand_xml_ident loc (prefix, uname) =
  match prefix with
  | None -> { xmlns = None; prefix = ""; uname }
  | Some prefix ->
    match Sc.resolve ["xmlns"; prefix] with
    | Some (Xmlns { xmlns; prefix }, ()) ->
      { xmlns = Some xmlns; prefix = prefix; uname }
    | _ ->
      Reporter.fatalf
        ?loc
        Resolution_error
        "expected path `%s` to resolve to xmlns"
        prefix

and expand_tree_inner (tree : Code.tree) : Syn.tree =
  let trace f =
    match tree.addr with
    | Some addr -> Reporter.tracef "when expanding tree at address `%s`" addr f
    | None -> f ()
  in
  let@ () = trace in
  let@ () = Sc.section [] in
  let units = U.get () in
  let syn = expand tree.code in
  let exports = Sc.get_export () in
  let units =
    match tree.addr with
    | None -> units
    | Some addr -> Unit_map.add addr exports units
  in
  U.set units;
  syn

let expand_tree (units : exports Unit_map.t) (tree : Code.tree) =
  let@ () = U.run ~init: units in
  let@ () = Sc.easy_run in
  Builtins.register_builtins
    [
      ["p"], Syn.Prim `P;
      ["em"], Syn.Prim `Em;
      ["strong"], Syn.Prim `Strong;
      ["li"], Syn.Prim `Li;
      ["ol"], Syn.Prim `Ol;
      ["ul"], Syn.Prim `Ul;
      ["code"], Syn.Prim `Code;
      ["blockquote"], Syn.Prim `Blockquote;
      ["pre"], Syn.Prim `Pre;
      ["figure"], Syn.Prim `Figure;
      ["figcaption"], Syn.Prim `Figcaption;
      ["transclude"], Syn.Transclude;
      ["tex"], Syn.Embed_tex;
      ["ref"], Syn.Ref;
      ["title"], Syn.Title;
      ["taxon"], Syn.Taxon;
      ["date"], Syn.Date;
      ["meta"], Syn.Meta;
      ["author"], Syn.Attribution (Author, `Iri);
      ["author"; "literal"], Syn.Attribution (Author, `Content);
      ["contributor"], Syn.Attribution (Contributor, `Iri);
      ["contributor"; "literal"], Syn.Attribution (Contributor, `Content);
      ["parent"], Syn.Parent;
      ["number"], Syn.Number;
      ["tag"], Syn.Tag `Content;
      ["query"], Syn.Results_of_query;
      ["query"; "rel"], Syn.Query_rel `Iri;
      ["query"; "rel"; "literal"], Syn.Query_rel `Content;
      ["query"; "union"], Syn.Query_union;
      ["query"; "isect"], Syn.Query_isect;
      ["query"; "isect-fam"], Syn.Query_isect_fam;
      ["query"; "union-fam"], Syn.Query_union_fam;
      ["query"; "isect-fam-rel"], Syn.Query_isect_fam_rel;
      ["query"; "union-fam-rel"], Syn.Query_union_fam_rel;
      ["query"; "compl"], Syn.Query_compl;
      ["query"; "tag"], Syn.Query_builtin (`Tag, `Content);
      ["query"; "taxon"], Syn.Query_builtin (`Taxon, `Content);
      ["query"; "author"], Syn.Query_builtin (`Author, `Iri);
      ["query"; "author"; "literal"], Syn.Query_builtin (`Author, `Content);
      ["query"; "incoming"], Syn.Query_polarity Incoming;
      ["query"; "outgoing"], Syn.Query_polarity Outgoing;
      ["query"; "edges"], Syn.Query_mode Edges;
      ["query"; "paths"], Syn.Query_mode Paths;
      ["rel"; "tags"], Syn.Text Query.Rel.tags;
      ["rel"; "taxa"], Syn.Text Query.Rel.taxa;
      ["rel"; "authors"], Syn.Text Query.Rel.authors;
      ["rel"; "contributors"], Syn.Text Query.Rel.authors;
      ["rel"; "transclusion"], Syn.Text Query.Rel.transclusion;
      ["rel"; "links"], Syn.Text Query.Rel.links
    ];
  Builtins.Transclude.alloc_expanded ();
  Builtins.Transclude.alloc_show_heading ();
  Builtins.Transclude.alloc_toc ();
  Builtins.Transclude.alloc_numbered ();
  Builtins.Transclude.alloc_show_metadata ();
  let tree = expand_tree_inner tree in
  let units = U.get () in
  units, tree
