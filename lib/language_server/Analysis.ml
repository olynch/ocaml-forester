(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

(* This module will probably end up containing a complete replacement of the batch compiler pipeline*)

open Forester_prelude
open Forester_core
open Forester_compiler

module L = Lsp.Types
module T = Types
module EP = Eio.Path
module G = Forester_forest.Forest_graphs.Make ()
module F = Forester_forest.Forest.Make(G)
module FU = Forester_forest.Forest_util.Make(F)
module PT = Forester_forest.Plain_text_client.Make(F)(struct let route _ = "todo" end)

type diagnostic = Reporter.Message.t Asai.Diagnostic.t

let parse_path path =
  let content = EP.load path in
  let path = EP.native_exn path |> Unix.realpath in
  Parse.parse ~source: (`File path) (Lexing.from_string content)

let parse_from = function
  | `String s -> Parse.parse_string ?source: None s
  | `Eio_path p -> parse_path p
  | `Text_document d ->
    let source = Lsp.Text_document.documentUri d |> Lsp.Uri.to_string in
    Parse.parse_string ~source: (`File source) (Lsp.Text_document.text d)
  | `Uri uri ->
    begin
      let server = State.get () in
      match Hashtbl.find_opt server.index.documents uri with
      | Some doc -> Parse.parse_string ~source: (`File (Lsp.Uri.to_path uri.uri)) (Lsp.Text_document.text doc)
      | None ->
        Error (Asai.Diagnostic.of_text Error Reporter.Message.Parse_error (Asai.Diagnostic.text "parse error"))
    end
  | `Iri (env, iri) ->
    match F.get_article iri with
    | Some { frontmatter = { source_path = Some str; _ }; _ } ->
      let p = EP.(env#fs / str) in
      parse_path p
    | _ ->
      Result.error @@ Reporter.diagnosticf (Tree_not_found iri) "could not find tree %a." pp_iri iri

module Dependencies = struct
  module Env = Algaeff.Reader.Make(struct type t = string end)

  let rec analyse_tree roots import_graph (v_opt : State.Graph.vertex option) code =
    let roots =
      Option.fold
        ~none: roots
        ~some: (fun x -> x :: roots)
        v_opt
    in
    analyse_code roots import_graph code;
    let@ uri = Option.iter @~ v_opt in
    State.Graph.add_vertex import_graph uri

  and analyse_code roots import_graph (code : Code.t) =
    List.iter (analyse_node roots import_graph) code

  and analyse_node roots import_graph (node : Code.node Asai.Range.located) =
    (* The key difference to Import_graph is that is we have not yet parsed all
       files when this is run, so when we encounter a dependency we parse from
       filesystem and analyse the result.*)
    let server = State.get () in
    let host = server.config.host in
    match node.value with
    | Import (_, dep) ->
      begin
        let dep_iri = Iri_scheme.user_iri ~host dep in
        begin
          match Hashtbl.find_opt server.index.resolver dep_iri with
          | Some dep_uri ->
            begin
              match parse_from (`Uri dep_uri) with
              | Ok code ->
                analyse_code [dep_uri.uri] import_graph code
              | Error _ ->
                (* TODO: accumulate diagnostic and report *)
                Eio.traceln "failed to parse %s. " (Lsp.Uri.to_string dep_uri.uri)
            end;
            let@ addr = List.iter @~ roots in
            State.Graph.add_edge
              import_graph
              dep_uri.uri
              addr;
          | None -> Eio.traceln "could not resolve %a" pp_iri dep_iri
        end
      end
    | Subtree (addr, code) ->
      let iri = Option.map (Iri_scheme.user_iri ~host) addr in
      let uri = Option.bind iri (Hashtbl.find_opt server.index.resolver) in
      begin
        match uri with
        | None -> ()
        | Some uri ->
          analyse_tree
            roots
            import_graph
            (Some uri.uri)
            code
      end
    | Scope code | Namespace (_, code) | Group (_, code) | Math (_, code) | Let (_, _, code) | Fun (_, code) | Def (_, _, code) ->
      analyse_code roots import_graph code
    | Object { methods; _ } | Patch { methods; _ } ->
      let@ _, code = List.iter @~ methods in
      analyse_code roots import_graph code
    | Dx_prop (rel, args) ->
      analyse_code roots import_graph rel;
      List.iter (analyse_code roots import_graph) args
    | Dx_sequent (concl, premises) ->
      analyse_code roots import_graph concl;
      List.iter (analyse_code roots import_graph) premises
    | Dx_query (_, positives, negatives) ->
      List.iter (analyse_code roots import_graph) positives;
      List.iter (analyse_code roots import_graph) negatives
    | Text _ | Hash_ident _ | Xml_ident _ | Verbatim _ | Ident _ | Open _ | Put _ | Default _ | Get _ | Decl_xmlns _ | Call _ | Alloc _ | Dx_var _ | Dx_const_content _ | Dx_const_iri _ | Comment _ | Error _ ->
      ()
end

let update_graph (uri : Lsp.Uri.t) code : unit =
  let server = State.get () in
  Dependencies.analyse_tree [] server.import_graph (Some uri) code

let syntax_diagnostics source =
  let ok = fun _ -> [] in
  let error = (fun diag -> [diag]) in
  Result.fold ~ok ~error (parse_from source)

(*  TODO: version of this function that only expands one tree *)
let expand () =
  let server = State.get () in
  let import_closure = State.Graph.Op.transitive_reduction server.import_graph in
  let diagnostics : (Lsp.Uri.t, diagnostic) Hashtbl.t = Hashtbl.create 100 in
  let task (uri : Lsp.Uri.t) (units, trees) =
    match Hashtbl.find_opt server.index.codes { uri } with
    | None ->
      units, trees
    | Some tree ->
      try
        let push_diagnostic (d : diagnostic) =
          Hashtbl.add diagnostics uri d
        in
        let emit = push_diagnostic in
        let fatal diag = push_diagnostic diag; units, trees in
        Reporter.run ~emit ~fatal @@
          fun () ->
            let units, syn = Forester_compiler.Expand.expand_tree units tree in
            let addr = Util.uri_to_addr (Lsp.Uri.to_path uri) in
            units, (addr, tree.source_path, syn) :: trees
      with
        | _ ->
          units, trees
  in
  let env, expanded_trees =
    State.Graph.topo_fold
      task
      import_closure
      (Forester_compiler.Expand.Env.empty, [])
  in
  diagnostics, env, expanded_trees

let eval (addr, source_path, syn) =
  let server = State.get () in
  let host = server.config.host in
  let iri = Iri_scheme.user_iri ~host addr in
  let uri = Hashtbl.find_opt server.index.resolver iri in
  let diagnostics : (Lsp.Uri.t, diagnostic list) Hashtbl.t = Hashtbl.create 100 in
  let push_diagnostic (d : diagnostic) =
    Option.fold
      ~none: ()
      ~some: (
        fun (uri : L.TextDocumentIdentifier.t) ->
          match Hashtbl.find_opt diagnostics uri.uri with
          | None -> ()
          | Some diags -> Hashtbl.replace diagnostics uri.uri (d :: diags)
      )
      uri
  in
  let emit = push_diagnostic in
  let fatal diag =
    push_diagnostic diag;
    Forester_compiler.Eval.{
      main =
      {
        frontmatter = T.default_frontmatter ();
        mainmatter = T.Content [];
        backmatter = T.Content []
      };
      side = [];
      jobs = []
    }
  in
  let evaluated =
    Reporter.run ~emit ~fatal @@
      fun () ->
        Forester_compiler.Eval.eval_tree ~host ~iri ~source_path syn
  in
  diagnostics, evaluated

(* NOTE: This should always get run after `check_syntax`, so the code should be
   part of the index.*)
let check_semantics uri =
  let server = State.get () in
  (* TODO: As it stands, any error during evaluation will get reported to the
     uri currently being checked. This is the wrong behavior.*)
  match Hashtbl.find_opt server.index.codes { uri } with
  | None -> ()
  | Some tree ->
    update_graph uri tree.code;
    let _diagnostics, _env, expanded_trees = expand () in
    List.iter
      (
        fun (addr, source_path, syn) ->
          let diagnostics, evaluated = eval (addr, source_path, syn) in
          F.plant_resource @@
            T.Article evaluated.main;
          Hashtbl.iter (fun uri diagnostics -> Publish.to_uri uri diagnostics) diagnostics
      )
      expanded_trees

let extract_addr (node : Code.node Range.located) : string option =
  match node.value with
  | Group (Braces, [{ value = Text addr; _ }])
  | Group (Parens, [{ value = Text addr; _ }])
  | Group (Squares, [{ value = Group (Squares, [{ value = Text addr; _ }]); _ }])
  | Text addr
  | Import (_, addr) ->
    Some addr
  | Verbatim _ | Math (_, _) | Ident _ | Hash_ident _ | Xml_ident _ | Subtree (_, _) | Let (_, _, _) | Open _ | Scope _ | Put (_, _) | Default (_, _) | Get _ | Fun (_, _) | Object _ | Patch _ | Call (_, _) | Def (_, _, _) | Decl_xmlns (_, _) | Alloc _ | Namespace (_, _) | _ -> None

let rec flatten (tree : Code.t) : Code.t =
  tree
  |> List.concat_map @@
    fun (node : 'a Range.located) ->
      match node.value with
      | Code.Subtree (_, tree) -> flatten tree
      | Code.Scope tree -> flatten tree
      | _ -> [node]

let contains = fun
      ~(position : Lsp.Types.Position.t)
      (located : _ Range.located)
    ->
    let L.Position.{ line = cursor_line; character = cursor_character } = position in
    match located.loc with
    | Some loc ->
      begin
        match Range.view loc with
        | `Range (start, end_) ->
          let start_pos = Lsp_shims.Loc.lsp_pos_of_pos start in
          let end_pos = Lsp_shims.Loc.lsp_pos_of_pos end_ in
          let at_or_after_start =
            cursor_line < end_pos.line
            || (cursor_line = start_pos.line && start_pos.character <= cursor_character)
          in
          let before_or_at_end =
            end_pos.line > cursor_line
            || (cursor_line = end_pos.line && cursor_character <= end_pos.character)
          in
          at_or_after_start && before_or_at_end
        | _ -> false
      end
    | None -> false

let nodes_within (node : Code.node Range.located) =
  match node.value with
  | Code.Math (_, t)
  | Code.Group (_, t)
  | Code.Let (_, _, t)
  | Code.Scope t
  | Code.Put (_, t)
  | Code.Fun (_, t)
  | Code.Default (_, t)
  | Code.Def (_, _, t)
  | Code.Namespace (_, t)
  | Code.Dx_const_iri t
  | Code.Dx_const_content t
  | Code.Call (t, _)
  | Code.Subtree (_, t) ->
    Some t
  | Code.Dx_prop (_, t)
  | Code.Dx_query (_, _, t)
  | Code.Dx_sequent (_, t) ->
    Some (List.concat t)
  | Code.Object { methods; _ } ->
    Some (methods |> List.map snd |> List.concat)
  | Code.Patch { obj; methods; _ } ->
    let methods = (methods |> List.map snd |> List.concat) in
    Some (List.append obj methods)
  | Code.Text _
  | Code.Verbatim _
  | Code.Ident _
  | Code.Hash_ident _
  | Code.Xml_ident (_, _)
  | Code.Open _
  | Code.Get _
  | Code.Import (_, _)
  | Code.Decl_xmlns (_, _)
  | Code.Alloc _
  | Code.Dx_var _
  | Code.Comment _
  | Code.Error _ ->
    None

let rec node_at ~(position : Lsp.Types.Position.t) (code : _ list) : Code.node Range.located option =
  let flattened = flatten code in
  match List.find_opt (contains ~position) flattened with
  | None -> None
  | Some n ->
    match Option.bind (nodes_within n) (node_at ~position) with
    | Some inner -> Some inner
    | None -> Some n

let addr_at ~(position : Lsp.Types.Position.t) (code : _ list) : string option =
  Option.bind (node_at ~position code) extract_addr
