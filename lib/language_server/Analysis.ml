(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_frontend

module L = Lsp.Types
module T = Types
module EP = Eio.Path
module G = Forester_forest.Forest_graphs.Make ()
module F = Forester_forest.Forest.Make(G)
module FU = Forester_forest.Forest_util.Make(F)

module Code_set = Set.Make(struct
  type t = Code.tree
  (* TODO: no polymorphic compare*)
  let compare = compare
end)

let path_of_dir ~env dir =
  EP.(Eio.Stdenv.fs env / dir)

let paths_of_dirs ~env =
  List.map (path_of_dir ~env)

let build_once (state : Base.server) () =
  let tree_dirs = (paths_of_dirs ~env: state.env state.config.trees) in
  Eio.traceln "Planting forest";
  let parsed_trees =
    Forester_frontend.Forester.parse_trees_in_dirs
      ~dev: true
      tree_dirs
  in
  parsed_trees
  |> List.iter
    (
      fun
          (Code.{ source_path; addr; _ } as code)
        ->
        match source_path with
        | Some p ->
          let uri = Lsp.Uri.of_path p in
          Hashtbl.add state.codes L.TextDocumentIdentifier.{ uri } code;
          begin
            match addr with
            | Some a ->
              Hashtbl.add
                state.resolver
                (Iri_scheme.user_iri ~host: state.config.host a)
                L.TextDocumentIdentifier.{ uri }
            | None -> ()
          end
        | None ->
          ()
    );
  try
    let articles, _ =
      Forester_frontend.Forest_reader.read_trees
        ~env: state.env
        ~host: state.config.host
        parsed_trees
    in
    let@ article = List.iter @~ articles in
    F.plant_resource @@ T.Article article
  with
    | _ -> ()

let parse_path path = Parse.parse_string @@ EP.load path

let parse_from = function
  | `String s -> Parse.parse_string s
  | `Eio_path p -> parse_path p
  | `Uri(uri, cache: L.TextDocumentIdentifier.t * _) ->
    begin
      match Hashtbl.find_opt cache uri with
      | Some doc -> Parse.parse_string (Lsp.Text_document.text doc)
      | None ->
        Error (Reporter.fatalf Internal_error "Could not find %s in the internal document store. This is a bug!" (Lsp.Uri.to_path uri.uri))
    end
  | `Iri (env, iri) ->
    match F.get_article iri with
    | Some { frontmatter = { source_path = Some str; _ }; _ } ->
      let p = EP.(env#fs / str) in
      parse_path p
    | _ ->
      Result.error @@
        Reporter.diagnosticf
          (Tree_not_found iri)
          "could not find tree %a "
          pp_iri
          iri

let dependencies (code : Code.t) host : iri Range.located list =
  let rec analyse_deps (node : Code.node Range.located) =
    match Range.(node.value) with
    | Import (_, dep) ->
      [Range.{ loc = node.loc; value = (Iri_scheme.user_iri ~host dep) }]
    | Subtree (_, code)
    | Scope code
    | Namespace (_, code)
    | Group (_, code)
    | Math (_, code)
    | Let (_, _, code)
    | Fun (_, code)
    | Def (_, _, code) ->
      List.concat_map analyse_deps code
    | Object { methods; _ } | Patch { methods; _ } ->
      let@ code = List.concat_map @~ methods in
      List.concat_map analyse_deps (snd code)
    | _ ->
      []
  in
  List.concat_map
    analyse_deps
    code

(* Does no IO*)
let get_dependencies (server : Base.server) code =
  let rec go c acc =
    let immediate_deps = dependencies c server.config.host in
    List.fold_left
      (
        fun acc' d ->
          match Hashtbl.find_opt server.resolver Range.(d.value) with
          | None ->
            Reporter.emitf ?loc: d.loc Resource_not_found "Could not find tree %a" pp_iri d.value;
            acc'
          | Some uri ->
            begin
              match Hashtbl.find_opt server.codes uri with
              | None ->
                Reporter.emitf ?loc: d.loc Resource_not_found "Could not find tree %s" @@ Lsp.Uri.to_path uri.uri;
                acc'
              | Some tree -> go tree.code (Code_set.add tree acc')
            end
      )
      acc
      immediate_deps
  in
  go code Code_set.empty

let check (server : Base.server) uri =
  let res = parse_from (`Uri (L.TextDocumentIdentifier.{ uri = uri }, server.documents)) in
  match res with
  | Ok code ->
    let str_path = (Lsp.Uri.to_path uri) in
    let addr =
      String.split_on_char '/' str_path |> List.rev
      |> List.hd
      |> Filename.chop_extension
      |> Option.some
    in
    let tree = Code.{ source_path = Some str_path; addr; code } in
    Hashtbl.replace server.codes { uri } tree;
    let trans_deps = get_dependencies server code in
    let trees = trans_deps |> Code_set.to_list in
    let _units, _expanded_trees =
      Forest_reader.expand
        ~host: server.config.host
        (tree :: trees)
    in
    ()
  | Error diagnostic ->
    Reporter.emit_diagnostic diagnostic

let extract_addr (node : Code.node Range.located) : string option =
  match node.value with
  | Group (Braces, [{ value = Text addr; _ }])
  | Group (Parens, [{ value = Text addr; _ }])
  | Group (Squares, [{ value = Group (Squares, [{ value = Text addr; _ }]); _ }])
  | Text addr
  | Import (_, addr) ->
    Some addr
  | Verbatim _ | Math (_, _) | Ident _ | Hash_ident _ | Xml_ident _ | Subtree (_, _) | Let (_, _, _) | Open _ | Scope _ | Put (_, _) | Default (_, _) | Get _ | Fun (_, _) | Object _ | Patch _ | Call (_, _) | Def (_, _, _) | Decl_xmlns (_, _) | Alloc _ | Namespace (_, _) | _ -> None

let _ =
  assert (
    extract_addr @@
      Range.{
        loc = None;
        value = Group (Parens, [{ value = Text "foo"; loc = None }])
      }
    = Some "foo"
  )

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
          let start_pos = LspShims.Loc.lsp_pos_of_pos start in
          let end_pos = LspShims.Loc.lsp_pos_of_pos end_ in
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
