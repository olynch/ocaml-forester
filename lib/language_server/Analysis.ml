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

let build_once ~env (state : Base.server) () =
  let tree_dirs = (paths_of_dirs ~env state.config.trees) in
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
        ~env
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
    | Some{ frontmatter = { source_path = Some str; _ }; _ } ->
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
    | Object{ methods; _ } | Patch{ methods; _ } ->
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
    let tree = Code.{ source_path = None; addr = None; code } in
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
  | Import (_, addr) ->
    Some addr
  | Text _ | Verbatim _ | Math (_, _) | Ident _ | Hash_ident _ | Xml_ident _ | Subtree (_, _) | Let (_, _, _) | Open _ | Scope _ | Put (_, _) | Default (_, _) | Get _ | Fun (_, _) | Object _ | Patch _ | Call (_, _) | Def (_, _, _) | Decl_xmlns (_, _) | Alloc _ | Namespace (_, _) | _ -> None

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

let within ~range: (a, b) x = a <= x && x <= b

let is_at = fun
      ~(position : Lsp.Types.Position.t)
      (located : _ Range.located)
    ->
    match located.loc with
    | Some loc ->
      begin
        match Range.view loc with
        | `Range (start, end_) ->
          within ~range: (start.line_num, end_.line_num) (position.line + 1)
          && within
            ~range: ((start.offset - start.start_of_line), (end_.offset - end_.start_of_line - 1))
            position.character
        | _ -> false
      end
    | None -> false

let node_at ~(position : Lsp.Types.Position.t) (code : _) : _ option =
  let flattened = flatten code in
  List.find_opt (is_at ~position) flattened

let addr_at ~(position : Lsp.Types.Position.t) (code : _) : _ option =
  Option.bind (node_at ~position code) extract_addr
