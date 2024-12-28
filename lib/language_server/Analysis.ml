(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_core
open Forester_compiler

module L = Lsp.Types

let extract_addr (node : Code.node Range.located) : string option =
  match node.value with
  | Group (Braces, [{ value = Text addr; _ }])
  | Group (Parens, [{ value = Text addr; _ }])
  | Group (Squares, [{ value = Group (Squares, [{ value = Text addr; _ }]); _ }])
  | Text addr
  | Import (_, addr) ->
    Some addr
  | Subtree (addr, _) -> addr
  | Verbatim _ | Math (_, _) | Ident _ | Hash_ident _ | Xml_ident _ | Let (_, _, _) | Open _ | Scope _ | Put (_, _) | Default (_, _) | Get _ | Fun (_, _) | Object _ | Patch _ | Call (_, _) | Def (_, _, _) | Decl_xmlns (_, _) | Alloc _ | Namespace (_, _) | _ -> None

(* TODO: Think about this some more. *)
let rec flatten (tree : Code.t) : Code.t =
  tree
  |> List.concat_map @@
    fun (node : 'a Range.located) ->
      match node.value with
      | Code.Subtree (_, nodes)
      | Code.Scope nodes ->
        flatten nodes
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
