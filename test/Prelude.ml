(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

let rec strip_loc : Code.t -> Code.t = fun nodes ->
  List.map
    (fun Range.{value; _} -> Range.{value = strip_loc_node value; loc = None})
    nodes

and strip_loc_node : Code.node -> Code.node = fun node ->
  let open Code in
  match node with
  | Group (d, t) -> Group (d, strip_loc t)
  | Math (m, t) -> Math (m, strip_loc t)
  | Namespace (p, t) -> Namespace (p, strip_loc t)
  | Dx_const_content t -> Dx_const_content (strip_loc t)
  | Dx_const_iri t -> Dx_const_iri (strip_loc t)
  | Def (p, b, t) -> Def (p, b, strip_loc t)
  | Dx_sequent (t, ts) -> Dx_sequent (strip_loc t, List.map strip_loc ts)
  | Dx_query (s, ts, rs) -> Dx_query (s, List.map strip_loc ts, List.map strip_loc rs)
  | Dx_prop (t, ts) -> Dx_prop (t, ts)
  | Subtree (s, t) -> Subtree (s, strip_loc t)
  | Let (p, b, t) -> Let (p, b, strip_loc t)
  | Default (p, t) -> Default (p, strip_loc t)
  | Scope t -> Scope (strip_loc t)
  | Put (p, t) -> Put (p, strip_loc t)
  | Fun (b, t) -> Fun (b, strip_loc t)
  | Call (t, s) -> Call (strip_loc t, s)
  | Text _
  | Verbatim _
  | Ident _
  | Hash_ident _
  | Xml_ident (_, _)
  | Open _
  | Get _
  (* TODO: strip object *)
  | Object _
  | Patch _
  | Import (_, _)
  | Decl_xmlns (_, _)
  | Alloc _
  | Dx_var _
  | Comment _
  | Error _ ->
    node

let parse_string str =
  let lexbuf = Lexing.from_string str in
  let res = Parse.parse ~source: (`String {title = None; content = str}) lexbuf in
  Result.map strip_loc res
