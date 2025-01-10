(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_compiler

module T = Types

type analysis_env = {
  graph: Forest_graph.t;
  follow: bool;
  forest: State.t;
}

module Analysis_env = Algaeff.Reader.Make(struct type t = analysis_env end)

let rec analyse_tree roots (tree : Forester_compiler.Code.tree) =
  let env = Analysis_env.read () in
  let host = env.forest.config.host in
  let addr_opt = tree.addr in
  let code = tree.code in
  let roots =
    Option.fold
      ~none: roots
      ~some: (
        fun x -> x :: roots
      )
      addr_opt
  in
  analyse_code roots code;
  let@ addr = Option.iter @~ addr_opt in
  let iri = Iri_scheme.user_iri ~host addr in
  Forest_graph.add_vertex env.graph (T.Iri_vertex iri)

and analyse_code roots (code : Code.t) =
  List.iter (analyse_node roots) code

and analyse_node roots (node : Forester_compiler.Code.node Asai.Range.located) =
  let env = Analysis_env.read () in
  let host = env.forest.config.host in
  match node.value with
  | Import (_, dep) ->
    let dep_iri = Iri_scheme.user_iri ~host dep in
    let dependency = T.Iri_vertex dep_iri in
    let@ addr = List.iter @~ roots in
    let target = T.Iri_vertex (Iri_scheme.user_iri ~host addr) in
    Forest_graph.add_edge env.graph dependency target;
    begin
      if env.follow then
        match Iri_resolver.(resolve (Iri dep_iri) To_code env.forest) with
        | None -> assert false
        | Some tree -> analyse_tree [] tree
      else ()
    end
  | Subtree (addr, code) ->
    analyse_tree
      roots
      (* Consider using the env to keep track of the current source path *)
      { addr; code; source_path = None }
  | Scope code | Namespace (_, code) | Group (_, code) | Math (_, code) | Let (_, _, code) | Fun (_, code) | Def (_, _, code) ->
    analyse_code roots code
  | Object { methods; _ } | Patch { methods; _ } ->
    let@ _, code = List.iter @~ methods in
    analyse_code roots code
  | Dx_prop (rel, args) ->
    analyse_code roots rel;
    List.iter (analyse_code roots) args
  | Dx_sequent (concl, premises) ->
    analyse_code roots concl;
    List.iter (analyse_code roots) premises
  | Dx_query (_, positives, negatives) ->
    List.iter (analyse_code roots) positives;
    List.iter (analyse_code roots) negatives
  | Text _ | Hash_ident _ | Xml_ident (_, _) | Verbatim _ | Ident _ | Open _ | Put (_, _) | Default (_, _) | Get _ | Decl_xmlns (_, _) | Call (_, _) | Alloc _ | Dx_var _ | Dx_const_content _ | Dx_const_iri _ | Comment _ | Error _ -> ()

let dependencies tree forest =
  let graph = Forest_graph.create () in
  let env = { graph; forest; follow = true } in
  let@ () = Analysis_env.run ~env in
  analyse_tree [] tree;
  env.graph

let _minimal_dependency_graph
    : addr: iri -> Forest_graph.t
  = fun ~addr ->
    let dep_graph = Forest_graph.create () in
    let rec f v =
      Forest_graph.iter_succ
        (fun w -> Forest_graph.add_edge dep_graph v w; f w)
        dep_graph
        v
    in
    f (T.Iri_vertex addr);
    dep_graph

let run_builder ?root env =
  let@ () = Analysis_env.run ~env in
  begin
    match root with
    | Some iri ->
      begin
        match Iri_resolver.(resolve (Iri iri) To_code env.forest) with
        | None -> ()
        | Some tree -> analyse_tree [] tree
      end
    | None ->
      env.forest.parsed
      |> Forest.to_seq_values
      |> Seq.iter (analyse_tree [])
  end;
  env.graph

let build forest =
  let graph = Forest_graph.create () in
  let env = { graph; forest; follow = false } in
  run_builder env
