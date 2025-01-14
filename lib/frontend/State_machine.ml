(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T = Types
module Cmp = Phases

type state = State.t

type 'a action =
  | Quit
  | Build_import_graph
  | Build_dependency_graph of iri
  | Do_nothing
  | Load_all
  | Parse_all
  | Expand_all
  | Eval_all
  | Expand_only of iri
  | Eval_only of iri
  | Parse of iri
  | Get of (('a Render.target [@opaque ]) * iri)
  | Query of (string, Vertex.t) Datalog_expr.query
  | Cache_results of (Vertex_set.t [@opaque ])
[@@deriving show]

type ('r, 'e) result =
  | Nothing
  | Vertex_set of Vertex_set.t
  | Render_result of 'r
  | Error of 'e

let log s =
  Logs.app (fun m -> m " ￮ %s...@." s)

let update
    : type a. a action -> state -> (a action * state * (a, _) result)
  = fun msg state ->
    match msg with
    | Query q ->
      let r = Forest.run_datalog_query state.graphs q in
      (Cache_results r, state, Vertex_set r)
    | Get (target, iri) ->
      begin
        match Forest.get_article iri state.resources with
        | Some article ->
          let result =
            try
              Render_result
                (
                  Render.render
                    ~dev: true
                    state
                    target
                    (Article article)
                )
            with
              | _ -> Error (`Failed_to_render iri)
          in
          (Do_nothing, state, result)
        | None -> (Do_nothing, state, Error (`Not_found iri))
      end
    | Quit -> exit 0
    | Load_all ->
      (Parse_all, Cmp.load_configured_dirs state, Nothing)
    | Parse_all ->
      log "Parse trees";
      (
        Build_import_graph,
        Cmp.parse ~quit_on_error: false state,
        Nothing
      )
    | Build_import_graph ->
      (
        Expand_all,
        Cmp.build_import_graph state,
        Nothing
      )
    | Build_dependency_graph iri ->
      (
        Expand_only iri,
        Cmp.build_import_graph_for ~addr: iri state,
        Nothing
      )
    | Expand_all ->
      log "Expand, evaluate and analyse forest";
      (
        Eval_all,
        Cmp.expand ~quit_on_error: false state,
        Nothing
      )
    | Expand_only iri ->
      (
        Eval_only iri,
        Cmp.expand_only iri state,
        Nothing
      )
    | Eval_all ->
      (
        Do_nothing,
        Cmp.eval ~dev: true state,
        Nothing
      )
    | Eval_only iri ->
      (
        Do_nothing,
        Cmp.eval_only iri state,
        Nothing
      )
    | Parse _
    | Cache_results _ ->
      (Do_nothing, state, Nothing)
    | Do_nothing ->
      (Do_nothing, state, Nothing)

let run_action action ~(until : 'a action) state : state * ('r, 'e) result =
  let rec go action state =
    match update action state with
    | (new_action, new_state, result) ->
      if until = action then (new_state, result)
      else
        go new_action new_state
  in
  go action state

let rec force
    : 'a action list -> state -> ('r, 'e) result
  = fun msgs state ->
    match msgs with
    | [] -> Nothing
    | msg :: remaining ->
      let _discard, new_state, _ = update msg state in
      force remaining new_state

let batch_run ~env ~config ~dev =
  Phases.init ~env ~config ~dev
  |> run_action Load_all ~until: Do_nothing
  |> fst

let language_server
    : state -> unit
  = fun _ -> ()

let render_tree ~env ~config ~dev target iri =
  let (forest, _) =
    Cmp.init ~env ~config ~dev
    |> run_action (Build_dependency_graph iri) ~until: Do_nothing
  in
  match Forest.get_article iri forest.resources with
  | None -> ""
  | Some article ->
    Format.asprintf "%a" Render.(pp ~dev forest target) (Article article)
