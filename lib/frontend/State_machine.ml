(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T = Types

type state = State.t

type 'a action =
  | Quit
  | Build_import_graph
  | Build_dependency_graph of iri
  | Plant_assets
  | Plant_foreign
  | Done
  | Load_all_configured_dirs
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

let update
    : type a. a action -> state -> (a action * state * (a, _) result)
  = fun msg state ->
    Logs.debug
      (
        fun m ->
          m
            "Running action %a"
            (pp_action (fun fmt _ -> Format.pp_print_nothing fmt ()))
            msg
      );
    match msg with
    | Query q ->
      let r = Forest.run_datalog_query state.graphs q in
      (Cache_results r, state, Vertex_set r)
    | Get (target, iri) ->
      begin
        match Forest.get_article iri state.resources with
        | Some article ->
          let result =
            Render_result
              (
                Render.render
                  ~dev: true
                  state
                  target
                  (Article article)
              )
          in
          (Done, state, result)
        | None -> (Done, state, Error (`Not_found iri))
      end
    | Quit -> exit 0
    | Load_all_configured_dirs ->
      (
        Parse_all,
        Phases.load_configured_dirs state,
        Nothing
      )
    | Parse_all ->
      Reporter.log Format.pp_print_string "Parse trees";
      (
        Build_import_graph,
        Phases.parse ~quit_on_error: false state,
        Nothing
      )
    | Build_import_graph ->
      (
        Expand_all,
        Phases.build_import_graph state,
        Nothing
      )
    | Build_dependency_graph iri ->
      (
        Expand_only iri,
        Phases.build_import_graph_for ~iri state,
        Nothing
      )
    | Expand_all ->
      Reporter.log Format.pp_print_string "Expand, evaluate and analyse forest";
      (
        Eval_all,
        Phases.expand ~quit_on_error: false state,
        Nothing
      )
    | Expand_only iri ->
      (
        Eval_only iri,
        Phases.expand_only iri state,
        Nothing
      )
    | Eval_all ->
      (
        Done,
        Phases.eval state,
        Nothing
      )
    | Eval_only iri ->
      (
        Done,
        Phases.eval_only iri state,
        Nothing
      )
    | Plant_assets ->
      (
        Done,
        Phases.plant_assets state,
        Nothing
      )
    | Plant_foreign ->
      (
        Done,
        Phases.implant_foreign state,
        Nothing
      )
    | Parse _
    | Cache_results _
    | Done ->
      (
        Done,
        state,
        Nothing
      )

let run_action action state : state =
  let rec go action state =
    match update action state with
    | (new_action, new_state, result) ->
      if action = Done then (new_state, result)
      else
        go new_action new_state
  in
  go action state
  |> fst

let rec force
    : 'a action list -> state -> ('r, 'e) result
  = fun msgs state ->
    match msgs with
    | [] -> Nothing
    | msg :: remaining ->
      let _discard, new_state, _ = update msg state in
      force remaining new_state

let implant_foreign = run_action Plant_foreign

let plant_assets = run_action Plant_assets

let batch_run ~env ~(config : Config.t) ~dev =
  Phases.init ~env ~config ~dev
  |> plant_assets
  |> implant_foreign
  |> run_action Load_all_configured_dirs

let language_server
    : state -> unit
  = fun _ -> ()

let render_tree ~env ~config ~dev target iri =
  let forest =
    Phases.init ~env ~config ~dev
    |> run_action (Build_dependency_graph iri)
  in
  match Forest.get_article iri forest.resources with
  | None -> assert false
  | Some article ->
    Format.asprintf "%a" Render.(pp ~dev forest target) (Article article)
