(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

module T = Types

type state = State.t

type 'a action =
  | Quit
  | Build_import_graph
  | Build_dependency_graph of iri
  | Plant_assets of (Eio.Fs.dir_ty Eio.Path.t list [@opaque ])
  | Plant_foreign of (Eio.Fs.dir_ty Eio.Path.t list [@opaque ])
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
        Do_nothing,
        Phases.eval ~dev: true state,
        Nothing
      )
    | Eval_only iri ->
      (
        Do_nothing,
        Phases.eval_only iri state,
        Nothing
      )
    | Plant_assets paths ->
      (
        Do_nothing,
        Phases.plant_assets paths state,
        Nothing
      )
    | Plant_foreign path ->
      (
        Do_nothing,
        Phases.implant_foreign path state,
        Nothing
      )
    | Parse _
    | Cache_results _
    | Do_nothing ->
      (
        Do_nothing,
        state,
        Nothing
      )

let run_action action ~(until : 'a action) state : state =
  let rec go action state =
    match update action state with
    | (new_action, new_state, result) ->
      if until = action then (new_state, result)
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

let implant_foreign paths state =
  state
  |> run_action
    (Plant_foreign paths)
    ~until: Do_nothing

let plant_assets paths state =
  state
  |> run_action
    (Plant_assets paths)
    ~until: Do_nothing

let batch_run ~env ~(config : Config.t) ~dev =
  let asset_paths = Eio_util.paths_of_dirs ~env config.assets in
  let foreign_paths = Eio_util.paths_of_dirs ~env config.foreign in
  Phases.init ~env ~config ~dev
  |> plant_assets asset_paths
  |> implant_foreign foreign_paths
  |> run_action Load_all ~until: Do_nothing

let language_server
    : state -> unit
  = fun _ -> ()

let render_tree ~env ~config ~dev target iri =
  let forest =
    Phases.init ~env ~config ~dev
    |> run_action (Build_dependency_graph iri) ~until: Do_nothing
  in
  match Forest.get_article iri forest.resources with
  | None -> assert false
  | Some article ->
    Format.asprintf "%a" Render.(pp ~dev forest target) (Article article)
