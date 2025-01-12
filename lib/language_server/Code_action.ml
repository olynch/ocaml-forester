(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Lsp_error
open Forester_compiler

module L = Lsp.Types

(* This function is mainly decodes the arguments to the command*)
let execute (params : L.ExecuteCommandParams.t) =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let config = State.config forest in
  match params with
  | { arguments; command; _ } ->
    match command with
    | "new tree" ->
      let open Yojson.Safe.Util in
      let prefix, mode =
        match arguments with
        | Some [json_stuff] ->
          let prefix = json_stuff |> member "prefix" |> to_string in
          let mode =
            json_stuff |> member "mode" |> to_string
            |> function
            | "random" -> `Random
            | "sequential" -> `Sequential
            | _ ->
              raise @@
              decode_error @@
              Format.asprintf
                "got invalid arguments when executing \"new tree\" command"
          in
          prefix, mode
        | x ->
          raise @@
          decode_error @@
          Format.(
            asprintf
              "got invalid arguments when executing \"new tree\" command. Expected data in the shape of [{\"prefix\" = ..., \"mode\" = ...}], but got: %a."
              (pp_print_option (pp_print_list Yojson.Safe.pp))
              x
          )
      in
      let env = State.env forest in
      let template = None in
      let res = Forester_frontend.Forester.create_tree ~env ~prefix ~template ~mode ~config ~forest in
      `String res
    | _ -> `Null

let resolve (params : L.CodeAction.t) = params

let create_new_tree_cmd ~prefix ~mode =
  let mode = match mode with `Sequential -> "sequential" | `Random -> "random" in
  L.Command.create
    ~command: "new tree"
    ~title: ""
    ~arguments: [
      `Assoc
        [
          "prefix", `String prefix;
          "mode", `String mode
        ]
    ]
    ()

let compute (_params : L.CodeActionParams.t) : L.CodeActionResult.t =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let config = State.config forest in
  let prefixes = config.prefixes in
  Eio.traceln "got %i prefixes" (List.length prefixes);
  let actions =
    prefixes
    |> List.concat_map
      (
        fun prefix ->
          let sequential =
            L.CodeAction.create
              ~title: (Format.asprintf "create tree with prefix %s" prefix)
              ~kind: (L.CodeActionKind.Other "new tree")
              ~command: (create_new_tree_cmd ~prefix ~mode: `Sequential)
              ()
          in
          let random =
            L.CodeAction.create
              ~title: (Format.asprintf "create tree with prefix %s (random)" prefix)
              ~kind: (L.CodeActionKind.Other "new tree")
              ~command: (create_new_tree_cmd ~prefix ~mode: `Random)
              ()
          in
          [`CodeAction sequential; `CodeAction random]
      )
  in
  Some actions
