(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_compiler
open Forester_frontend

module L = Lsp.Types
module RPC = Jsonrpc

(* TODO: set up json conversions for forester config*)
let compute (params : L.DidChangeConfigurationParams.t) =
  match params with
  | { settings } ->
    begin
      match settings with
      | `Assoc xs ->
        begin
          match List.assoc_opt "configuration_file" xs with
          | Some (`String f) ->
            let config = Config_parser.parse_forest_config_file f in
            Lsp_state.modify (fun state -> { state with forest = State.with_config config state.forest })
          | _ ->
            Eio.traceln "invalid value for configuration_file"
          (* RPC.Response.Error.raise *)
          (*   ( *)
          (*     RPC.Response.Error.make *)
          (*       ~code: InvalidRequest *)
          (*       ~message: "invalid value for configuration_file" *)
          (*       () *)
          (*   ) *)
        end
      | json -> Eio.traceln "unknown configuration value %a" Yojson.Safe.pp json
    end
