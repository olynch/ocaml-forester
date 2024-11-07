(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

module L = Lsp.Types
module RPC = Jsonrpc

(* TODO: set up json conversions for forester config*)
let compute (params : L.DidChangeConfigurationParams.t) =
  let server = State.get () in
  match params with
  | { settings } ->
    begin
      match settings with
      | `Assoc xs ->
        begin
          match List.assoc_opt "configuration_file" xs with
          | Some (`String f) ->
            let config = Forester_frontend.Config.parse_forest_config_file f in
            State.set { server with config = config }
          | _ ->
            RPC.Response.Error.raise
              (
                RPC.Response.Error.make
                  ~code: InvalidRequest
                  ~message: "invalid value for configuration_file"
                  ()
              )
        end
      | json -> Eio.traceln "unknown configuration value %a" Yojson.Safe.pp json
    end
