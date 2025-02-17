(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
module L = Lsp.Types

let rec random_not_in keys =
  let attempt = Random.int (36 * 36 * 36 * 36 - 1) in
  if List.fold_left (fun x y -> x || y) false (List.map (fun k -> k = attempt) keys) then
    random_not_in keys
  else
    attempt

let next_iri
    : prefix: string ->
    mode: [< `Random | `Sequential] ->
    config: Config.t ->
    (iri * string) list ->
    string * string
  = fun
      ~prefix
      ~mode
      ~(config : Config.t)
      addrs
    ->
    let keys =
      let@ (addr, uri) = List.filter_map @~ addrs in
      let prefix', key = Iri_scheme.split_addr addr in
      if prefix = prefix' then
        Option.map (fun key -> (key, uri)) key
      else None
    in
    let next, dest_dir =
      match mode with
      | `Sequential ->
        let max, uri =
          List.fold_left
            (fun (i, uri) (acc_i, _) -> (max i acc_i, uri))
            (0, List.hd config.trees)
            keys
        in
        1 + max, uri
      | `Random ->
        random_not_in (List.map fst keys),
        snd @@ List.hd keys
    in
    prefix ^ "-" ^ BaseN.Base36.string_of_int next, dest_dir

(* Reporting diagnostics requires a document URI to publish *)
let guess_uri (d : Reporter.diagnostic) =
  match d with
  | { explanation; _ } ->
    match explanation.loc with
    | None -> None
    | Some loc ->
      match Range.view loc with
      | `End_of_file { source; _ }
      | `Range ({ source; _ }, _) ->
        match source with
        | `String _ -> None
        | `File path ->
          if path <> "" then
            Some (Lsp.Uri.of_path path)
          else None

let start_of_file =
  let beginning = L.Position.create ~character: 0 ~line: 0 in
  L.Range.create ~start: beginning ~end_: beginning
