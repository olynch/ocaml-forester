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

let split_addr addr =
  (* primitively check for address of form YYYY-MM-DD *)
  let date_regex = Str.regexp {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$|} in
  if Str.string_match date_regex addr 0 then
    (addr, None)
  else
    match String.rindex_opt addr '-' with
    | Some i ->
      let prefix = String.sub addr 0 i
      and suffix = String.sub addr (i + 1) (String.length addr - i - 1)
      in
      begin
        match BaseN.Base36.int_of_string suffix with
        | Some key -> prefix, Some key
        | None -> addr, None
      end
    | _ -> addr, None

let next_addr
    ~prefix
    ~mode
    ~(config : Config.Forest_config.t)
    (addrs : (string * string) List.t)
  =
  let keys =
    let@ (addr, uri) = List.filter_map @~ addrs in
    let prefix', key = split_addr addr in
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

let last_segment str =
  str
  |> String.split_on_char '/'
  |> List.rev
  |> List.hd
  |> Filename.chop_extension

let path_to_iri ~host str =
  try
    str
    |> last_segment
    |> Iri_scheme.user_iri ~host
  with
    | _ -> Eio.traceln "failed to convert path to iri segment %s" str; exit 1

let uri_to_addr uri =
  try
    uri
    |> Lsp.Uri.to_path
    |> last_segment
  with
    | _ -> Eio.traceln "failed to convert uri to addr %s" (Lsp.Uri.to_path uri); exit 1

let iri_to_addr iri =
  try
    iri
    |> Iri.to_string ~pctencode: false
    |> String.split_on_char '/'
    |> List.rev
    |> List.hd
  with
    | _ -> Eio.traceln "failed to convert iri to addr %s" (Iri.to_string iri); exit 1

let uri_to_iri
    : host: string -> Lsp.Uri.t -> iri
  = fun ~host uri ->
    uri
    |> uri_to_addr
    |> Iri_scheme.user_iri ~host

let source_path_to_addr p =
  p
  |> Filename.basename
  |> Filename.chop_extension

let start_of_file =
  let beginning = L.Position.create ~character: 0 ~line: 0 in
  L.Range.create ~start: beginning ~end_: beginning
