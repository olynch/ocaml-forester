(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

let router : (string, iri) Hashtbl.t = Hashtbl.create 100

let normalize source_path =
  try
    Unix.realpath source_path
  with
    | Unix.Unix_error (e, _, m) ->
      Reporter.fatalf Configuration_error "%s: %s" (Unix.error_message e) m

let install ~host ~source_path ~content =
  let normalized = normalize source_path in
  match Hashtbl.find_opt router normalized with
  | Some iri -> iri
  | None ->
    let hash = Result.get_ok @@ Multihash_digestif.of_cstruct `Sha3_256 (Cstruct.of_string content) in
    let cid = Cid.v ~version: `Cidv1 ~codec: `Raw ~base: `Base32 ~hash in
    let cid_str = Cid.to_string cid in
    let ext = Filename.extension normalized in
    let filename = cid_str ^ ext in
    let iri = Iri_scheme.hash_iri ~host filename in
    Hashtbl.add router normalized iri;
    iri

let iri_of_asset ?loc ~source_path () =
  let normalized = normalize source_path in
  match Hashtbl.find_opt router normalized with
  | Some iri -> iri
  | None ->
    Reporter.fatalf ?loc Resource_not_found "Asset located at `%s' does not have a content address" normalized
