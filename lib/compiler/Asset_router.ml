(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

let router : (string, iri) Hashtbl.t = Hashtbl.create 100

let install ~host ~source_path ~content =
  match Hashtbl.find_opt router source_path with
  | Some iri -> iri
  | None ->
    let hash = Result.get_ok @@ Multihash_digestif.of_cstruct `Sha3_256 (Cstruct.of_string content) in
    let cid = Cid.v ~version: `Cidv1 ~codec: `Raw ~base: `Base32 ~hash in
    let cid_str = Cid.to_string cid in
    let ext = Filename.extension source_path in
    let filename = cid_str ^ ext in
    let iri = Iri_scheme.hash_iri ~host filename in
    Hashtbl.add router source_path iri;
    iri

let iri_of_asset ?loc ~source_path () =
  match Hashtbl.find_opt router source_path with
  | Some iri -> iri
  | None ->
    Reporter.fatalf ?loc Resource_not_found "Asset located at `%s' does not have a content address" source_path
