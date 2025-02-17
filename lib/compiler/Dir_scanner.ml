(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
module EP = Eio.Path
module S = Algaeff.Sequencer.Make(struct type t = Eio.Fs.dir_ty EP.t end)

let rec process_file condition fp =
  if EP.is_directory fp then
    process_dir condition fp
  else if condition fp then
    S.yield fp

and process_dir condition dir =
  assert (not @@ Filename.is_relative @@ EP.native_exn dir);
  try
    let@ fp = List.iter @~ EP.read_dir dir in
    process_file condition EP.(dir / fp)
  with
    | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let is_tree fp =
  match EP.split fp with
  | None -> false
  | Some (_, basename) ->
    Filename.extension basename = ".tree" && not @@ String.starts_with ~prefix: "." basename

let is_not_hidden fp =
  match EP.split fp with
  | None -> false
  | Some (_, basename) ->
    not @@ String.starts_with ~prefix: "." basename

let matching_basename str fp =
  match EP.split fp with
  | None -> false
  | Some (_, basename) ->
    is_tree fp
    && Filename.chop_extension basename = str

let scan_directories dirs =
  let@ () = S.run in
  let@ fp = List.iter @~ dirs in
  process_dir is_tree fp

let scan_asset_directories dirs =
  let@ () = S.run in
  let@ fp = List.iter @~ dirs in
  process_dir is_not_hidden fp

exception Is_relative of string

let find_tree dirs iri =
  let matches =
    let@ () = S.run in
    let@ fp = List.iter @~ dirs in
    process_dir (matching_basename (Iri_scheme.name iri)) fp
  in
  try
    let first_match = List.hd @@ List.of_seq matches in
    let native = Eio.Path.native_exn first_match in
    if Filename.is_relative native then raise (Is_relative native);
    Some native
  with
    | Is_relative _ -> assert false
    | Failure _ -> None
