(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
module EP = Eio.Path
module S = Algaeff.Sequencer.Make(struct type t = Eio.Fs.dir_ty EP.t end)

let rec process_file fp =
  if EP.is_directory fp then
    process_dir fp
  else
    let@ _, basename = Option.iter @~ EP.split fp in
    if Filename.extension basename = ".tree" && not @@ String.starts_with ~prefix: "." basename then
      S.yield fp

and process_dir dir =
  try
    let@ fp = List.iter @~ EP.read_dir dir in
    process_file EP.(dir / fp)
  with
    | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let scan_directories dirs =
  let@ () = S.run in
  let@ fp = List.iter @~ dirs in
  process_dir fp
