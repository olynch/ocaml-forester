(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

type env = Eio_unix.Stdenv.base

let resources_dir cwd =
  Eio.Path.(cwd / "build" / "resources")

let latex_to_svg ~env source =
  let cwd = Eio.Stdenv.cwd env in
  let hash = Digest.to_hex @@ Digest.string source in
  let name = hash ^ ".svg" in
  let svg_path = Eio.Path.(resources_dir cwd / name) in
  let perm = 0o755 in
  Eio_util.ensure_context_of_path ~perm svg_path;
  try
    Eio.Path.load svg_path
  with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      Reporter.emitf Log "Building %s" (Eio.Path.native_exn svg_path);
      let svg_code = LaTeX_pipeline.latex_to_svg ~env source in
      Eio.Path.save ~create: (`Or_truncate perm) svg_path svg_code;
      svg_code
