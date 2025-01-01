(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty Eio.Path.t

type format = HTML | JSON | XML | STRING

val plant_assets :
  env:
  < cwd: [> Eio.Fs.dir_ty] Eio.Path.t;
  fs: Eio.Fs.dir_ty Eio.Path.t;
  .. > ->
  host: string ->
  asset_dirs: Eio.Fs.dir_ty Eio.Path.t list ->
  forest: Compiler.state ->
  unit

val plant_raw_forest_from_dirs :
  env: env ->
  dev: bool ->
  config: Forester_forest.Config.Forest_config.t ->
  Compiler.state

val render_forest :
  dev: bool ->
  forest: Compiler.state ->
  unit

val render_tree :
  env: env ->
  format: format ->
  config: Forester_forest.Config.Forest_config.t ->
  string ->
  unit

val copy_contents_of_dir :
  env: env ->
  dir ->
  unit

val create_tree :
  env: env ->
  prefix: string ->
  template: string option ->
  mode: [`Sequential | `Random] ->
  config: Forester_forest.Config.Forest_config.t ->
  forest: Compiler.state ->
  string

(* val export_publication : *)
(*   env:< cwd : [> Eio.Fs.dir_ty ] Eio.Path.t; .. > -> Forester_compiler.Job.publication -> unit *)

val json_manifest :
  dev: bool ->
  forest: Compiler.state ->
  string

val complete :
  forest: Compiler.state ->
  string ->
  (iri * string) Seq.t
