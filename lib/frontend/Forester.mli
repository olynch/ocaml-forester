(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty Eio.Path.t

type target = Target : 'a Render.target -> target

val compile :
  env: env ->
  dev: bool ->
  config: Config.t ->
  State.t

val render_forest :
  dev: bool ->
  forest: State.t ->
  unit

val render_tree :
  env: env ->
  config: Config.t ->
  target: target ->
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
  config: Config.t ->
  forest: State.t ->
  string

(* val export_publication : *)
(*   env:< cwd : [> Eio.Fs.dir_ty ] Eio.Path.t; .. > -> Forester_compiler.Job.publication -> unit *)

val json_manifest :
  dev: bool ->
  forest: State.t ->
  string

val complete :
  forest: State.t ->
  string ->
  (iri * string) Seq.t

val export : forest: State.t -> unit
