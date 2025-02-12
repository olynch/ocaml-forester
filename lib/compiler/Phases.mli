(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

type transition = State.t -> State.t

val init : env: Eio_unix.Stdenv.base -> config: Config.t -> dev: bool -> State.t
val load : Eio.Fs.dir_ty Eio.Path.t list -> transition
(* val load_dependencies : iri -> transition *)
val load_configured_dirs : transition
val parse : quit_on_error: bool -> transition
val reparse : Lsp.Text_document.t -> transition
val build_import_graph : transition
val build_import_graph_for : iri: Forester_core.iri -> transition
val expand : quit_on_error: bool -> transition
val expand_only : Forester_core.iri -> transition
val eval : transition
val eval_only : Forester_core.iri -> transition
val implant_foreign : Eio.Fs.dir_ty Eio.Path.t list -> transition
val plant_assets : Eio.Fs.dir_ty Eio.Path.t list -> transition
