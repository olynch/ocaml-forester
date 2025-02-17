(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base

(** Forester uses an IRI scheme that incorporates a host that demarcates the current forest. A typical tree has have an address like this: [forest://host/xxx-NNNN]. Resources such as PDFs or images will be referred to by their hash.*)

val scheme : string

val base_iri :
  host: string ->
  iri

val user_iri :
  host: string ->
  string ->
  iri

val hash_iri :
  host: string ->
  string ->
  iri

val is_named_iri : iri -> bool

val relativise_iri :
  host: string ->
  iri ->
  iri

val uri_to_iri :
  host:string ->
  Lsp.Uri.t ->
  Iri.t

val split_addr :
  Iri.t ->
  string * int option

val path_to_iri :
  host:string ->
  string ->
  Iri.t

val last_segment : 
  string -> string

val name : Iri.t -> string
