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
