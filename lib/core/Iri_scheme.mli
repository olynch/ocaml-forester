open Base

val scheme : string

val base_iri :
  host: string option ->
  iri

val user_iri :
  host: string option ->
  string ->
  iri

val hash_iri :
  host: string option ->
  string ->
  iri

val fresh :
  host: string option ->
  iri

val is_stable_iri : iri -> bool

val relativise_iri :
  host: string option ->
  iri ->
  iri
