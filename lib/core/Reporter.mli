(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Message :
sig
  type t =
    | Tree_not_found of Base.iri
    | Duplicate_tree of Base.iri
    | Parse_error
    | Type_error
    | Type_warning
    | Resolution_error
    | Resolution_warning
    | Duplicate_attribute
    | Unhandled_case
    | Transclusion_loop
    | Internal_error
    | Configuration_error
    | Initialization_warning
    | Routing_error
    | Profiling
    | External_error
    | Resource_not_found
    | Broken_link
    | IO_error
    | Log
  val pp :
    Ppx_deriving_runtime.Format.formatter ->
    t ->
    Ppx_deriving_runtime.unit
  val show : t -> Ppx_deriving_runtime.string
  val default_severity : t -> Asai.Diagnostic.severity
  val short_code : t -> string
end

include module type of Asai.Reporter.Make(Message)
module Tty : module type of Asai.Tty.Make(Message)

type diagnostic = Message.t Asai.Diagnostic.t
val log : (Format.formatter -> 'a -> unit) -> 'a -> unit
val profile : string -> (unit -> 'a) -> 'a
val easy_run : (unit -> 'a) -> 'a
val silence : (unit -> 'a) -> 'a
val test_run : (unit -> 'a) -> 'a
val lsp_run :
  ?init_loc: Asai.Range.t ->
  ?init_backtrace: Asai.Diagnostic.backtrace ->
  ('a -> Message.t Asai.Diagnostic.t list -> unit) ->
  'a ->
  (unit -> unit) ->
  unit
val ignore :
  ?init_loc: Asai.Range.t ->
  ?init_backtrace: Asai.Diagnostic.backtrace ->
  (unit -> 'a) ->
  'a
