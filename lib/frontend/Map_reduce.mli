(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module type Monoid = sig
  type t
  val nil : t
  val plus : t -> t -> t
end

module type Input = sig
  type env
  type input
  type output

  include Monoid with type t := output
  val eval : env -> input -> output
end

module type S = sig
  type env
  type input
  type output

  val reduce :
    ?max_fibers: int ->
    ?init: output ->
    env: env ->
    input list ->
    output
end

module Make
  (I : Input) : S with
  type env := I.env
  and type input := I.input
  and type output := I.output
