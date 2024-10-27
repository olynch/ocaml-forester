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

module Make (I: Input) = struct
  let reduce ?(max_fibers = 20) ?(init = I.nil) ~env inputs =
    List.fold_left I.plus init @@
      Eio.Fiber.List.map ~max_fibers (I.eval env) inputs
end
