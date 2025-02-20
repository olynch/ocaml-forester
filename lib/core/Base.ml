(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type iri = Iri.t

let iri_t = Repr.map Repr.string (Iri.of_string ~pctdecode: true) (Iri.to_string ~pctencode: true)

let pp_iri (fmt : Format.formatter) (iri : Iri.t) =
  Format.fprintf fmt "%s" @@
    Iri.to_string ~pctencode: false iri

module Iri_hash_safe : sig
  type t = private iri
  val make : iri -> t
  include Hashtbl.HashedType with type t := t
end = struct
  type t = iri
  let make (x : iri) : t = Iri.normalize x
  let equal = Iri.equal ~normalize: false

  let distill iri = Iri.scheme iri, Iri.host iri, Iri.port iri, Iri.user iri, Iri.path iri

  (* IRI has mutable state that seems to be interfering with Hashtbl.hash *)
  let hash (iri : t) = Hashtbl.hash (distill iri)
end

module Iri_set = Iri.Set
module Iri_map = Iri.Map

module Iri_tbl = struct
  module S = Iri_hash_safe
  module Tbl = Hashtbl.Make(S)

  type 'a t = 'a Tbl.t
  type key = iri

  let find_opt tbl iri =
    Tbl.find_opt tbl (S.make iri)

  let mem tbl iri =
    Tbl.mem tbl (S.make iri)

  let add tbl iri x =
    Tbl.add tbl (S.make iri) x

  let replace tbl iri x =
    Tbl.replace tbl (S.make iri) x

  let length = Tbl.length
  let create = Tbl.create
  let iter f = Tbl.iter (fun x y -> f (x : S.t :> iri) y)
  let to_seq_values = Tbl.to_seq_values
  let to_seq_keys tbl = Tbl.to_seq_keys tbl |> Seq.map (fun x -> (x : S.t :> iri))
end

type delim =
  Braces | Squares | Parens
[@@deriving show, repr]

type binding_strategy =
  Lazy | Strict
[@@deriving show, repr]

type 'a binding = binding_strategy * 'a
[@@deriving show, repr]

let delim_to_strings = function
  | Braces -> "{", "}"
  | Squares -> "[", "]"
  | Parens -> "(", ")"

type math_mode =
  Inline | Display
[@@deriving show, repr]

type visibility =
  Private | Public
[@@deriving show, repr]
