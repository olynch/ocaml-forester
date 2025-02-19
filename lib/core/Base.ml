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

module Iri_ord = struct
  type t = Iri.t
  let compare = Iri.compare ~normalize: true
end

module Iri_set = Set.Make(Iri_ord)
module Iri_map = Map.Make(Iri_ord)
module String_map = Map.Make(String)

module Iri_tbl = struct

  (* Ought to use Iri_hash, but I am optimising by avoiding normalizing on equality comparison. *)
  module Tbl = Hashtbl.Make(struct
    type t = iri
    let equal = Iri.equal ~normalize: false

    (* IRI has mutable state that seems to be interfering with Hashtbl.hash *)
    let clean iri = Iri.with_query iri (Iri.query iri)

    let hash iri = clean iri |> Hashtbl.hash
  end)

  type 'a t = 'a Tbl.t
  type key = iri

  let find_opt tbl iri =
    Tbl.find_opt tbl (Iri.normalize iri)

  let mem tbl iri =
    Tbl.mem tbl (Iri.normalize iri)

  let add tbl iri x =
    Tbl.add tbl (Iri.normalize iri) x

  let replace tbl iri x =
    Tbl.replace tbl (Iri.normalize iri) x

  let length = Tbl.length
  let create = Tbl.create
  let iter = Tbl.iter
  let to_seq_values = Tbl.to_seq_values
  let to_seq_keys = Tbl.to_seq_keys
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
