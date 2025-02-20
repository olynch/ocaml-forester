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

module String_map = Map.Make(String)

module Iri_ord_unsafe = struct
  type t = Iri.t
  let compare = Iri.compare ~normalize: true
end

module Iri_hash_unsafe = struct
  type t = iri
  let equal = Iri.equal ~normalize: false

  let distill iri = Iri.scheme iri, Iri.host iri, Iri.port iri, Iri.user iri, Iri.path iri

  (* IRI has mutable state that seems to be interfering with Hashtbl.hash *)
  let hash iri = Hashtbl.hash (distill iri)
end

module Iri_set = struct
  module M = Set.Make(Iri_ord_unsafe)
  type t = M.t
  type elt = M.elt
  let empty = M.empty
  let add iri = M.add (Iri.normalize iri)
  let mem iri = M.mem (Iri.normalize iri)
end

module Iri_map = struct
  module M = Map.Make(Iri_ord_unsafe)
  type 'a t = 'a M.t
  type key = M.key
  let empty = M.empty
  let add iri = M.add (Iri.normalize iri)
  let find_opt iri = M.find_opt (Iri.normalize iri)
  let to_seq = M.to_seq
  let cardinal = M.cardinal
  let to_list = M.to_list
end

module Iri_tbl = struct
  module Tbl = Hashtbl.Make(Iri_hash_unsafe)

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
