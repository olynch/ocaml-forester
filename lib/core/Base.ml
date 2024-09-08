type iri = Iri.t

let iri_t = Repr.map Repr.string Iri.of_string (Iri.to_string ~pctencode: false)

let pp_iri (fmt : Format.formatter) (iri : Iri.t) =
  Format.fprintf fmt "%s" @@
    Iri.to_string ~pctencode: false iri

module Iri_ord = struct
  type t = Iri.t
  let compare = Iri.compare ~normalize: true
end

module Iri_hash = struct
  include Iri_ord
  let equal = Iri.equal ~normalize: true
  let hash = Hashtbl.hash
end

module Iri_map = Map.Make(Iri_ord)
module String_map = Map.Make(String)

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
