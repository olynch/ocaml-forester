type addr = Addr.t
let pp_addr = Addr.pp

module Addr_map = Map.Make(Addr)
module Addr_set = Set.Make(Addr)
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

type xml_qname = { prefix: string; uname: string; xmlns: string option }
[@@deriving show, repr]
