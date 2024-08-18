type addr = Addr.t
let pp_addr = Addr.pp

module Addr_map = Map.Make (Addr)
module Addr_set = Set.Make (Addr)
module String_map = Map.Make (String)

type delim = Braces | Squares | Parens
[@@deriving show]

type binding_strategy = Lazy | Strict
[@@deriving show]

type 'a binding = binding_strategy * 'a
[@@deriving show]

let delim_to_strings =
  function
  | Braces -> "{", "}"
  | Squares -> "[", "]"
  | Parens -> "(", ")"

type math_mode = Inline | Display
[@@deriving show]

type visibility = Private | Public
[@@deriving show]

type xml_qname = {prefix : string; uname : string; xmlns : string option}
[@@deriving show, repr]
