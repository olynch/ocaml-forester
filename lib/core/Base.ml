type addr =
  | User_addr of string
  (** The address of a tree that can be referenced from user text. *)

  | Machine_addr of int
  (** The address of an anonymous tree.*)
[@@deriving repr]

let pp_addr fmt =
  function
  | User_addr str -> Format.pp_print_string fmt str
  | Machine_addr ix -> Format.fprintf fmt "#%i" ix

module Addr =
struct
  type t = addr
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)

  let to_user_addr =
    function
    | User_addr addr -> Some addr
    | _ -> None
end

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
