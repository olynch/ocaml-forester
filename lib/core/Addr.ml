type t =
  | User_addr of string
  (** The address of a tree that can be referenced from user text. *)

  | Machine_addr of int
  (** The address of an tree with unstable address.*)

  | Hash_addr of string
  (** The address of a content-addressed tree. *)

  | Anon
[@@deriving repr]

let pp fmt =
  function
  | User_addr str -> Format.pp_print_string fmt str
  | Machine_addr ix -> Format.fprintf fmt "#%i" ix
  | Anon -> Format.fprintf fmt "<anon>"
  | Hash_addr hash -> Format.fprintf fmt "<hash:%s>" hash

let user_addr str = User_addr str
let hash_addr str = Hash_addr str
let fresh () = Machine_addr (Oo.id object end)

let anon = Anon

let is_user_addr =
  function
  | User_addr _ -> true
  | _ -> false

let compare = compare
let hash = Hashtbl.hash
let equal = (=)
