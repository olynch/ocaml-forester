module Rel =
struct
  type t = string
  let pp = Format.pp_print_string
  let show x = x

  let make_builtin name = "org.forester.rel." ^ name

  let links = make_builtin "links"
  let transclusion = make_builtin "transclusion"
  let authors = make_builtin "authors"
  let contributors = make_builtin "contributors"
  let taxa = make_builtin "taxa"
  let tags = make_builtin "tags"
end

type rel = Rel.t

type mode =
  | Edges
  | Paths
[@@deriving show, repr]

type polarity =
  | Incoming
  | Outgoing
[@@deriving show, repr]

type rel_query = mode * polarity * Rel.t
[@@deriving show]
