include Asai.Range

let pp_located pp_arg fmt (x : 'a located) =
  pp_arg fmt x.value

let map f node =
  {node with value = f node.value}

type string_source = Asai.Range.string_source = {
  title: string option;
  content: string;
}
[@@deriving repr]

type source =
  [ `File of string
  | `String of string_source
  ]
[@@deriving repr]

type position = Asai.Range.position = {
  source : source;
  offset : int;
  start_of_line : int;
  line_num : int;
}
[@@deriving repr]

let curry f x y = f (x, y)

let t : t Repr.t =
  let open Repr in
  record "range" (curry make)
  |+ field "start" position_t (Fun.compose fst split)
  |+ field "end" position_t (Fun.compose snd split)
  |> sealr

type 'a located = 'a Asai.Range.located = {loc : t option; value : 'a}
[@@deriving repr]
