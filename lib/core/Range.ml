include Asai.Range

let pp_located pp_arg fmt (x : 'a located) =
  pp_arg fmt x.value

let map f node =
  { node with value = f node.value }

type string_source = Asai.Range.string_source = {
  title: string option;
  content: string;
}
[@@deriving repr]

type source = [`File of string
| `String of string_source]
[@@deriving repr]

type position = Asai.Range.position = {
  source: source;
  offset: int;
  start_of_line: int;
  line_num: int;
}
[@@deriving repr]

let t : t Repr.t =
  let open Repr in
  variant
    "t"
    begin
      fun range end_of_file t ->
        match view t with
        | `Range (x, y) -> range (x, y)
        | `End_of_file x -> end_of_file x
    end
  |~ case1 "Range" (pair position_t position_t) make
  |~ case1 "End_of_file" position_t eof
  |> sealv

type 'a located = 'a Asai.Range.located = { loc: t option; value: 'a }
[@@deriving repr]
