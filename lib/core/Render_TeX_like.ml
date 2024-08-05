open Forester_prelude

module Printer =
struct
  module P0 =
  struct
    type out = Format.formatter
    let text txt fmt =
      Format.fprintf fmt "%s" txt
  end

  include Printer_kit.Kit (P0)

  let contents (printer : t) : string =
    Format.asprintf "%a" (fun fmt _ -> printer fmt) ()
end

let rec render_node : Sem.node Range.located -> Printer.t =
  fun located ->
  match located.value with
  | Sem.Text txt | Sem.Verbatim txt ->
    Printer.text txt
  | Sem.Math (mode, xs) ->
    render xs;
  | Sem.Xml_tag (_, _, body) ->
    render body
  | Sem.TeX_cs (Symbol x) ->
    Printer.text @@ Format.sprintf "\\%c" x
  | Sem.TeX_cs (Word x) ->
    Printer.text @@ Format.sprintf "\\%s " x
  | node ->
    Reporter.fatalf ?loc:located.loc Type_error "Render_TeX_like: cannot render this kind of object"

and render nodes =
  Printer.iter render_node nodes
