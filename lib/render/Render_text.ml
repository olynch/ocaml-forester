open Forester_prelude
open Forester_core

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

let rec render_node ~trees : Sem.node Range.located -> Printer.t =
  fun node ->
  match node.value with
  | Sem.Text txt | Sem.Verbatim txt ->
    Printer.text txt
  | Sem.Math (_, xs) ->
    render ~trees xs
  | Sem.Xml_tag (name, _, body) ->
    render ~trees body
  | Sem.Link (addr, None, modifier) ->
    render ~trees @@
    Option.value ~default:[Range.locate_opt None @@ Sem.Text "Untitled"] @@
    Option.bind (Addr_map.find_opt addr trees) @@ fun (tree : Sem.tree)  ->
    Option.map (Sem.apply_modifier modifier) tree.fm.title
  | Sem.Link (addr, Some title, modifier) ->
    render ~trees @@ Sem.apply_modifier modifier title
  | Sem.Prim (_, x) ->
    render ~trees x
  | Sem.TeX_cs _ ->
    Printer.nil
  | _ ->
    Reporter.fatal ?loc:node.loc Unhandled_case "unhandled case in plain text renderer"

and render ~trees xs =
  Printer.iter (render_node ~trees) xs
