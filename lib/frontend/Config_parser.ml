(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

let parse lexbuf filename =
  match Toml.Parser.parse lexbuf filename with
  | `Error (desc, { source; _ }) ->
    let@ () = Reporter.tracef "when parsing configuration file" in
    let loc = Asai.Range.of_lexbuf ~source: (`File source) lexbuf in
    Reporter.fatalf ~loc Configuration_error "%s" desc
  | `Ok tbl ->
    let open Toml.Lenses in
    let forest = key "forest" |-- table in
    let host =
      match get tbl (forest |-- key "host" |-- string) with
      | Some host -> String.lowercase_ascii host
      | None -> Reporter.fatalf Configuration_error "You need to set the `host' key in your configuration file; this is a global identifier that will be used to distinguish your forest from other forests (you can use your name, e.g. `johnqpublic')"
    in
    let home = get tbl (forest |-- key "home" |-- string) in
    let _ =
      match get tbl (forest |-- key "root" |-- string) with
      | None -> ()
      | Some _ ->
        Reporter.emitf Configuration_error "In your configuration file, change `root' key to `home' in the [forest] group."
    in
    let _ =
      match get tbl (forest |-- key "stylesheet" |-- string) with
      | None -> ()
      | Some _ ->
        Reporter.emitf Configuration_error "Custom XSL stylesheet injection is no longer supported; please remove the `stylesheet' key from the [forest] group."
    in
    let trees =
      Option.value ~default: Config.default.trees @@
        get tbl (forest |-- key "trees" |-- array |-- strings)
    in
    let foreign =
      Option.value ~default: Config.default.foreign @@
        get tbl (forest |-- key "foreign" |-- array |-- strings)
    in
    let assets =
      Option.value ~default: Config.default.assets @@
        get tbl (forest |-- key "assets" |-- array |-- strings)
    in
    let theme =
      Option.value ~default: Config.default.theme @@
        get tbl (forest |-- key "theme" |-- string)
    in
    let prefixes =
      Option.value ~default: Config.default.prefixes @@
        get tbl (forest |-- key "prefixes" |-- array |-- strings)
    in
    Config.{ host; assets; trees; foreign; theme; home; prefixes }

let parse_forest_config_string str =
  let lexbuf = Lexing.from_string str in
  parse lexbuf "<anonymous>"

let parse_forest_config_file filename =
  let ch = open_in filename in
  let@ () = Fun.protect ~finally: (fun _ -> close_in ch) in
  let lexbuf = Lexing.from_channel ch in
  parse lexbuf filename
