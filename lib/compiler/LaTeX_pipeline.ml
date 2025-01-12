(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

module EP = Eio.Path

type env = Eio_unix.Stdenv.base

let indent_string string =
  string
  |> String.split_on_char '\n'
  |> List.map (Format.sprintf "\t%s")
  |> String.concat "\n"

(* TODO: When error occurs on stderr, there is nothing informative in the diagnostic*)
let pipe_latex_dvi ~env ~tex_source ?loc: _ kont =
  let mgr = Eio.Stdenv.process_mgr env in
  let@ tmp = Eio_util.with_open_tmp_dir ~env in
  let tex_fn = "job.tex" in
  begin
    let@ tex_sink = EP.with_open_out ~create: (`Or_truncate 0o644) EP.(tmp / tex_fn) in
    Eio.Flow.copy tex_source tex_sink
  end;
  begin
    let out_buf = Buffer.create 1000 in
    let stdout = Eio.Flow.buffer_sink out_buf in
    let stderr = Eio_util.null_sink () in
    let cmd = ["latex"; "-halt-on-error"; "-interaction=nonstopmode"; tex_fn] in
    try
      Eio.Process.run ~cwd: tmp ~stdout ~stderr mgr cmd
    with
      | _ ->
        (* Eio.traceln "%a" (Format.pp_print_option Asai.Range.dump) loc; *)
        let formatted_output = Buffer.contents out_buf |> indent_string in
        Reporter.fatalf
          External_error
          (* ?loc *)
          "Encountered fatal LaTeX error: @.@.%s@.@. while running `%s` in directory `%s`."
          formatted_output
          (String.concat " " cmd)
          (Eio.Path.native_exn tmp)
  end;
  EP.with_open_in EP.(tmp / "job.dvi") kont

let pipe_dvi_svg ~env ?loc: _ ~dvi_source ~svg_sink () =
  let cwd = Eio.Stdenv.cwd env in
  let mgr = Eio.Stdenv.process_mgr env in
  let err_buf = Buffer.create 1000 in
  let stderr = Eio.Flow.buffer_sink err_buf in
  let cmd = ["dvisvgm"; "--exact"; "--clipjoin"; "--font-format=woff"; "--bbox=papersize"; "--zoom=1.5"; "--stdin"; "--stdout"] in
  try
    Eio.Process.run ~cwd ~stdin: dvi_source ~stdout: svg_sink ~stderr mgr cmd
  with
    | _ ->
      (* Eio.traceln "%a" (Format.pp_print_option Asai.Range.dump) loc; *)
      Reporter.fatalf
        (* ?loc *)
        External_error
        "Encountered fatal error running `dvisvgm`: %s"
        (Buffer.contents err_buf)

let pipe_latex_svg ~env ?loc ~tex_source ~svg_sink () =
  let@ dvi_source = pipe_latex_dvi ~env ~tex_source ?loc in
  pipe_dvi_svg ~env ?loc ~dvi_source ~svg_sink ()

let latex_to_svg ~env ?loc code =
  let tex_source = Eio.Flow.string_source code in
  let svg_buf = Buffer.create 1000 in
  let svg_sink = Eio.Flow.buffer_sink svg_buf in
  pipe_latex_svg ~env ~tex_source ~svg_sink ?loc ();
  Buffer.contents svg_buf
