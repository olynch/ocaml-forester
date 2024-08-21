open Forester_prelude
open Forester_core

module EP = Eio.Path

type 'a env = 'a constraint 'a = <
    cwd : Eio.Fs.dir_ty EP.t;
    process_mgr : _ Eio.Process.mgr;
    stdout : _ Eio.Flow.sink;
    ..
  > as 'a

let indent_string string =
  string
  |> String.split_on_char '\n'
  |> List.map (Format.sprintf "\t%s")
  |> String.concat "\n"

let latex_to_dvi ~(env : _ env) code  =
  let cwd = Eio.Stdenv.cwd env in
  let mgr = Eio.Stdenv.process_mgr env in

  Eio_util.with_open_tmp_dir ~env @@ fun tmp ->
  let tex_fn = "job.tex" in

  EP.save ~append:false ~create:(`Or_truncate 0o644) EP.(tmp / tex_fn) code;

  (* `latex' sends errors to stdout rather than stderr. *)
  let out_buf = Buffer.create 1000 in
  let stdout = Eio.Flow.buffer_sink out_buf in
  let stderr = Eio_util.null_sink () in

  let cmd = ["latex"; "-halt-on-error"; "-interaction=nonstopmode"; tex_fn] in

  begin
    try
      Eio.Process.run ~cwd:tmp ~stdout ~stderr mgr cmd
    with exn ->
      let formatted_output = Buffer.contents out_buf |> indent_string in
      Reporter.fatalf External_error
        "Encountered fatal LuaLaTeX error: @.@.%s@.@. while running `%s` in directory `%s`."
        formatted_output
        (String.concat " " cmd)
        (Eio.Path.native_exn tmp)
  end;

  EP.load EP.(tmp / "job.dvi")


let dvi_to_svg ~env dvi =
  let cwd = Eio.Stdenv.cwd env in
  let mgr = Eio.Stdenv.process_mgr env in
  let out_buf = Buffer.create 1000 in
  let err_buf = Buffer.create 1000 in
  let stdout = Eio.Flow.buffer_sink out_buf in
  let stderr = Eio_util.null_sink () in
  let stdin = Eio.Flow.string_source dvi in

  let cmd = ["dvisvgm"; "--exact"; "--clipjoin"; "--font-format=woff"; "--bbox=papersize"; "--zoom=1.5"; "--stdin"; "--stdout"] in

  begin
    try Eio.Process.run ~cwd ~stdin ~stdout ~stderr mgr cmd with _ ->
      Reporter.fatalf External_error "Encountered fatal error running `dvisvgm`"
  end;

  Buffer.contents out_buf

let latex_to_svg ~env code =
  code
  |> latex_to_dvi ~env
  |> dvi_to_svg ~env
