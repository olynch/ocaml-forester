open Eio.Std
open Forester_core
open Forester_prelude

type 'a env = 'a constraint 'a = <
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    process_mgr : _ Eio.Process.mgr;
    stdout : _ Eio.Flow.sink;
    ..
  > as 'a

let resources_dir cwd =
  Eio.Path.(cwd/"build"/"resources")

let build_latex ~env ~ignore_tex_cache ~name ~source : unit =
  let cwd = Eio.Stdenv.cwd env in
  let svg_path = Eio.Path.(resources_dir cwd / name) in

  if ignore_tex_cache || not @@ Eio_util.file_exists svg_path then
    begin
      Reporter.emitf Log "Building %s" (Eio.Path.native_exn svg_path);
      let svg_code = LaTeX_pipeline.latex_to_svg ~env source in
      Eio.Path.save ~create:(`Or_truncate 0o644) svg_path svg_code
    end
