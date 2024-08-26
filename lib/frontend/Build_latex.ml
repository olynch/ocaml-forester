open Forester_core

type 'a env = 'a
  constraint 'a = < cwd: Eio.Fs.dir_ty Eio.Path.t;
  process_mgr: _ Eio.Process.mgr;
  stdout: _ Eio.Flow.sink;
  .. > as 'a

let resources_dir cwd =
  Eio.Path.(cwd / "build" / "resources")

let latex_to_svg ~env source =
  let cwd = Eio.Stdenv.cwd env in
  Eio_util.ensure_dir_path cwd ["build"; "resources"];
  let hash = Digest.to_hex @@ Digest.string source in
  let name = hash ^ ".svg" in
  let svg_path = Eio.Path.(resources_dir cwd / name) in
  try
    Eio.Path.load svg_path
  with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
      Reporter.emitf Log "Building %s" (Eio.Path.native_exn svg_path);
      let svg_code = LaTeX_pipeline.latex_to_svg ~env source in
      Eio.Path.save ~create: (`Or_truncate 0o644) svg_path svg_code;
      svg_code
