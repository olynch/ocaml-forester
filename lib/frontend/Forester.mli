open Forester_core

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty Eio.Path.t

val plant_raw_forest_from_dirs :
  env: env ->
  host: string ->
  dev: bool ->
  tree_dirs: dir list ->
  asset_dirs: dir list ->
  foreign_dirs: dir list ->
  unit

val render_forest :
  env: env ->
  dev: bool ->
  host: string ->
  home: string option ->
  stylesheet: string ->
  unit

val export :
  env: env ->
  host: string ->
  asset_dirs: dir list ->
  unit

val copy_contents_of_dir :
  env: env ->
  dir ->
  unit

val create_tree :
  env: env ->
  dest: dir ->
  prefix: string ->
  template: string option ->
  mode: [`Sequential | `Random] ->
  string

val json_manifest :
  host: string ->
  home: string option ->
  dev: bool ->
  string

val complete :
  host: string ->
  string ->
  (iri * string) Seq.t
