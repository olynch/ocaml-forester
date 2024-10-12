open Forester_core

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty Eio.Path.t

val plant_forest_from_dirs :
  env: env ->
  host: string option ->
  dev: bool ->
  tree_dirs: dir list ->
  asset_dirs: dir list ->
  unit

val render_forest :
  env: env ->
  dev: bool ->
  host: string option ->
  home: string option ->
  stylesheet: string ->
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
  host: string option ->
  home: string option ->
  dev: bool ->
  string

val complete :
  host: string option ->
  string ->
  (iri * string) Seq.t
