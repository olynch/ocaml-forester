open Forester_core

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty Eio.Path.t

val plant_forest_from_dirs :
  env:env ->
  dev:bool ->
  dir list ->
  unit

val render_forest :
  env:env ->
  dev:bool ->
  root:string option ->
  stylesheet : string ->
  unit

val copy_contents_of_dir :
  env:env ->
  dir ->
  unit

val create_tree :
  env:env ->
  dest:dir ->
  prefix:string ->
  template:string option ->
  mode:[`Sequential | `Random] ->
  string

val json_manifest :
  root:string option ->
  dev:bool ->
  string

val complete :
  string ->
  (addr * string) Seq.t
