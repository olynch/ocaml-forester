open Forester_core

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
   root : string option;
   stylesheet : string;
   no_assets: bool;
   no_theme: bool;
   dev : bool}

val plant_forest_from_dirs : cfg:config -> Eio.Fs.dir_ty Eio.Path.t list -> unit
val render_forest : cfg:config -> unit
val create_tree : cfg:config -> dest:Eio.Fs.dir_ty Eio.Path.t -> prefix:string -> template:string option -> mode:[`Sequential | `Random] -> string

val generate_json : cfg:config -> string
val complete : string -> (addr * string) Seq.t
