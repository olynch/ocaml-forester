open Forester_core

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
   root : string option;
   stylesheet : string;
   no_assets: bool;
   no_theme: bool}

val read_and_render_forest : cfg:config -> Eio.Fs.dir_ty Eio.Path.t list -> unit
val create_tree : cfg:config -> addrs:string Seq.t -> dest:Eio.Fs.dir_ty Eio.Path.t -> prefix:string -> template:string option -> mode:[`Sequential | `Random] -> string

val complete : string -> (addr * string) Seq.t
