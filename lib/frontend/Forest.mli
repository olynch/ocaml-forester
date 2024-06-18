open Forester_core

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
   root : string option;
   stylesheet : string;
   ignore_tex_cache : bool;
   no_assets: bool;
   no_theme: bool}

type raw_forest = Code.tree list

type forest =
  {trees : Sem.tree Addr_map.t;
   run_query : addr Query.t -> Addr_set.t}

val plant_forest : raw_forest -> forest
val render_trees : cfg:config -> forest:forest -> render_only:addr list option -> unit
val create_tree : cfg:config -> addrs:string Seq.t -> dest:Eio.Fs.dir_ty Eio.Path.t -> prefix:string -> template:string option -> mode:[`Sequential | `Random] -> string

val complete : forest:forest -> string -> (string * string) Seq.t

val taxa : forest:forest-> (string * string) Seq.t
val tags : forest:forest -> (string * string list) Seq.t
