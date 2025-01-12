open Eio

(* val ( / ) : ([> Fs.dir_ty ] as 'a) Path.t -> string -> 'a Path.t *)

val path_of_dir :
  env:< fs : ([> Fs.dir_ty ] as 'a) Path.t; .. > ->
  string -> 'a Path.t

val paths_of_dirs :
  env:< fs : ([> Fs.dir_ty ] as 'a) Path.t; .. > ->
  string list -> 'a Path.t list

val null_sink : unit -> Flow.sink_ty Resource.t

val ensure_context_of_path :
  perm:File.Unix_perm.t -> [> Fs.dir_ty ] Path.t -> unit

val ensure_remove_file : [> Fs.dir_ty ] Path.t -> unit

val with_open_tmp_dir :
  env:< cwd : [> Fs.dir_ty ] Path.t; .. > ->
  ([< `Close | `Dir > `Dir ] Path.t -> 'a) -> 'a

val run_process :
  ?quiet:bool ->
  env:< process_mgr : [> [> `Generic ] Process.mgr_ty ] Process.mgr;
        .. > ->
  cwd:[> Fs.dir_ty ] Path.t -> string list -> unit

val file_exists : [> Fs.dir_ty ] Path.t -> bool

(* val try_create_dir : cwd:[> Fs.dir_ty ] Path.t -> string -> unit *)
(**)
(* val try_create_file : *)
(*   cwd:[> Fs.dir_ty ] Path.t -> ?content:string -> string -> unit *)

val copy_to_dir :
  env:< process_mgr : [> [> `Generic ] Process.mgr_ty ] Process.mgr;
        .. > ->
  cwd:[> Fs.dir_ty ] Path.t ->
  source:string -> dest_dir:string -> unit

