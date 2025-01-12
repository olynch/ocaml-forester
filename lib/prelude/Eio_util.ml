(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Eio
open Fun_util

(* let ( / ) = Eio.Path.( / ) *)

let path_of_dir ~env dir =
  Path.(Eio.Stdenv.fs env / dir)

let paths_of_dirs ~env =
  List.map (path_of_dir ~env)

module NullSink: Flow.Pi.SINK with type t = unit = struct
  type t = unit
  let single_write _ _ = 0
  let copy _ ~src: _ = ()
end

let null_sink () : Flow.sink_ty Resource.t =
  let ops = Eio.Flow.Pi.sink (module NullSink) in
  Eio.Resource.T ((), ops)

let ensure_context_of_path ~perm (path : _ Path.t) =
  let@ path', _ = Option.iter @~ Path.split path in
  Path.mkdirs ~exists_ok: true ~perm path'

let ensure_remove_file path =
  try
    Eio.Path.unlink path
  with
    | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let with_open_tmp_dir ~env kont =
  let dir_name = string_of_int @@ Oo.id ( object end) in
  let cwd = Eio.Stdenv.cwd env in
  let tmp = "_tmp" in
  let tmp_path = Eio.Path.(cwd / tmp / dir_name) in
  Path.mkdirs ~exists_ok: true ~perm: 0o755 tmp_path;
  let@ p = Eio.Path.with_open_dir tmp_path in
  let result = kont p in
  Path.rmtree ~missing_ok: true tmp_path;
  result

let run_process ?(quiet = false) ~env ~cwd cmd =
  let mgr = Eio.Stdenv.process_mgr env in
  let outbuf = Buffer.create 100 in
  let errbuf = Buffer.create 100 in
  let errsink = Eio.Flow.buffer_sink errbuf in
  let outsink = Eio.Flow.buffer_sink outbuf in
  if not quiet then
    Eio.traceln "Running %s" (String.concat " " cmd);
  try
    Eio.Process.run ~cwd ~stdout: outsink ~stderr: errsink mgr cmd
  with
    | exn ->
      Eio.traceln "Error: %s" (Buffer.contents errbuf);
      Eio.traceln "Output: %s" (Buffer.contents outbuf);
      raise exn

let file_exists path =
  try
    let@ _ = Eio.Path.with_open_in path in
    true
  with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

(* TODO: test this! *)
let copy_to_dir ~env ~cwd ~source ~dest_dir =
  if Sys.unix then
    run_process ~quiet: true ~env ~cwd ["cp"; "-R"; source; dest_dir ^ "/"]
  else
    run_process ~quiet: true ~env ~cwd ["xcopy"; source; dest_dir ^ "/"]
