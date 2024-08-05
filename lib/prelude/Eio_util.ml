open Eio

let formatter_of_writer w =
  let out buf off len =
    Eio.Buf_write.string w buf ~off ~len
  in
  let flush () = () in
  Format.make_formatter out flush

module NullSink : Flow.Pi.SINK with type t = unit =
struct
  type t = unit
  let single_write _ _ = 0
  let copy _ ~src = ()
end

let null_sink () : Flow.sink_ty Resource.t =
  let ops = Eio.Flow.Pi.sink (module NullSink) in
  Eio.Resource.T ((), ops)

let ensure_dir path =
  try Eio.Path.mkdir ~perm:0o755 path with
  | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> ()

let ensure_dir_path path dirs =
  let rec loop path =
    function
    | [] -> ()
    | dir :: dirs ->
      let path' = Eio.Path.(path / dir) in
      ensure_dir path';
      loop path' dirs
  in
  loop path dirs

let ensure_remove_file path =
  try Eio.Path.unlink path with
  | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

let rec rm_rec path =
  match Eio.Path.is_file path with
  | true ->
    Eio.Path.unlink path
  | false ->
    match
      Eio.Path.read_dir path |> List.iter @@ fun fn ->
      rm_rec Eio.Path.(path / fn)
    with
    | _ -> Eio.Path.rmdir path
    | exception _ -> ()


let with_open_tmp_dir ~env kont =
  let dir_name = string_of_int @@ Oo.id (object end) in
  let cwd = Eio.Stdenv.cwd env in
  let tmp = "_tmp" in
  let () = ensure_dir_path cwd [tmp; dir_name] in
  let tmp_path = Eio.Path.(cwd / tmp / dir_name) in
  Eio.Path.with_open_dir tmp_path @@ fun p ->
  let result = kont p in
  rm_rec tmp_path;
  result


let run_process ?(quiet = false) ~env ~cwd cmd =
  let mgr = Eio.Stdenv.process_mgr env in
  let outbuf = Buffer.create 100 in
  let errbuf = Buffer.create 100 in
  let errsink = Eio.Flow.buffer_sink errbuf in
  let outsink = Eio.Flow.buffer_sink outbuf in
  if not quiet then
    Eio.traceln "Running %s" (String.concat " " cmd);
  try Eio.Process.run ~cwd ~stdout:outsink ~stderr:errsink mgr cmd with
  | exn ->
    Eio.traceln "Error: %s" (Buffer.contents errbuf);
    Eio.traceln "Output: %s" (Buffer.contents outbuf);
    raise exn


let file_exists path =
  try Eio.Path.with_open_in path @@ fun _ -> true with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

(* TODO: make this portable *)
let copy_to_dir ~env ~cwd ~source ~dest_dir =
  run_process ~quiet:true ~env ~cwd ["cp"; "-R"; source; dest_dir ^ "/" ]

