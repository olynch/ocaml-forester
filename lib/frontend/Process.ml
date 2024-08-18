open Forester_compiler

let read_trees_in_dirs ~dev ?(ignore_malformed = false) dirs =
  Forest_scanner.scan_directories dirs |> List.of_seq |> List.filter_map @@ fun fp ->
  Option.bind (Eio.Path.split fp) @@ fun (_, basename) ->
  let addr = Filename.chop_extension basename in
  let native = Eio.Path.native_exn fp in
  let source_path = if dev then Some (Unix.realpath native) else None in
  match Parse.parse_file native with
  | Result.Ok code -> Some Code.{source_path; addr = Some addr; code}
  | Result.Error _ -> None
  | exception exn ->
    if ignore_malformed then None else raise exn
