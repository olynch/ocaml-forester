open Forester_prelude
module EP = Eio.Path
module S = Algaeff.Sequencer.Make(struct type t = string list end)

let rec process_file dirs fp =
  if EP.is_directory fp then
    let dirs =
      match EP.split fp with
      | Some (_, basename) -> dirs @ [basename]
      | None -> dirs
    in
    process_dir dirs fp
  else
    let@ _, basename = Option.iter @~ EP.split fp in
    if not @@ String.starts_with ~prefix: "." basename then
      S.yield (dirs @ [basename])

and process_dir dirs dir =
  try
    let@ fp = List.iter @~ EP.read_dir dir in
    process_file dirs EP.(dir / fp)
  with
    | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> ()

let scan_directories dirs =
  let@ () = S.run in
  let@ fp = List.iter @~ dirs in
  let@ _, basename = Option.iter @~ EP.split fp in
  process_dir [basename] fp
