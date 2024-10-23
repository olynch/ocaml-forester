open Forester_core

let router : (string, string * iri) Hashtbl.t = Hashtbl.create 100

let install ~source_path ~content =
  match Hashtbl.find_opt router source_path with
  | Some (filename, iri) -> (filename, iri)
  | None ->
    let hash = Result.get_ok @@ Multihash_digestif.of_cstruct `Sha3_256 (Cstruct.of_string content) in
    let cid = Cid.v ~version: `Cidv1 ~codec: `Raw ~base: `Base32 ~hash in
    let cid_str = Cid.to_string cid in
    let ext = Filename.extension source_path in
    let filename = cid_str ^ ext in
    let iri = Iri.iri ~path: (Relative ["content"; filename]) () in
    Hashtbl.add router source_path (filename, iri);
    filename, iri

let iri_of_asset ~source_path =
  match Hashtbl.find_opt router source_path with
  | Some (_, iri) -> iri
  | None ->
  Reporter.fatalf Resource_not_found "Asset located at `%s' does not have a content address" source_path
