(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude

let scheme = "forest"

let base_iri ~host =
  Iri.iri ~scheme ~host ()

let user_iri ~host str =
  Iri.iri
    ~host
    ~scheme
    ~path: (Absolute [str])
    ()

let hash_iri ~host hash_str =
  Iri.iri
    ~host
    ~scheme
    ~path: (Absolute ["hash"; hash_str])
    ()

let is_named_iri iri =
  match Iri.scheme iri, Iri.path iri with
  | sch, Absolute ("hash" :: _) when sch = scheme -> false
  | _ -> true

let relativise_iri ~host iri =
  if Iri.scheme iri = scheme && Iri.host iri = Some host then
    let (Iri.Absolute components | Iri.Relative components) = Iri.path iri in
    Iri.iri ~path: (Iri.Relative components) ()
  else
    iri

let last_segment str =
  str
  |> String.split_on_char '/'
  |> List.rev
  |> List.hd
(* |> Filename.chop_extension *)

let name
    : Iri.t -> string
  = fun iri ->
    iri
    |> Iri.path_string
    |> last_segment

let split_addr
    : Iri.t -> string * int option
  = fun iri ->
    let name = last_segment @@ Iri.path_string iri in
    (* primitively check for address of form YYYY-MM-DD *)
    let date_regex = Str.regexp {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$|} in
    if Str.string_match date_regex name 0 then
      (name, None)
    else
      match String.rindex_opt name '-' with
      | Some i ->
        let prefix = String.sub name 0 i
        and suffix = String.sub name (i + 1) (String.length name - i - 1)
        in
        begin
          match BaseN.Base36.int_of_string suffix with
          | Some key -> prefix, Some key
          | None -> name, None
        end
      | _ -> name, None

let path_to_iri ~host str =
  str
  |> last_segment
  |> user_iri ~host

let uri_to_iri
    : host: string -> Lsp.Uri.t -> Iri.t
  = fun ~host uri ->
    let iri =
      uri
      |> Lsp.Uri.to_path
      |> Filename.chop_extension
      |> last_segment
      |> user_iri ~host
    in
    assert ((Filename.extension @@ Iri.path_string iri) = "");
    iri

let path_to_iri ~host str =
  str
  |> last_segment
  |> user_iri ~host

let source_path_to_addr p = Filename.(chop_extension @@ basename p)

let () =
  let@ exn = Printexc.register_printer in
  match exn with
  | Iri.Error err ->
    Option.some @@ Format.sprintf "Iri.error (%s)" (Iri.string_of_error err)
  | _ -> None
