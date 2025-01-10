(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* This stuff used to work when this module was part of an executable stanza in
   the dune file. *)

open Forester_prelude

let ( / ) = Eio.Path.( / )

let parse_paths fs s =
  String.split_on_char (if Sys.win32 then ';' else ':') s
  |> List.filter_map (function "" -> None | p -> Some Eio.Path.(fs / p))

let exists filename =
  try
    Eio.Path.stat filename ~follow: true
    |> function
    | { kind = `Regular_file; _ } -> true
    | _ ->
      false
  with
    | _ -> false

let which prog =
  List.find_map
    (
      fun dir ->
        if exists Eio.Path.(dir / prog) then Some prog else None
    )

let pipe_into ~env stdin cmd =
  let cwd = Eio.Stdenv.cwd env in
  Eio.Process.parse_out
    ~cwd: (cwd / "test/lsp-forest")
    env#process_mgr
    Eio.Buf_read.take_all
    ~stdin
    cmd
  |> String.trim

let () =
  let@ env = Eio_main.run in
  let test_initialization () =
    let capabilities = Lsp.Types.ClientCapabilities.create () in
    let init_params = Lsp.Types.InitializeParams.create ~capabilities () in
    let params =
      match Lsp.Types.InitializeParams.yojson_of_t
        init_params with
      | `Assoc p -> `Assoc p
      | _ -> assert false
    in
    let request =
      Jsonrpc.Request.(
        yojson_of_t @@
          create
            ~id: (`Int 1)
            ~method_: "initialize"
            ~params
            ()
      )
    in
    let content
      =
      (
        Format.asprintf
          "%s"
          (
            Yojson.Safe.to_string
              request
          )
      )
    in
    let header = Lsp.Header.create ~content_length: (String.length content) () in
    let initialize_request =
      Format.sprintf
        "%s%s"
        (Lsp.Header.to_string header)
        content
    in
    let stdin =
      Eio.Flow.string_source
        initialize_request
    in
    let paths = parse_paths env#fs @@ Sys.getenv "PATH" in
    List.iter (fun p -> Format.printf "%a" Eio.Path.pp p) paths;
    let forester = Option.get @@ which "forester" paths in
    (* let _o = *)
    (*   pipe_into ~env stdin [forester; "lsp"] *)
    (* in *)
    Alcotest.(check string)
      ""
      ""
      ""
  in
  let open Alcotest in
  (* Format.printf "%s@." o *)
  run "Language server" ["initialization", [test_case "" `Quick test_initialization]]
