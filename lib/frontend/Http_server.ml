(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

module T = Types

let log_warning ex = Logs.warn (fun f -> f "WARNING: %a" Eio.Exn.pp ex)

let run
    : env: Eio_unix.Stdenv.base -> State.t -> unit
  = fun ~env state ->
    let module P = Pure_html in
    let module H = P.HTML in
    let@ sw = Eio.Switch.run ?name: None in
    let port = ref 8080 in
    (* let state = ref state in *)
    let handler _ request (_body : Cohttp_eio.Body.t) =
      (* let state = !state in *)
      let resource = Iri.of_string (Http.Request.resource request) in
      match Iri.path_string (resource) with
      | "/" ->
        begin
          let home =
            Option.bind
              (
                Option.map
                  (Iri_scheme.user_iri ~host: state.config.host)
                  state.config.home
              )
              (fun iri -> Forest.get_article iri state.resources)
          in
          match home with
          | None ->
            Cohttp_eio.Server.respond_string
              ~status: `OK
              ~body: (
                Format.asprintf
                  "not found"
              )
              ()
          | Some r ->
            Cohttp_eio.Server.respond_string
              ~status: `OK
              ~body: (
                Format.asprintf "%a" Render.(pp ~dev: true state HTML) (Article r)
              )
              ()
        end
      | "style.css" ->
        Cohttp_eio.Server.respond_string
          ~status: `OK
          ~body: Css.stylesheet
          ()
      | "/query" ->
        let q = Iri.query_kv resource |> Iri.KV.find_opt "query" in
        let response =
          q
          |> Option.get
          |> Iri_types.pct_decode
          |> Repr.of_json_string
            Datalog_expr.(query_t Repr.string (T.vertex_t T.content_t))
          |> function
          | Ok q ->
            Logs.app (fun m -> m "parsed successfully");
            let (_, _, result) = State_machine.update (Query q) state in
            begin
              match result with
              | Vertex_set vs ->
                Htmx_client.render_query_result state vs
              | Render_result _
              | Error _
              | Nothing ->
                [P.txt "failed to run"]
            end
          | Error (`Msg str) ->
            [P.txt "failed to parse: %s" str]
        in
        Cohttp_eio.Server.respond_string
          ~status: `OK
          ~body: (
            Format.asprintf
              "%a"
              P.pp
              (H.ul [] response)
          )
          ()
      | path ->
        let path =
          let len = String.length path in
          String.sub path 1 (len - 1)
        in
        let iri = Iri_scheme.user_iri ~host: state.config.host path in
        match Forest.get_article iri state.resources with
        | None ->
          Cohttp_eio.Server.respond_string
            ~status: `Not_found
            ~body: (
              Format.asprintf
                "%s: 404 not found"
                path
            )
            ()
        | Some r ->
          Cohttp_eio.Server.respond_string
            ~status: `OK
            ~body: (
              Format.asprintf "%a" Render.(pp ~dev: true state HTML) (Article r)
            )
            ()
    in
    let socket =
      Eio.Net.listen
        env#net
        ~sw
        ~backlog: 128
        ~reuse_addr: true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, !port))
    and server = Cohttp_eio.Server.make ~callback: handler ()
    in
    Cohttp_eio.Server.run socket server ~on_error: log_warning
