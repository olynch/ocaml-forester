(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* TODO: should just use cram tests for this instead. *)

open Forester_core
open Forester_prelude
open Forester_compiler
open Forester_frontend

module EP = Eio.Path

module T = Types
module HTML = Pure_html.HTML

let config = {Config.default with trees = ["transclude"]}
let host = config.host

let href = Iri_scheme.user_iri ~host "transcludee"

module Transclusions = struct
  (* It would be cool to use quickcheck here, but no good way to test the result*)
  open T
  let full_default = {
    href;
    target = Full default_section_flags;
    modifier = Identity
  }

  let metadata_shown = {default_section_flags with
    metadata_shown =
    Some true
  }
end

let () =
  let@ env = Eio_main.run in
  let@ () = Reporter.easy_run in
  (* Needs to be false to make tests reproducible. The source path depends on the host *)
  let tree_dirs = Eio_util.paths_of_dirs ~env config.trees in
  let forest =
    Phases.init ~env ~config ~dev: false
    |> State_machine.(run_action Load_all_configured_dirs)
  in
  let iri = Iri_scheme.user_iri ~host "transcludee" in
  let print_transclusion
    : T.content T.transclusion -> unit
  = fun t ->
    let content = Option.get @@ Forest.get_content_of_transclusion t forest.resources in
    Format.printf
      "%a"
      Render.(pp_xml ~forest ~dev: false ?stylesheet: None)
      T.{
        frontmatter = default_frontmatter ~iri: href ();
        mainmatter = content;
        backmatter = Content []
      }
  in
  let test_full_default () =
    print_transclusion
      Transclusions.full_default
  in
  let test_title_default () =
    print_transclusion
      {
        href = iri;
        target = Title {empty_when_untitled = false};
        modifier = Identity
      }
  in
  let test_full_metadata () =
    print_transclusion
      {
        href = iri;
        target = Full Transclusions.metadata_shown;
        modifier = Identity
      }
  in
  List.iter
    (fun f -> f ())
    [
      test_full_default;
      test_title_default;
      test_full_metadata
    ]
