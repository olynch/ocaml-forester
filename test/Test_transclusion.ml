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

let config = { Config.default with trees = ["transclude"] }
let host = config.host

let href = Iri_scheme.user_iri ~host "transcludee"

module Transclusions = struct
  (* It would be cool to use quickcheck here, but no good way to test the result*)
  open T
  let full_default =
    {
      href;
      target = Full default_section_flags;
      modifier = Identity
    }

  let metadata_shown =
    {
      default_section_flags with
      metadata_shown =
      Some true
    }
end

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  let@ () = Reporter.easy_run in
  (* Needs to be false to make tests reproducible. The source path depends on the host *)
  let tree_dirs = Eio_util.paths_of_dirs ~env config.trees in
  let forest =
    Phases.init ~env ~config ~dev: false
    |> State_machine.(run_action Load_all ~until: Do_nothing)
  in
  let iri = Iri_scheme.user_iri ~host "transcludee" in
  let test_transclusion t expect =
    let result = Forest.get_content_of_transclusion t forest.resources in
    Alcotest.(check @@ option string)
      ""
      expect
      (
        Option.map
          (
            fun content ->
              Format.asprintf
                "%a"
                Render.(pp_xml ~forest ~dev: false ?stylesheet: None)
                T.{
                  frontmatter = default_frontmatter ~iri: href ();
                  mainmatter = content;
                  backmatter = Content []
                }
          )
          result
      )
  in
  let test_full_default () =
    test_transclusion
      Transclusions.full_default
      (
        Some
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<fr:tree xmlns:fr=\"http://www.jonmsterling.com/jms-005P.xml\" root=\"false\">\n  <fr:frontmatter>\n    <fr:authors />\n    <fr:addr>transcludee</fr:addr>\n    <fr:route>transcludee.xml</fr:route>\n    <fr:title text=\"\" />\n  </fr:frontmatter>\n  <fr:mainmatter>\n    <fr:tree show-metadata=\"false\">\n      <fr:frontmatter>\n        <fr:authors />\n        <fr:addr>transcludee</fr:addr>\n        <fr:route>transcludee.xml</fr:route>\n        <fr:title text=\"I am being transcluded\">I am being transcluded</fr:title>\n      </fr:frontmatter>\n      <fr:mainmatter />\n    </fr:tree>\n  </fr:mainmatter>\n  <fr:backmatter />\n</fr:tree>\n"
      )
  in
  let test_title_default () =
    test_transclusion
      { href = iri; target = Title { empty_when_untitled = false }; modifier = Identity }
      (
        Some
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<fr:tree xmlns:fr=\"http://www.jonmsterling.com/jms-005P.xml\" root=\"false\">\n  <fr:frontmatter>\n    <fr:authors />\n    <fr:addr>transcludee</fr:addr>\n    <fr:route>transcludee.xml</fr:route>\n    <fr:title text=\"\" />\n  </fr:frontmatter>\n  <fr:mainmatter>I am being transcluded</fr:mainmatter>\n  <fr:backmatter />\n</fr:tree>\n"
      )
  in
  let test_full_metadata () =
    test_transclusion
      { href = iri; target = Full Transclusions.metadata_shown; modifier = Identity }
      (
        Some
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<fr:tree xmlns:fr=\"http://www.jonmsterling.com/jms-005P.xml\" root=\"false\">\n  <fr:frontmatter>\n    <fr:authors />\n    <fr:addr>transcludee</fr:addr>\n    <fr:route>transcludee.xml</fr:route>\n    <fr:title text=\"\" />\n  </fr:frontmatter>\n  <fr:mainmatter>\n    <fr:tree show-metadata=\"true\">\n      <fr:frontmatter>\n        <fr:authors />\n        <fr:addr>transcludee</fr:addr>\n        <fr:route>transcludee.xml</fr:route>\n        <fr:title text=\"I am being transcluded\">I am being transcluded</fr:title>\n      </fr:frontmatter>\n      <fr:mainmatter />\n    </fr:tree>\n  </fr:mainmatter>\n  <fr:backmatter />\n</fr:tree>\n"
      )
  in
  run
    "Transclusion configuration"
    [
      "works",
      [
        test_case "check full transclusion with default section flags" `Quick test_full_default;
        test_case "check title transclusion with default title flags" `Quick test_title_default;
        test_case "check full transclusion with metadata shown" `Quick test_full_metadata;
      ]
    ]
