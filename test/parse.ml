(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

let emit _ = () (* ignore *)

let fatal _ = exit 1

let _ =
  Reporter.run ~emit ~fatal @@
    fun () ->
      let good =
        Result.get_ok @@
          Parse.parse_string
            {|
    \title{Good}
    \taxon{Test}
    \author{Testy}

    \p{
      This should parse correctly.
    }
    |}
      in
      Format.printf "parse_good_result:\n%s\n\n" (Code.show good)

