(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type xml_qname = {prefix: string; uname: string; xmlns: string option}
[@@deriving show, repr]

let split_xml_qname str =
  match String.split_on_char ':' str with
  | [prefix; uname] -> Some prefix, uname
  | [uname] -> None, uname
  | _ -> failwith "split_xml_qname"
