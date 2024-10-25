(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module HDT = Forester_human_datetime

type t = HDT.datetime

let drop_time = function
  | HDT.Year (y, Some (HDT.Month (m, Some (HDT.Day (d, _))))) ->
    HDT.Year (y, Some (HDT.Month (m, Some (HDT.Day (d, None)))))
  | dt -> dt

let t =
  let of_string str = HDT.parse_string_exn str in
  let to_string dt = Format.asprintf "%a" HDT.pp_datetime dt in
  Repr.map Repr.string of_string to_string

let pp = HDT.pp_datetime
let parse = HDT.parse_string
let compare = HDT.compare_datetime

let year = function HDT.Year (y, _) -> y

let month = function
  | HDT.Year (_, Some (HDT.Month (m, _))) -> Some m
  | _ -> None

let day = function
  | HDT.Year (_, Some (HDT.Month (_, Some (HDT.Day (d, _))))) -> Some d
  | _ -> None

let now () =
  let t = Unix.gmtime (Unix.time ()) in
  let second = HDT.Second t.tm_sec in
  let minute = HDT.Minute (t.tm_min, Some second) in
  let hour = HDT.Hour (t.tm_hour, Some minute) in
  let day = HDT.Day (t.tm_mday, Some (hour, HDT.Z)) in
  let month = HDT.Month (1 + t.tm_mon, Some day) in
  HDT.Year (1900 + t.tm_year, Some month)
