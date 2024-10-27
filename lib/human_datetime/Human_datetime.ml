(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

include Types
open Conversion

type t = datetime

let year = function Year (y, _) -> y

let month = function
  | Year (_, Some (Month (m, _))) -> Some m
  | _ -> None

let day = function
  | Year (_, Some (Month (_, Some (Day (d, _))))) -> Some d
  | _ -> None

let drop_time = function
  | Year (y, Some (Month (m, Some (Day (d, _))))) ->
    Year (y, Some (Month (m, Some (Day (d, None)))))
  | dt -> dt

let pp = pp_datetime

let compare dt0 dt1 =
  match to_ptime dt0, to_ptime dt1 with
  | Some x0, Some x1 -> Ptime.compare x0 x1
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1

let parse lexbuf =
  match Grammar.datetime Lexer.token lexbuf with
  | datetime -> Some datetime
  | exception Grammar.Error ->
    None

let parse_string str =
  let lexbuf = Lexing.from_string str in
  parse lexbuf

let parse_string_exn str =
  match parse_string str with
  | None -> failwith "human datetime: parse error"
  | Some dt -> dt

let t =
  let of_string str = parse_string_exn str in
  let to_string dt = Format.asprintf "%a" pp dt in
  Repr.map Repr.string of_string to_string

let now () =
  let t = Unix.gmtime (Unix.time ()) in
  let second = Second t.tm_sec in
  let minute = Minute (t.tm_min, Some second) in
  let hour = Hour (t.tm_hour, Some minute) in
  let day = Day (t.tm_mday, Some (hour, Z)) in
  let month = Month (1 + t.tm_mon, Some day) in
  Year (1900 + t.tm_year, Some month)
