(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type void = |

type second = Second of int
type 'a minute = Minute of int * 'a option
type 'a hour = Hour of int * 'a option

type pm = Plus | Minus
type offset = Z | Offset of pm * void minute hour

type time_with_offset = second minute hour * offset

type day = Day of int * time_with_offset option
type month = Month of int * day option
type year = Year of int * month option

type datetime = year

let pp_void _fmt (x : void) =
  match x with
  | _ -> .

let pp_second fmt (Second s) =
  Format.fprintf fmt "%02d" s

let pp_minute pp_rest fmt (Minute (m, rest_opt)) =
  Format.fprintf fmt "%02d" m;
  match rest_opt with
  | None -> ()
  | Some rest ->
    Format.fprintf fmt ":%a" pp_rest rest

let pp_hour pp_rest fmt (Hour (h, rest_opt)) =
  Format.fprintf fmt "%02d" h;
  match rest_opt with
  | None -> ()
  | Some rest ->
    Format.fprintf fmt ":%a" pp_rest rest

let pp_pm fmt = function
  | Plus -> Format.fprintf fmt "+"
  | Minus -> Format.fprintf fmt "-"

let pp_offset fmt = function
  | Z -> Format.fprintf fmt "Z"
  | Offset (pm, hm) ->
    pp_pm fmt pm;
    pp_hour (pp_minute pp_void) fmt hm

let pp_time_with_offset fmt (hms, offset) =
  pp_hour (pp_minute pp_second) fmt hms;
  pp_offset fmt offset

let pp_day fmt (Day (d, time_with_offset_opt)) =
  Format.fprintf fmt "%02d" d;
  match time_with_offset_opt with
  | None -> ()
  | Some time_with_offset ->
    Format.fprintf fmt "T%a" pp_time_with_offset time_with_offset

let pp_month fmt (Month (m, day_opt)) =
  Format.fprintf fmt "%02d" m;
  match day_opt with
  | None -> ()
  | Some day ->
    Format.fprintf fmt "-%a" pp_day day

let pp_year fmt (Year (y, month_opt)) =
  Format.fprintf fmt "%04d" y;
  match month_opt with
  | None -> ()
  | Some month ->
    Format.fprintf fmt "-%a" pp_month month

let pp_datetime = pp_year
