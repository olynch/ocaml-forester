(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Types

type ptime_time_state = {
  mutable hour: int;
  mutable minute: int;
  mutable second: int
}

type ptime_date_state = {
  mutable year: int;
  mutable month: int;
  mutable day: int
}

let ptime_time_state_to_seconds state =
  state.second + 60 * (state.minute + 60 * state.hour)

type ptime_date_time_state = {
  date: ptime_date_state;
  time: ptime_time_state;
  mutable tz_offset_s: int
}

let init_ptime_time_state () = {hour = 0; minute = 0; second = 0}

let init_ptime_date_state () = {year = 0; month = 1; day = 1}

let init_ptime_date_time_state () = {
  date = init_ptime_date_state ();
  time = init_ptime_time_state ();
  tz_offset_s = 0
}

let to_ptime datetime =
  let go_void (x : void) = match x with _ -> . in
  let go_second state (Second s) =
    state.second <- s
  in
  let go_minute state rest (Minute (m, rest_opt)) =
    state.minute <- m;
    Option.iter rest rest_opt
  in
  let go_hour state rest (Hour (h, rest_opt)) =
    state.hour <- h;
    Option.iter rest rest_opt
  in
  let go_offset state = function
    | Z -> state.tz_offset_s <- 0
    | Offset (pm, time) ->
      let time_state = init_ptime_time_state () in
      go_hour time_state (go_minute time_state go_void) time;
      let offset = ptime_time_state_to_seconds time_state in
      let sign =
        match pm with
        | Plus -> 1
        | Minus -> -1
      in
      state.tz_offset_s <- sign * offset
  in
  let go_time_with_offset state ((time, offset) : time_with_offset) =
    go_hour state.time (go_minute state.time (go_second state.time)) time;
    go_offset state offset
  in
  let go_day state (Day (d, time_with_offset_opt)) =
    state.date.day <- d;
    Option.iter (go_time_with_offset state) time_with_offset_opt
  in
  let go_month state (Month (m, day_opt)) =
    state.date.month <- m;
    Option.iter (go_day state) day_opt
  in
  let go_year state (Year (y, month_opt)) =
    state.date.year <- y;
    Option.iter (go_month state) month_opt
  in
  let state = init_ptime_date_time_state () in
  go_year state datetime;
  let date = state.date.year, state.date.month, state.date.day in
  let time = ((state.time.hour, state.time.minute, state.time.second), state.tz_offset_s) in
  Ptime.of_date_time (date, time)
