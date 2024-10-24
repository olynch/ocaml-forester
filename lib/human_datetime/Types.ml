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

let init_ptime_time_state () =
  { hour = 0; minute = 0; second = 0 }

let init_ptime_date_state () =
  { year = 0; month = 1; day = 1 }

let init_ptime_date_time_state () =
  {
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
