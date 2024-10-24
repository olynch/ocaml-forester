%{ open Types %}

%token <char> DIGIT
%token VOID COLON HYPHEN T Z PLUS EOF

%start <datetime> datetime

%%

let void :=
| VOID; { failwith "Impossible" }

let xxxx ==
| x0 = DIGIT; x1 = DIGIT; x2 = DIGIT; x3 = DIGIT; {
    int_of_string @@ Format.sprintf "%c%c%c%c" x0 x1 x2 x3
  }
  
let xx == 
| x0 = DIGIT; x1 = DIGIT; {
    int_of_string @@ Format.sprintf "%c%c" x0 x1
  }

let second := 
| ~ = xx; <Second>

let minute(rest) :=
| ~ = xx; ~ = option(preceded(COLON, rest)); <Minute> 

let hour(rest) := 
| ~ = xx; ~ = option(preceded(COLON, rest)); <Hour>

let pm := 
| HYPHEN; {Minus}
| PLUS; {Plus}

let offset := 
| Z; {Z}
| ~ = pm; ~ = hour(minute(void)); <Offset>

let time_with_offset := 
| ~ = hour(minute(second)); ~ = offset; <> 

let day := 
| ~ = xx; ~ = option(preceded(T, time_with_offset)); <Day>

let month := 
| ~ = xx; ~ = option(preceded(HYPHEN, day)); <Month>

let year := 
| ~ = xxxx; ~ = option(preceded(HYPHEN, month)); <Year>

let datetime := 
| ~ = year; EOF; <>
