open Forester_core

val parse_file : string -> (Code.t, Code.t * Reporter.Message.t Asai.Diagnostic.t list) result

val parse_string : string -> (Code.t, Code.t * Reporter.Message.t Asai.Diagnostic.t list) result
