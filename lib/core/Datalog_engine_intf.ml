module type S = sig
  type relation = string
  type vertex
  type term
  type literal
  type clause
  
  type var
  val var_of_string : string -> var
  
  val mk_var : var -> term
  val mk_const : vertex -> term
  val mk_literal : relation -> term list -> literal
  val mk_clause : literal -> literal list -> clause

  type db
  val db_create : unit -> db
  val db_add_fact : db -> literal -> unit
  val db_add : db -> clause -> unit

  type answers
  val list_of_answers : answers -> vertex array list
  val ask : db -> ?neg: literal list -> var array -> literal list -> answers
end
