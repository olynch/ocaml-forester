module type S = sig
  val run : tape:Syn.t -> (unit -> 'a) -> 'a
  val pop_node_opt : unit -> Syn.node Range.located option

  val pop_arg_opt : unit -> Syn.t Range.located option
  val pop_arg : loc:Range.t option -> Syn.t Range.located
  val pop_args : unit -> Syn.t Range.located list
end


module Make () : S
