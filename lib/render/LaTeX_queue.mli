module type S =
sig
  val enqueue : preamble:string -> source:string -> string
  val process : env:_ Build_latex.env -> ignore_tex_cache : bool -> unit
end

module Make () : S
