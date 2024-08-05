module type S =
sig
  val enqueue : name:string -> preamble:string -> source:string -> unit
  val process : env:_ Build_latex.env -> ignore_tex_cache : bool -> unit
end

module Make () : S
