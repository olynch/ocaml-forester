module G : Forester_forest.Forest_graphs.S
module F : Forester_forest.Forest.S
module L = Lsp.Types

val check : Base.server -> L.DocumentUri.t -> unit


val build_once :
  env:Forester_frontend.Forest_reader.env ->
  Base.server -> unit -> unit

val extract_addr :
  Forester_compiler.Code.node Forester_core.Range.located ->
  string option

val addr_at :
  position:Lsp.Types.Position.t ->
  Forester_compiler.Code.t ->
  string option

