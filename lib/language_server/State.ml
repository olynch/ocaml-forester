(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_compiler
open Forester_frontend

module Graph = struct
  module Uri_vertex = struct
    type t = Lsp.Uri.t
    let compare s t = String.compare (Lsp.Uri.to_string s) (Lsp.Uri.to_string t)
    let hash = Hashtbl.hash
    let equal s t = String.equal (Lsp.Uri.to_string s) (Lsp.Uri.to_string t)
  end

  module Gph = Graph.Imperative.Digraph.Concrete(Uri_vertex)
  module Topo = Graph.Topological.Make(Gph)
  module Op = Graph.Oper.I(Gph)
  let topo_fold = Topo.fold

  include Gph
end

type server = {
  env: Forest_reader.env;
  units: Expand.Env.t;
  config: Forester_frontend.Config.Forest_config.t;
  lsp_io: LspEio.io;
  should_shutdown: bool;
  did_initial_build: bool;
  import_graph: Graph.t;
  index: Index.t;
}

include Algaeff.State.Make(struct type t = server end)
