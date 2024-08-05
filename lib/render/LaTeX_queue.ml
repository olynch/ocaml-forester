open Forester_core

module type S =
sig
  val enqueue : preamble:string -> source:string -> string
  val process : env:_ Build_latex.env -> ignore_tex_cache : bool -> unit
end

module Make () : S =
struct
  let svg_queue : (string, string * string) Hashtbl.t = Hashtbl.create 100

  let enqueue ~preamble ~source =
    let hash = Digest.to_hex @@ Digest.string @@ preamble ^ source in
    let name = hash ^ ".svg" in
    if not @@ Hashtbl.mem svg_queue name then
      Hashtbl.add svg_queue name (preamble, source);
    name

  let process ~env ~ignore_tex_cache : unit  =
    let task (name, (preamble, source)) =
      Reporter.easy_run @@ fun () ->
      Build_latex.build_latex ~ignore_tex_cache ~env ~name ~source ~preamble
    in
    Hashtbl.to_seq svg_queue
    |> List.of_seq
    |> Eio.Fiber.List.iter ~max_fibers:20 task
end
