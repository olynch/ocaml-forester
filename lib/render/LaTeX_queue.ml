open Forester_core

module type S =
sig
  val enqueue : string -> string
  val process : env:_ Build_latex.env -> ignore_tex_cache : bool -> unit
end

module Make () : S =
struct
  let svg_queue : (string, string) Hashtbl.t = Hashtbl.create 100

  let enqueue source =
    let hash = Digest.to_hex @@ Digest.string source in
    let name = hash ^ ".svg" in
    if not @@ Hashtbl.mem svg_queue name then
      Hashtbl.add svg_queue name source;
    name

  let process ~env ~ignore_tex_cache : unit  =
    let task (name, source) =
      Reporter.easy_run @@ fun () ->
      Build_latex.build_latex ~ignore_tex_cache ~env ~name ~source
    in
    Hashtbl.to_seq svg_queue
    |> List.of_seq
    |> Eio.Fiber.List.iter ~max_fibers:20 task
end
