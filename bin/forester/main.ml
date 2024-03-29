open Eio.Std
open Forester
open Core
open Cmdliner

module Tty = Asai.Tty.Make (Core.Reporter.Message)

let version =
  Format.asprintf "%s" @@
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v


let build ~env input_dirs assets_dirs root base_url dev max_fibers ignore_tex_cache no_assets no_theme =
  let assets_dirs = if no_assets then [] else assets_dirs in
  let cfg = Forest.{env; root; base_url; assets_dirs; max_fibers; ignore_tex_cache; no_assets; no_theme} in
  let forest = Forest.plant_forest @@ Process.read_trees_in_dirs ~dev input_dirs in
  Forest.render_trees ~cfg ~forest

let new_tree ~env input_dir dest_dir prefix template random =
  let cfg = Forest.{env; root = None; base_url = None; assets_dirs = []; max_fibers = 20; ignore_tex_cache = true; no_assets = true; no_theme = true;} in
  let forest = Process.read_trees_in_dir ~dev:true input_dir in
  let dest_dir = Option.value ~default:input_dir dest_dir in
  let mode = if random then `Random else `Sequential in
  let addr = Forest.create_tree ~cfg ~dir:input_dir ~dest:dest_dir ~prefix ~template ~forest ~mode in
  Core.Reporter.emitf Created_tree "created tree `%s` at `%s/%s.tree`" addr dest_dir addr

let complete ~env input_dirs title =
  let forest = Forest.plant_forest @@ Process.read_trees_in_dirs ~dev:true input_dirs in
  let completions = Forest.complete ~forest title in
  completions |> Seq.iter @@ fun (addr, title) ->
  Format.printf "%s, %s\n" addr title

let build_cmd ~env =

  let arg_input_dirs =
    Arg.non_empty @@ Arg.pos_all Arg.dir [] @@
    Arg.info [] ~docv:"INPUT_DIR"
  in

  let arg_assets_dirs =
    let doc = "The contents of the $(i,ASSET_DIR) directories will be copied into the rendered forest." in
    Arg.value @@ Arg.opt (Arg.list Arg.dir) ["assets"] @@
    Arg.info ["assets-dirs"] ~docv:"ASSET_DIR" ~doc
  in

  let arg_root =
    let doc = "The address of the root tree, e.g. $(i,jms-0001); if this option is supplied, the tree $(i,jms-0001) will be rendered to $(i,output/index.xml)." in
    Arg.value @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["root"] ~docv:"ADDR" ~doc
  in

  let arg_dev =
    let doc = "Run forester in development mode; this will attach source file locations to the generated json." in
    Arg.value @@ Arg.flag @@ Arg.info ["dev"] ~doc
  in

  let arg_base_url =
    let doc = "Set the base URL for local hyperlinks." in
    Arg.value @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["base-url"] ~docv:"URL" ~doc
  in

  let arg_max_fibers =
    let doc = "Maximum number of fibers with which to build LaTeX assets concurrently." in
    Arg.value @@ Arg.opt Arg.int 20 @@
    Arg.info ["max-fibers"] ~docv:"NUM" ~doc
  in

  let arg_ignore_tex_cache =
    let doc = "Ignore the SVG/PDF cache when building LaTeX assets." in
    Arg.value @@ Arg.flag @@ Arg.info ["ignore-tex-cache"] ~doc
  in

  let arg_no_assets =
    let doc = "Build without copying the asset directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-assets"] ~doc
  in

  let arg_no_theme =
    let doc = "Build without copying the theme directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-theme"] ~doc
  in

  let doc = "Build the forest" in
  let man = [
    `S Manpage.s_description;
    `P "The $(tname) command builds a hypertext $(b,forest) from trees stored in each $(i,INPUT_DIR) or any of its subdirectories; tree files are expected to be of the form $(i,addr.tree) where $(i,addr) is the global address of the tree. Note that the physical location of a tree is not taken into account, and two trees with the same address are not permitted.";
  ]
  in
  let info = Cmd.info "build" ~version ~doc ~man in
  Cmd.v info
    Term
    .(const (build ~env)
      $ arg_input_dirs
      $ arg_assets_dirs
      $ arg_root
      $ arg_base_url
      $ arg_dev
      $ arg_max_fibers
      $ arg_ignore_tex_cache
      $ arg_no_assets
      $ arg_no_theme)

let new_tree_cmd ~env =
  let arg_prefix =
    let doc = "The namespace prefix for the created tree." in
    Arg.required @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["prefix"] ~docv:"XXX" ~doc
  in
  let arg_template =
    let doc = "The tree to use as a template" in
    Arg.value @@ Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["template"] ~docv:"XXX" ~doc
  in
  let arg_input_dir =
    let doc = "The directory in which to scan tree identifiers." in
    Arg.value @@ Arg.opt Arg.file "." @@
    Arg.info ["dir"] ~docv:"DIR" ~doc
  in
  let arg_dest_dir =
    let doc = "The directory in which to deposit created tree." in
    Arg.value @@ Arg.opt (Arg.some Arg.file) None @@
    Arg.info ["dest"] ~docv:"DEST" ~doc
  in
  let arg_random =
    let doc = "True if the new tree should have id assigned randomly rather than sequentially" in
    Arg.value @@ Arg.flag @@ Arg.info ["random"] ~doc
  in
  let doc = "Create a new tree." in
  let info = Cmd.info "new" ~version ~doc in
  Cmd.v info
    Term
    .(const (new_tree ~env)
      $ arg_input_dir
      $ arg_dest_dir
      $ arg_prefix
      $ arg_template
      $ arg_random)

let complete_cmd ~env =
  let arg_title =
    let doc = "The tree title prefix to complete." in
    Arg.value @@ Arg.opt Arg.string "" @@
    Arg.info ["title"] ~docv:"title" ~doc
  in
  let arg_input_dirs =
    Arg.non_empty @@ Arg.pos_all Arg.file [] @@
    Arg.info [] ~docv:"INPUT_DIR"
  in
  let doc = "Complete a tree title." in
  let info = Cmd.info "complete" ~version ~doc in
  Cmd.v info Term.(const (complete ~env) $ arg_input_dirs $ arg_title)

let cmd ~env =
  let doc = "a tool for tending mathematical forests" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <~jonsterling/forester-discuss@lists.sr.ht>." ;
    `S Manpage.s_authors;
    `P "Jonathan Sterling"
  ]
  in

  let info = Cmd.info "forester" ~version ~doc ~man in
  Cmd.group info [build_cmd ~env; new_tree_cmd ~env; complete_cmd ~env]

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Printexc.record_backtrace true;
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  exit @@ Cmd.eval ~catch:false @@ cmd ~env
