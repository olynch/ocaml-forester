open Eio.Std
open Forester_prelude
open Forester_core
open Forester_frontend
open Cmdliner

module EP = Eio.Path

let path_of_dir ~env dir =
  EP.(Eio.Stdenv.fs env / dir)

let paths_of_dirs ~env =
  List.map (path_of_dir ~env)

let version =
  Format.asprintf "%s" @@
    match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v

let build ~env config_filename dev render_only no_assets no_theme =
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  Forester.plant_forest_from_dirs ~env ~host: config.host ~dev @@ paths_of_dirs ~env config.trees;
  Forester.render_forest ~env ~dev ~host: config.host ~root: config.root ~stylesheet: config.stylesheet;
  let dirs_to_copy =
    (if not no_theme then [config.theme] else []) @
      (if not no_assets then config.assets else [])
  in
  let@ dir_to_copy = List.iter @~ dirs_to_copy in
  Forester.copy_contents_of_dir ~env @@ path_of_dir ~env dir_to_copy

let new_tree ~env config_filename dest_dir prefix template random =
  let@ () = Reporter.silence in
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  Forester.plant_forest_from_dirs ~env ~host: config.host ~dev: true @@ paths_of_dirs ~env config.trees;
  let mode = if random then `Random else `Sequential in
  let dest = path_of_dir ~env dest_dir in
  let addr = Forester.create_tree ~env ~dest ~prefix ~template ~mode in
  Format.printf "%s/%s.tree\n" dest_dir addr

let complete ~env config_filename title =
  let@ () = Reporter.silence in
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  Forester.plant_forest_from_dirs ~env ~host: config.host ~dev: true @@ paths_of_dirs ~env config.trees;
  let@ iri, title = Seq.iter @~ Forester.complete ~host: config.host title in
  Format.printf "%a, %s\n" pp_iri iri title

let query_all ~env config_filename =
  let@ () = Reporter.silence in
  let config = Forester_frontend.Config.parse_forest_config_file config_filename in
  Forester.plant_forest_from_dirs ~env ~host: config.host ~dev: true @@ paths_of_dirs ~env config.trees;
  Forester.json_manifest ~host: config.host ~root: config.root ~dev: true |> Format.printf "%s"

let init ~env () =
  let default_theme_url = "https://git.sr.ht/~jonsterling/forester-base-theme" in
  let theme_version = "4.3.0" in
  let fs = Eio.Stdenv.fs env in
  let try_create_dir name =
    try
      EP.mkdir ~perm: 0o755 EP.(fs / name)
    with
      | _ ->
        Reporter.emitf Initialization_warning "Directory `%s` already exists" name
  in
  let default_config_str =
    (* More convenient to just write this string instead of constructing it with the toml library*)
    {|[forest]
trees = ["trees" ]                   # The directories in which your trees are stored
assets = ["assets"]                  # The directories in which your assets are stored
theme = "theme"                      # The directory in which your theme is stored
|}
  in
  let index_tree_str =
    {|
\title{Hello, World!}
\p{
  Welcome to your first tree! This tree is the root of your forest.
  \ul{
    \li{[Build and view your forest for the first time](http://www.jonmsterling.com/jms-007D.xml)}
    \li{[Overview of the Forester markup language](http://www.jonmsterling.com/jms-007N.xml)}
    \li{[Creating new trees](http://www.jonmsterling.com/jms-007H.xml)}
    \li{[Creating your personal biographical tree](http://www.jonmsterling.com/jms-007K.xml)}
  }
}
|}
  in
  begin
    if EP.is_file EP.(Eio.Stdenv.fs env / "forest.toml") then
      Reporter.emitf Initialization_warning "forest.toml already exists"
    else
      EP.(save ~create: (`Exclusive 0o644) (fs / "forest.toml") default_config_str)
  end;
  EP.(save ~create: (`Exclusive 0o644) (fs / ".gitignore") {|output/|});
  begin
    try
      let shut_up = Eio_util.null_sink () in
      let@ cmd =
        List.iter @~
          [
            ["git"; "init"];
            ["git"; "branch"; "-m"; "main"];
            ["git"; "submodule"; "add"; default_theme_url; "theme"];
            ["git"; "-C"; "theme"; "checkout"; theme_version];
          ]
      in
      Eio.Process.run (Eio.Stdenv.process_mgr env) ~stdout: shut_up ~stderr: shut_up cmd
    with
      | _ ->
        Reporter.fatalf
          Configuration_error
          {|Failed to set up theme. To perform this step manually, run the commands

   git init
   git submodule add %s
   git -C theme checkout %s|}
          default_theme_url
          theme_version
  end;
  ["trees"; "assets"] |> List.iter try_create_dir;
  begin
    try
      EP.(save ~create: (`Exclusive 0o644) (fs / "trees" / "index.tree") index_tree_str)
    with
      | _ ->
        let@ () = Reporter.with_backtrace Emp in
        Reporter.emitf Initialization_warning "`index.tree` already exists"
  end;
  build ~env "forest.toml" true None false false;
  Format.printf "%s" "Initialized forest, try editing `trees/index.tree` and running `forester build`. Afterwards, you can open `output/index.xml` in your browser to view your forest.\n"

let arg_config =
  let doc = "A TOML file like $(i,forest.toml)" in
  Arg.(value & pos 0 file "forest.toml" & info [] ~docv: "FOREST" ~doc)

let build_cmd ~env =
  let arg_dev =
    let doc = "Run forester in development mode; this will attach source file locations to the generated json." in
    Arg.value @@ Arg.flag @@ Arg.info ["dev"] ~doc
  in
  let arg_no_assets =
    let doc = "Build without copying the asset directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-assets"] ~doc
  in
  let arg_no_theme =
    let doc = "Build without copying the theme directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-theme"] ~doc
  in
  let arg_render_only =
    let doc = "Builds the entire forest but renders only the specified comma-separated list of trees" in
    Arg.value @@ Arg.opt (Arg.some' (Arg.list Arg.string)) None @@ Arg.info ["render-only"] ~docv: "TREES" ~doc
  in
  let doc = "Build the forest" in
  let man =
    [
      `S Manpage.s_description;
      `P "The $(tname) command builds a hypertext $(b,forest) from trees stored in each $(i,INPUT_DIR) or any of its subdirectories; tree files are expected to be of the form $(i,addr.tree) where $(i,addr) is the address of the tree. Note that the physical location of a tree is not taken into account, and two trees with the same address are not permitted.";
    ]
  in
  let info = Cmd.info "build" ~version ~doc ~man in
  Cmd.v
    info
    Term.(
      const (build ~env)
      $ arg_config
      $ arg_dev
      $ arg_render_only
      $ arg_no_assets
      $ arg_no_theme
    )

let new_tree_cmd ~env =
  let arg_prefix =
    let doc = "The namespace prefix for the created tree." in
    Arg.required @@
    Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["prefix"] ~docv: "XXX" ~doc
  in
  let arg_template =
    let doc = "The tree to use as a template" in
    Arg.value @@
    Arg.opt (Arg.some Arg.string) None @@
    Arg.info ["template"] ~docv: "XXX" ~doc
  in
  let arg_dest_dir =
    let doc = "The directory in which to deposit created tree." in
    Arg.required @@
    Arg.opt (Arg.some Arg.dir) None @@
    Arg.info ["dest"] ~docv: "DEST" ~doc
  in
  let arg_random =
    let doc = "True if the new tree should have id assigned randomly rather than sequentially" in
    Arg.value @@ Arg.flag @@ Arg.info ["random"] ~doc
  in
  let doc = "Create a new tree." in
  let info = Cmd.info "new" ~version ~doc in
  Cmd.v
    info
    Term.(
      const (new_tree ~env)
      $ arg_config
      $ arg_dest_dir
      $ arg_prefix
      $ arg_template
      $ arg_random
    )

let complete_cmd ~env =
  let arg_title =
    let doc = "The tree title prefix to complete." in
    Arg.value @@
    Arg.opt Arg.string "" @@
    Arg.info ["title"] ~docv: "title" ~doc
  in
  let doc = "Complete a tree title." in
  let info = Cmd.info "complete" ~version ~doc in
  Cmd.v info Term.(const (complete ~env) $ arg_config $ arg_title)

let query_all_cmd ~env =
  let doc = "List all trees in JSON format" in
  let info = Cmd.info "all" ~version ~doc in
  Cmd.v info Term.(const (query_all ~env) $ arg_config)

let query_cmd ~env =
  let doc = "Query your forest" in
  let info = Cmd.info "query" ~version ~doc in
  Cmd.group info [query_all_cmd ~env]

let init_cmd ~env =
  let doc = "Initialize a new forest" in
  let man =
    [
      `S Manpage.s_description;
      `P "The $(tname) command initializes a $(b,forest) in the current directory. This involves initialising a git repository, setting up a git submodule for the theme, creating an assets and trees directory, as well as a config file."
    ]
  in
  let info = Cmd.info "init" ~version ~doc ~man in
  Cmd.v info Term.(const (init ~env) $ const ())

let cmd ~env =
  let doc = "a tool for tending mathematical forests" in
  let man =
    [
      `S Manpage.s_bugs;
      `P "Email bug reports to <~jonsterling/forester-discuss@lists.sr.ht>.";
      `S Manpage.s_authors;
      `P "Jonathan Sterling"
    ]
  in
  let info = Cmd.info "forester" ~version ~doc ~man in
  Cmd.group info [build_cmd ~env; new_tree_cmd ~env; complete_cmd ~env; init_cmd ~env; query_cmd ~env]

let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  let@ env = Eio_main.run in
  let@ () = Forester_core.Reporter.easy_run in
  exit @@ Cmd.eval ~catch: false @@ cmd ~env
