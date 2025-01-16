(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_frontend
open Forester_compiler
open Cmdliner

module EP = Eio.Path

let setup_logs style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let verbosity =
  let env = Cmd.Env.info "PART_LOGS" in
  Logs_cli.level ~env ~docs: Manpage.s_common_options ()

let renderer =
  let env = Cmd.Env.info "PART_FMT" in
  Fmt_cli.style_renderer ~docs: Manpage.s_common_options ~env ()

let arg_logs = Term.(const setup_logs $ renderer $ verbosity)

let version =
  Format.asprintf "%s" @@
    match Build_info.V1.version () with
    | None -> "n/a"
    | Some v -> Build_info.V1.Version.to_string v

let build ~env _ config_filename dev no_theme =
  let config = Config_parser.parse_forest_config_file config_filename in
  Logs.debug (fun m -> m "Parsed config file %s" config_filename);
  let forest = Forester.compile ~env ~dev ~config in
  forest
  |> State.diagnostics
  |> Diagnostic_store.iter (fun _ d -> List.iter Reporter.Tty.display d);
  Forester.render_forest ~dev ~forest;
  let dirs_to_copy = (if not no_theme then [config.theme] else []) in
  let@ dir_to_copy = List.iter @~ dirs_to_copy in
  Forester.copy_contents_of_dir ~env @@ Eio_util.path_of_dir ~env dir_to_copy

let export ~env _ config_filename dev =
  let config = Config_parser.parse_forest_config_file config_filename in
  Logs.debug (fun m -> m "Parsed config file %s" config_filename);
  let forest = Forester.compile ~env ~dev ~config in
  forest
  |> State.diagnostics
  |> Diagnostic_store.iter (fun _ d -> List.iter Reporter.Tty.display d);
  Forester.export ~forest

let new_tree ~env config_filename prefix template random =
  let@ () = Reporter.silence in
  let config = Config_parser.parse_forest_config_file config_filename in
  let forest = Forester.compile ~env ~dev: true ~config in
  let mode = if random then `Random else `Sequential in
  let new_tree = Forester.create_tree ~env ~prefix ~template ~mode ~config ~forest in
  Format.printf "%s.tree\n" new_tree

let complete ~env config_filename title =
  let@ () = Reporter.silence in
  let config = Config_parser.parse_forest_config_file config_filename in
  let forest = Forester.compile ~env ~dev: true ~config in
  let@ iri, title = Seq.iter @~ Forester.complete ~forest title in
  Format.printf "%a, %s\n" pp_iri iri title

let query_all ~env config_filename =
  let@ () = Reporter.silence in
  let config = Config_parser.parse_forest_config_file config_filename in
  let forest = Forester.compile ~env ~config ~dev: true in
  Format.printf "%s" @@
    Forester.json_manifest ~dev: true ~forest

let default_config_str =
  {|[forest]
trees = ["trees" ]  # The directories in which your trees are stored
assets = ["assets"] # The directories in which your assets are stored
theme = "theme"     # The directory in which your theme is stored
home = "index"
host = "CHANGEME"
|}

let index_tree_str =
  {|\title{Hello, World!}
\p{Welcome to your first tree! This tree is the root of your forest.}
\ul{
  \li{[Build and view your forest for the first time](http://www.jonmsterling.com/jms-007D.xml)}
  \li{[Overview of the Forester markup language](http://www.jonmsterling.com/jms-007N.xml)}
  \li{[Creating new trees](http://www.jonmsterling.com/jms-007H.xml)}
  \li{[Creating your personal biographical tree](http://www.jonmsterling.com/jms-007K.xml)}
}
|}

let try_create_dir ~cwd dname =
  let ( / ) = EP.( / ) in
  if Eio.Path.is_directory (cwd / dname) then
    Reporter.emitf
      Initialization_warning
      "`%s` already exists"
      dname
  else
    try
      Eio.Path.mkdir ~perm: 0o755 (cwd / dname)
    with
      | exn ->
        Forester_core.Reporter.emitf Initialization_warning "Failed to create directory `%s`: %a" dname Eio.Exn.pp exn

let try_create_file ~cwd ?(content = "") fname =
  let ( / ) = EP.( / ) in
  if Eio.Path.is_file (cwd / fname) then
    Forester_core.Reporter.emitf Initialization_warning "`%s` already exists" fname
  else
    try
      Eio.Path.save ~create: (`Exclusive 0o644) (cwd / fname) content
    with
      | exn ->
        Forester_core.Reporter.emitf Initialization_warning "Failed to create file `%s`: %a" fname Eio.Exn.pp exn

let init ~env dir =
  let default_theme_url = "https://git.sr.ht/~jonsterling/forester-base-theme" in
  let theme_version = "4.3.0" in
  let cwd =
    match dir with
    | None -> Eio.Stdenv.cwd env
    | Some d ->
      begin
        try
          EP.mkdir ~perm: 0o755 EP.(Eio.Stdenv.cwd env / d)
        with
          | _ ->
            Reporter.emitf Initialization_warning "Directory `%s` already exists" d
      end;
      EP.((Eio.Stdenv.cwd env) / d)
  in
  begin
    try
      let proc_mgr = Eio.Stdenv.process_mgr env in
      let@ cmd =
        List.iter @~
          [
            ["git"; "init"];
            ["git"; "branch"; "-m"; "main"];
            ["git"; "submodule"; "add"; default_theme_url; "theme"];
            ["git"; "-C"; "theme"; "checkout"; theme_version];
          ]
      in
      Eio.Process.run ~cwd proc_mgr cmd
    with
      | exn ->
        Reporter.fatalf
          Configuration_error
          {|
Failed to set up theme: %a. To perform this step manually, run the commands

git init
git submodule add %s
git -C theme checkout %s
        |}
          Eio.Exn.pp
          exn
          default_theme_url
          theme_version
  end;
  ["trees"; "assets"] |> List.iter (try_create_dir ~cwd);
  try_create_file ~cwd ~content: default_config_str "forest.toml";
  try_create_file ~cwd ~content: "output/" ".gitignore";
  try_create_file ~cwd ~content: "" "assets/.gitkeep";
  try_create_file ~cwd ~content: index_tree_str "trees/index.tree";
  Reporter.emitf Log "%s" "Initialized forest, try editing `trees/index.tree` and running `forester build`. Afterwards, you can open `output/index.xml` in your browser to view your forest."

let arg_config =
  let doc = "A TOML file like $(i,forest.toml)" in
  Arg.(value & pos 0 file "forest.toml" & info [] ~docv: "FOREST" ~doc)

let build_cmd ~env =
  let arg_dev =
    let doc = "Run forester in development mode; this will attach source file locations to the generated json." in
    Arg.value @@ Arg.flag @@ Arg.info ["dev"] ~doc
  in
  let arg_no_theme =
    let doc = "Build without copying the theme directory" in
    Arg.value @@ Arg.flag @@ Arg.info ["no-theme"] ~doc
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
      $ arg_logs
      $ arg_config
      $ arg_dev
      $ arg_no_theme
    )

let export_cmd ~env =
  let arg_dev =
    let doc = "Run forester in development mode; this will attach source file locations to the generated json." in
    Arg.value @@ Arg.flag @@ Arg.info ["dev"] ~doc
  in
  let doc = "Export the forest" in
  let man =
    [
    ]
  in
  let info = Cmd.info "export" ~version ~doc ~man in
  Cmd.v
    info
    Term.(
      const (export ~env)
      $ arg_logs
      $ arg_config
      $ arg_dev
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
  let arg_dir =
    let doc = "The directory in which to initialize the forest" in
    Arg.value @@ Arg.opt (Arg.some Arg.string) None @@ Arg.info ["dir"] ~docv: "DIR" ~doc
  in
  let doc = "Initialize a new forest" in
  let man =
    [
      `S Manpage.s_description;
      `P "The $(tname) command initializes a $(b,forest) in the current directory. This involves initialising a git repository, setting up a git submodule for the theme, creating an assets and trees directory, as well as a config file."
    ]
  in
  let info = Cmd.info "init" ~version ~doc ~man in
  Cmd.v info Term.(const (init ~env) $ arg_dir)

let lsp ~env _ config =
  let config = Config_parser.parse_forest_config_file config in
  Forester_lsp.start
    ~env
    ~config

let lsp_cmd ~env =
  let man =
    [
      `S Manpage.s_description;
      `P "The $(tname) command starts the forester language server.";
    ]
  in
  let doc = "Start the LSP" in
  let info = Cmd.info "lsp" ~version ~doc ~man in
  Cmd.v
    info
    Term.(
      const (lsp ~env)
      $ arg_logs
      $ arg_config
    )

let render ~env target addr config =
  let config = Config_parser.parse_forest_config_file config in
  Forester_frontend.Forester.render_tree ~env ~target ~config addr

let render_cmd ~env =
  let open Cmdliner in
  let man =
    [
      `S Manpage.s_description;
      `P "The $(tname) renders a tree in the specified format.";
    ]
  in
  let arg_addr =
    let doc = "the tree to render" in
    Arg.(
      required @@
      opt (some string) None @@
      info ["addr"] ~docv: "XXX" ~doc
    )
  in
  let arg_format =
    Arg.(
      value
      & opt
        (
          Arg.enum
            [
              "html", Forester.Target HTML;
              "json", Forester.Target JSON;
              "xml", Forester.Target XML;
              "string", Forester.Target STRING
            ]
        )
        ~vopt: (Forester.Target HTML)
        (Target HTML)
      & info ["format"]
    )
  in
  let doc
    =
    "Render a tree"
  in
  let info = Cmd.info "render" ~version ~doc ~man in
  Cmd.v
    info
    Term.(
      const (render ~env)
      $ arg_format
      $ arg_addr
      $ arg_config
    )

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
  Cmd.group
    info
    [
      build_cmd ~env;
      export_cmd ~env;
      new_tree_cmd ~env;
      complete_cmd ~env;
      init_cmd ~env;
      query_cmd ~env;
      render_cmd ~env;
      lsp_cmd ~env;
    ]

let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  Logs.set_reporter (Logs_fmt.reporter ());
  let@ env = Eio_main.run in
  let@ () = Forester_core.Reporter.easy_run in
  exit @@ Cmd.eval ~catch: false @@ cmd ~env
