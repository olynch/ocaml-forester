(* open Forester_compiler *)
open Brr
open Brr_io
open Note
open Note_brr

open Forester_parser

open Forester_core
module T = Types

module Editor = struct
  type t = { source: Jstr.t; }

  let empty = { source = Jstr.v ""; }

  let of_json j =
    {
      source = Jv.Jstr.get j "source";
    }

  let to_json t =
    Jv.(
      obj
        [|"source", of_jstr t.source; |]
    )
  let _source t = t.source
end

type action =
  Source_changed of Jstr.t

let show_action : action -> string = function
  | Source_changed jstr -> "source_changed:" ^ Jstr.to_string jstr

let state_key = Jstr.v "tree-editor-state"

let load_state () =
  let s = Storage.local G.window in
  match Storage.get_item s state_key with
  | None -> Editor.empty
  | Some j ->
    (Result.map Editor.of_json (Json.decode j))
    |> Console.log_if_error ~use: Editor.empty

let save_state editor =
  let s = Storage.local G.window in
  (Storage.set_item s state_key (Json.encode (Editor.to_json editor)))
  |> Console.log_if_error ~use: ()

let do_action
    : action -> Editor.t -> Editor.t
  = fun action editor ->
    Console.log [Jstr.v @@ show_action action];
    match action with
    | Source_changed _ -> editor

let text_field
    : unit -> action event * El.t
  = fun () ->
    let at =
      At.[
        type' (Jstr.v "text");
        class' (Jstr.v "editor-textfield");
        autofocus;
      ]
    in
    let _keydown = Ev.keydown in
    let i = El.textarea ~at [] in
    let change_events =
      Evr.on_el
        Ev.input
        (fun _ -> Source_changed (El.prop El.Prop.value i))
        i
    in
    change_events, i

let _test_article =
  let frontmatter = T.default_frontmatter () in
  let mainmatter = T.Content [T.Text "Hello World"] in
  let backmatter = T.Content [] in
  T.{ frontmatter; mainmatter; backmatter }

let render_path : Trie.path -> El.t = fun p -> El.span @@ List.map El.txt' p

let rec render_code_node = function
  | Code.Text s -> El.txt' s
  | Verbatim s -> El.txt' @@ "verbatim: " ^ s
  | Group (d, code) ->
    let o, c = delim_to_strings d in
    El.span [El.txt' o; El.section @@ render_code code; El.txt' c]
  | Math (_, _) -> El.txt' "math"
  | Ident p -> El.span [El.txt' "\\"; render_path p]
  | Hash_ident _ -> El.txt' "hashident"
  | Xml_ident (_, _) -> El.txt' "xml"
  | Subtree (_, _) -> El.txt' "subtree"
  | Let (_, _, _) -> El.txt' "let"
  | Open _ -> El.txt' "open"
  | Scope _ -> El.txt' "scope"
  | Put (_, _) -> El.txt' "put"
  | Default (_, _) -> El.txt' "default"
  | Get _ -> El.txt' "get"
  | Fun (_, _) -> El.txt' "fun"
  | Object _ -> El.txt' "object"
  | Patch _ -> El.txt' "patch"
  | Call (_, _) -> El.txt' "call"
  | Import (_, _) -> El.txt' "import"
  | Def (_, _, _) -> El.txt' "def"
  | Decl_xmlns (_, _) -> El.txt' "declxmlns"
  | Alloc _ -> El.txt' "alloc"
  | Namespace (_, _) -> El.txt' "namespace"
  | Dx_sequent (_, _) -> El.txt' "sequent"
  | Dx_query (_, _, _) -> El.txt' "query"
  | Dx_prop (_, _) -> El.txt' "prop"
  | Dx_var _ -> El.txt' "var"
  | Dx_const_content _ -> El.txt' "const_content"
  | Dx_const_iri _ -> El.txt' "iri"
  | Comment _ -> El.txt' "comment"
  | Error _ ->
    El.div [El.txt' "todo"]

and render_code : Code.t -> El.t list = fun nodes ->
    List.map
      (Fun.compose render_code_node (fun Asai.Range.{ value; _ } -> value))
      nodes

let editor_el () =
  let action, text_field = text_field () in
  let rendered = El.code [] in
  let source_evt =
    E.filter_map
      (
        function
        | Source_changed src ->
          Some (Parse.parse ~source: (`File "") (Lexing.from_string @@ Jstr.to_string src))
      )
      action
  in
  let msg =
    E.map
      (
        function
        | Ok code -> render_code code
        | Error _ -> [El.txt' "error"]
      )
      source_evt
  in
  let () = Elr.set_children rendered ~on: msg in
  action,
  El.section
    [
      El.h1
        [El.txt (Jstr.v "Tree editor")];
      text_field;
      El.div [rendered];
    ]

let ui
    : editor: Editor.t -> (Editor.t signal * El.t list)
  = fun ~editor ->
    let def
        : Editor.t signal -> Editor.t signal * 'a
      = fun e ->
        let action, editor = editor_el () in
        let do_action = E.map do_action action in
        let editor' = S.accum (S.value e) do_action in
        editor', (editor', [editor])
    in
    S.fix editor def

let main () =
  let id = Jstr.v "app" in
  match Document.find_el_by_id G.document id with
  | None -> Console.(error [str "No element with id '%s' found"; id])
  | Some el ->
    let editor, children = ui ~editor: (load_state ()) in
    Logr.(hold @@ S.log editor save_state);
    El.set_children el children

let () = main ()
