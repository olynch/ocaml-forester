open Forester_core
open Forester_core.Reps
open Repr

let binding a : 'a binding ty = pair binding_strategy_t a

let _object (t : Code.t ty) : Code.t Code._object ty =
  let open Code in
  record "object" (fun self methods : _ _object -> { self; methods })
  |+ field "self" (option (list string)) (fun (s : _ _object) -> s.self)
  |+ field "methods" (list (pair string t)) (fun (s : _ _object) -> s.methods)
  |> sealr

let patch (t : Code.t ty) : Code.patch ty =
  let open Code in
  record "patch" (fun obj self methods : patch -> { obj; self; methods })
  |+ field "obj" t (fun s -> s.obj)
  |+ field "self" (option (list string)) (fun s -> s.self)
  |+ field "methods" (list (pair string t)) (fun s -> s.methods)
  |> sealr

let node_t (t : Code.t ty) : Code.node ty =
  let open Code in
  variant "node"
    (fun
      text
      verbatim
      group
      math
      ident
      xml_tag
      subtree
      _let
      _open
      scope
      put
      default
      get
      _fun
      _object
      patch
      call
      import
      def
      decl_xmlns
      alloc
      namespace
      -> function
        | Text s -> text s
        | Verbatim s -> verbatim s
        | Group (x, y) -> group (x, y)
        | Math (x, y) -> math (x, y)
        | Ident (x, y) -> ident (x, y)
        | Xml_tag (x, y, z) -> xml_tag (x, y, z)
        | Subtree (x, y) -> subtree (x, y)
        | Let (x, y, z) -> _let (x, y, z)
        | Open x -> _open x
        | Scope x -> scope x
        | Put (x, y) -> put (x, y)
        | Default (x, y) -> default (x, y)
        | Get x -> get x
        | Fun (x, y) -> _fun (x, y)
        | Object x -> _object x
        | Patch x -> patch x
        | Call (x, y) -> call (x, y)
        | Import (x, y) -> import (x, y)
        | Def (x, y, z) -> def (x, y, z)
        | Decl_xmlns (x, y) -> decl_xmlns (x, y)
        | Alloc x -> alloc x
        | Namespace (x, y) -> namespace (x, y))
  |~ case1 "Text" string (fun x -> Text x)
  |~ case1 "Verbatim" string (fun x -> Verbatim x)
  |~ case1 "Group" (pair delim_t t) (fun (x, y) -> Group (x, y))
  |~ case1 "Math" (pair math_mode_t t) (fun (x, y) -> Math (x, y))
  |~ case1 "Ident"
    (pair (list string) (list string))
    (fun (x, y) -> Ident (x, y))
  |~ case1 "Xml_tag"
    (triple
       (pair (option string) string)
       (list (pair (pair (option string) string) t))
       t)
    (fun (x, y, z) -> Xml_tag (x, y, z))
  |~ case1 "Subtree" (pair (option string) t) (fun (x, y) -> Subtree (x, y))
  |~ case1 "Let"
    (triple (list string) (list (binding (list string))) t)
    (fun (x, y, z) -> Let (x, y, z))
  |~ case1 "Open" (list string) (fun x -> Open x)
  |~ case1 "Scope" t (fun x -> Scope x)
  |~ case1 "Put" (pair (list string) t) (fun (x, y) -> Put (x, y))
  |~ case1 "Default" (pair (list string) t) (fun (x, y) -> Default (x, y))
  |~ case1 "Get" (list string) (fun x -> Get x)
  |~ case1 "Fun"
    (pair (list (binding (list string))) t)
    (fun (x, y) -> Fun (x, y))
  |~ case1 "Object" (_object t) (fun x -> Object x)
  |~ case1 "Patch" (patch t) (fun x -> Patch x)
  |~ case1 "Call" (pair t string) (fun (x, y) -> Call (x, y))
  |~ case1 "Import" (pair visibility_t string) (fun (x, y) -> Import (x, y))
  |~ case1 "Def"
    (triple (list string) (list (binding (list string))) t)
    (fun (x, y, z) -> Def (x, y, z))
  |~ case1 "Decl_xmlns" (pair string string) (fun (x, y) -> Decl_xmlns (x, y))
  |~ case1 "Alloc" (list string) (fun x -> Alloc x)
  |~ case1 "Namespace" (pair (list string) t) (fun (x, y) -> Namespace (x, y))
  |> sealv

let located_code_node (t : Code.t ty) : Code.node Range.located ty =
  let open Asai in
  let open Range in
  record "located_code_node" (fun loc value -> { loc; value })
  |+ field "loc" (option range) (fun t -> None)
  |+ field "value" (node_t t) (fun t -> t.value)
  |> sealr

let t : Code.t ty = mu (fun t -> list (located_code_node t))

let tree : Code.tree ty =
  let open Code in
  record "tree" (fun source_path addr code -> { source_path; addr; code })
  |+ field "source_path" (option string) (fun x -> x.source_path)
  |+ field "addr" (option string) (fun x -> x.addr)
  |+ field "code" t (fun x -> x.code)
  |> sealr
